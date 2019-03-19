# This program can be imported by an EV3 robot manager to communicate
# with ERGO over a TCP connection.  The two procedures to import are:
#   - ergo_tcp_session(portnum,initialize,handle_endogenous):
#        this sets up the server, waits for a TCP connection,
#        calls the given initialize(), and then calls the given 
#        handle_endogenous(actName,args) on each endogenous action received
#   - signal_exogenous(actName,args):
#        this sends an exogenous action back to ERGO over TCP
#        on a separate thread

import sys, socket, threading, queue

connection = None             # TCP connection
exo_acts = queue.Queue()      # queue of pending exo acts

def get_tcp_connection(portnum):
    global connection
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sys.stderr.write ('Waiting for TCP connection on port %s\n' % portnum)
    sock.bind(('0.0.0.0', portnum))
    sock.listen(1)
    connection, client_address = sock.accept()
    sys.stderr.write ('Connected from %s\n' % client_address[0])

def lose_tcp_connection():
    connection.close()
    sys.stderr.write ('Disconnected from TCP connection\n')
    sys.exit()

# convert str to flat s-expr
def str2fs(str):
    if str[0] == '(' and str[-1] == ')':
        return list(map(str2fs,str[1:-1].split(' ')))
    try: return int(str)
    except:
        try: return float(str)
        except: return str

# convert flat s-expr to str
def fs2str(s):
    if isinstance(s,int) or isinstance(s,float):
        return str(s)
    elif isinstance(s,list):
        return '('+' '.join(list(map(fs2str,s)))+')'
    else: return s
    
# add given act to exo_act queue
def signal_exogenous(actName,args):
    global exo_acts
    if args==[]: exo_acts.put(actName)
    else: exo_acts.put('('+actName+' '+fs2str(args)[1:])

# repeatedly get endogenous act from ERGO over TCP and call endofn on it
def process_endogenous(endofn):
    sys.stderr.write("Ready to receive endogenous actions\n") 
    for data in connection.makefile('r'):
        act = str2fs(data.rstrip())
        if isinstance(act,list): endofn(act[0],act[1:])
        else: endofn(act,[])   
            
# repeatedly pop exo_act queue and send result to ERGO over TCP
def monitor_exogenous():
    global exo_acts
    sys.stderr.write("Ready to send exogenous actions as they occur\n")
    while True:
        act = exo_acts.get(block=True)
        try: connection.sendall((act+'\n').encode())
        except: break

# manage ERGO interface over TCP: do given initfn, print to TCP any exo acts 
# (in a thread), and read from TCP and process any endo acts with given endofn
def ergo_tcp_session(portnum,initfn,endofn):
    get_tcp_connection(portnum)
    sys.stderr.write("Initializing ... ")
    initfn()
    sys.stderr.write("done\n") 
    threading.Thread(target=monitor_exogenous, daemon=True).start()
    try: process_endogenous(endofn)
    except: lose_tcp_connection()
