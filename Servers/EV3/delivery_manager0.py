import sys, time, threading #, ev3dev.ev3 as ev3 (uncomment when needed)
from ergo_communication import signal_exogenous, ergo_tcp_session

def do_leave_location():
    print('*** heading straight')
    threading.Thread(target=going_to_location, daemon=True).start()

def going_to_location():
    for i in range(4):
        print('      moving forward ...')
        time.sleep(2.0)
    signal_exogenous('arrive-at-location!',[])

def do_turn(dir):
    if dir=='left': print('*** turning left')
    elif dir=='right': print('*** turning right')
    else: print('*** turning around')
    time.sleep(3.0)

def do_req_customer_action():
    print('*** request customer_action')
    threading.Thread(target=getting_customer_action, daemon=True).start()

def getting_customer_action():    
    input('Type ENTER when customer_action is complete: ')
    signal_exogenous('customer-action-done!',[])

########################################################################
###   The two procedures needed for ergo_tcp_session

def initialize():
    print('Starting Delivery Manager')

def do_endogenous(actName,args):
    if actName=='leave-location!': do_leave_location()
    elif actName=='turn!': do_turn(args[0])
    elif actName=='req-customer-action!': do_req_customer_action()
    else: print('No action')

ergo_tcp_session(8123,initialize,do_endogenous)
