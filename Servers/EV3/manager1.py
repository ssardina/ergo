#  This is a robot manager for an EV3 brick interacting with ERGO over TCP
#  It accepts the following endogenous actions:
#       - (run_motor! t), where t is in milliseconds
#       - req_sensor!
#  It generates the following exogenous actions:
#       - (reply_sensor! n), where n is between 1 and 100

import sys, ev3dev.ev3 as ev3
from ergo_communication import signal_exogenous, ergo_tcp_session

moto = ev3.LargeMotor('outA')    # need a motor on out port A
colo = ev3.ColorSensor()         # need a color sensor on an in port
colo.mode = 'COL-REFLECT'        # color sensor returns 1 to 100

# Run the motor on port A for t milliseconds
def run_motor(t):
    moto.run_timed(time_sp=t, speed_sp=500)

# Return a number from 1 to 100 in a reply_sensor! exogenous action
def req_sensor():
    signal_exogenous('reply_sensor!',[colo.value()])

########################################################################
###   The two procedures needed for ergo_tcp_session

def ini():
    ev3.Sound.speak('Starting Manager One').wait()
    print("Starting Manager 1")

def dispatch(actName,args):
    if actName=='run_motor!': run_motor(args[0])
    elif actName=='req_sensor!': req_sensor()
    else: print('No dice')

ergo_tcp_session(8123,ini,dispatch)
