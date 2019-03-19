import time
import ev3dev.ev3 as ev3

# motors must be connected to out ports A and D
left_wheel = ev3.LargeMotor('outA')
right_wheel = ev3.LargeMotor('outD')

# drive both motors for ms milliseconds
def straight(ms):
        left_wheel.run_timed(time_sp=ms,speed_sp=500)
        right_wheel.run_timed(time_sp=ms,speed_sp=500)
        time.sleep(ms/1000.0)

# turn either left (dir=1) or right (dir=-1)
def turn(dir):
        left_wheel.run_timed(time_sp=360,speed_sp=-500*dir)
        right_wheel.run_timed(time_sp=360,speed_sp=500*dir)
        time.sleep(.5)

# drive a squarish pattern        
def square(ms):
        for i in range(0,4):
                straight(ms)
                turn(1)
        
square(1500)
