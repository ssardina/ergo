import time, ev3dev.ev3 as ev3

# motors must be connected to out ports A and D
left_wheel = ev3.LargeMotor('outA')
right_wheel = ev3.LargeMotor('outD')

# the color sensor must be connected to an in port
color_sensor = ev3.ColorSensor()
color_sensor.mode = 'COL-REFLECT'    # report a value from 1 to 100    

# drive both motors at speed sp until told otherwise
def straight_forever(sp):
        left_wheel.run_forever(speed_sp=sp)
        right_wheel.run_forever(speed_sp=sp)

# drive forward while color sensor < t
def threshold(t):
        straight_forever(200)
        while (color_sensor.value() < t): time.sleep(.1)
        straight_forever(0)
        
threshold(50)
                             
