import ev3dev.ev3 as ev3

# motors must be connected to out ports A and D
left_wheel = ev3.LargeMotor('outA')
right_wheel = ev3.LargeMotor('outD')

left_wheel.run_forever(speed_sp=0)
right_wheel.run_forever(speed_sp=0)


                             
