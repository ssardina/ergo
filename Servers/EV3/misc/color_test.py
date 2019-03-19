import ev3dev.ev3 as ev3

# color sensor must be connected to an in port
sense1 = ev3.ColorSensor()
sense1.mode = 'COL-REFLECT'    # report a value from 1 to 100    
print('The color sensor report %s' % sense1.value())
                             
