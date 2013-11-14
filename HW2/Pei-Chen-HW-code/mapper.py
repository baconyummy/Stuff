#!/usr/bin/python
import sys
import math

# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    # split the line into words (x and y) each of them separates by tab
    word_x, word_y= line.split('\t', 1)
    # convert the string to float to do the calculation
    x_f = float(word_x)
    # get the floor number to the first decimal for each of row
    x_fl = math.floor(x_f*10)/10
    y_f = float(word_y)
    y_fl = math.floor(y_f*10)/10
    # and convert back to strings
    # seperate x_fl and y_fl by '_' for easier implementation in reduce part
    a = str(x_fl) + '_' + str(y_fl)
    # combine 2 strings
    map = '%s\t%s' % (a, 1)
    # print out the result
    print map
