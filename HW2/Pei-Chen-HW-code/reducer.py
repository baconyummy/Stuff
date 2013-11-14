#!/usr/bin/env python
from operator import itemgetter
import sys

current_word = None
current_count = 0
word = None

# input comes from STDIN
for line in sys.stdin:
	# remove leading and trailing whitespace
	line = line.strip()
	# parse the input we got from mapper.py
	word, count = line.split('\t', 1)
	# convert count (currently a string) to int
	try:
		count = int(count)
	except ValueError:
		# count was not a number, so silently
		# ignore/discard this line
		continue

	# this IF-switch only works because Hadoop sorts map output
	# by key (here: word) before it is passed to the reducer
	if current_word == word:
		current_count += count
	else:
		if current_word:
            # since current_word = x_y as from the my mapper
            # we need to split them by '_'
			x,y = current_word.split('_',1)
            # them add the upper bound for each x and y
			x_hi = str(float(x) +0.1)
			y_hi = str(float(y) +0.1)
            # combine the lower and upper bound and the count by ','
            # note: that %d is a number
			a = '%s,%s,%s,%s,%d' % (x,x_hi,y,y_hi, current_count)
			print a
		current_count = count
		current_word = word


# do not forget to output the last word if needed!
if current_word == word:
    # the same algorithm as the previous one
	x,y = current_word.split('_',1)
	x_hi = str(float(x) +0.1)
	y_hi = str(float(y) +0.1)
	a = '%s,%s,%s,%s,%d' % (x,x_hi,y,y_hi, current_count)
	print a

