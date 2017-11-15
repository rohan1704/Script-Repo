# -*- coding: utf-8 -*-
"""
Created on Wed Jun 28 11:10:25 2017

@author: ROHAN
"""

# Write Python code that assigns to the 
# variable url a string that is the value 
# of the first URL that appears in a link 
# tag in the string page.
# Your code should print http://udacity.com
# Make sure that if page were changed to

# page = '<a href="http://udacity.com">Hello world</a>'

# that your code still assigns the same value to the variable 'url', 
# and therefore still prints the same thing.

# page = contents of a web page
page =('<div id="top_bin"><div id="top_content" class="width960">'
'<div class="udacity float-left"><a href="http://udacity.com">')

start_link = page.find('<a href=')

q1 = page.find('"',start_link)

q2 = page.find('"',q1 + 1)

url = page[q1 + 1:q2]

print(url)