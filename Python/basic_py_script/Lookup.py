# -*- coding: utf-8 -*-
"""
Created on Wed Jun 28 11:43:41 2017

@author: ROHAN
"""

index = [['udacity', ['http://udacity.com', 'http://npr.org']],
         ['computing', ['http://acm.org']]]

def lookup(index,keyword):
    l = len(index) - 1
    i = 0
    while i <= l:
        if keyword in index[i]:
            return index[i][1]
            break
        else:
            i = i + 1
    return []


def alt_lookup(index,keyword):
    for e in index:
        if e[0] == keyword:
            return e[1]
    return []




print(lookup(index,'udacity'))
print(alt_lookup(index,'udacity'))
#>>> ['http://udacity.com','http://npr.org']
print(lookup(index,'computing'))
print(alt_lookup(index,'computing'))
#>>> ['http://acm.org']