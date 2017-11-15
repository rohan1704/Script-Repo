# -*- coding: utf-8 -*-
"""
Created on Wed Jun 28 13:00:41 2017

@author: ROHAN
"""

index = []


def add_to_index(index,keyword,url):
    for entry in index:
        if entry[0] == keyword:
            entry[1].append(url)
            return
    index.append([keyword,[url]])

def add_page_to_index(index,url,content):
    keyword = content.split()
    for e in keyword:
        add_to_index(index,e,url)



add_page_to_index(index,'fake.text',"This is a test")
print(index)
#>>> [['This', ['fake.text']], ['is', ['fake.text']], ['a', ['fake.text']],
#>>> ['test',['fake.text']]]
