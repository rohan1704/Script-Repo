# By Websten from forums
#
# Given your birthday and the current date, calculate your age in days. 
# Account for leap days. 
#
# Assume that the birthday and current date are correct dates (and no 
# time travel). 
#
nday = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
lday = [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

def leap(n):
    if n % 400 == 0:
        return True
    if n % 100 == 0:
        return False
    if n % 4 == 0:
        return True
    else:
        return False

def first(year1,month1,day1):
    mon = 1
    day = 0
    if leap(year1) == True:
        while True:
            if mon >= month1:
                break
            day = day + lday[mon-1]
            mon = mon + 1
        day = day + (day1 - 1)
        return 366 - day
    else:
        while True:
            if mon >= month1:
                break
            day = day + nday[mon-1]
            mon = mon + 1
        day = day + (day1 - 1)
        return 365 - day

def last(year2,month2,day2):
    mon = 1
    day = 0
    if leap(year2) == True:
        while True:
            if mon >= month2:
                break
            day = day + lday[mon-1]
            mon = mon + 1
        day = day + (day2 - 1)
        return day
    else:
        while True:
            if mon >= month2:
                break
            day = day + nday[mon-1]
            mon = mon + 1
        day = day + (day2 - 1)
        return day
    
def equal(year1, month1, day1, year2, month2, day2):
    mon = 1
    day = 0
    if leap(year1) == True:
        while True:
            if mon >= month2:
                break
            day = day + lday[mon-1]
            mon = mon + 1
        day = day + (day2 - 1)
        return day
    else:
        while True:
            if mon >= month2:
                break
            day = day + nday[mon-1]
            mon = mon + 1
        day = day + (day2 - 1)
        return day
    
def daysBetweenDates(year1, month1, day1, year2, month2, day2):
    ##
    # Your code here.
    ##
    if year2 < year1 :
        return "AssertionError"
    if year2 == year1:
        if month2 < month1:
            return "AssertionError"
        if month2 == month1:
            if day2 < day1:
                return "AssertionError"
				
	i = year1 + 1
	if year1 == year2:
		return equal(year1, month1, day1, year2, month2, day2)
		else:
			liv = first(year1,month1,day1)
        while i < year2:
            if leap(i) == True:
                liv = liv + 366
            else:
                liv = liv + 365
            i = i + 1
		liv = liv + last(year2,month2,day2)
    return liv
    
    
# Test routine
def test():
    test_cases = [((2012,1,1,2012,2,28), 58), 
                  ((2012,1,1,2012,3,1), 60),
                  ((2011,6,30,2012,6,30), 366),
                  ((2011,1,1,2012,8,8), 585 ),
                  ((1900,1,1,1999,12,31), 36523)]
    for (args, answer) in test_cases:
        result = daysBetweenDates(*args)
        if result != answer:
            print("Test with data:", args, "failed")
        else:
            print("Test case passed!")

test()


