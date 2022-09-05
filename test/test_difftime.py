from difftime.time import (
    convert_24_hour,
    to_time,
)

# convert_24_hour tests

# 24 Hour
def test_24hour_am():
    expect = "7:00"
    actual = convert_24_hour(expect, False)
    assert(actual == expect)

def test_24hour_pm():
    expect = "21:00"
    actual = convert_24_hour(expect, False)
    assert(actual == expect)

def test_24hour_noon():
    expect = "12:00"
    actual = convert_24_hour(expect, False)
    assert(actual == expect)

def test_24hour_midnight():
    expect = "00:00"
    actual = convert_24_hour(expect, False)
    assert(actual == expect)

# 12 Hour
def test_12hour_am():
    time = "7:00am"
    expect = "7:00"
    actual = convert_24_hour(time, False)
    assert(actual == expect)

def test_12hour_pm():
    time = "9:00pm"
    expect = "21:00"
    actual = convert_24_hour(time, False)
    assert(actual == expect)

def test_12hour_noon():
    time = "12:00pm"
    expect = "12:00"
    actual = convert_24_hour(time, False)
    assert(actual == expect)

def test_12hour_midnight():
    time = "12:00am"
    expect = "0:00"
    actual = convert_24_hour(time, False)
    assert(actual == expect)
