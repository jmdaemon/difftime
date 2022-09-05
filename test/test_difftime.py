from difftime.time import convert_24_hour

# Helper functions
def run_test_24hr(expect):
    actual = convert_24_hour(expect, False)
    assert(actual == expect)

def run_test_12hr(time, expect):
    actual = convert_24_hour(time, False)
    assert(actual == expect)

# convert_24_hour tests

# 24 Hour
def test_24hour_am():
    run_test_24hr("7:00")

def test_24hour_pm():
    run_test_24hr("21:00")

def test_24hour_noon():
    run_test_24hr("12:00")

def test_24hour_midnight():
    run_test_24hr("00:00")

# 12 Hour
def test_12hour_am():
    run_test_12hr("7:00am", "7:00")

def test_12hour_pm():
    run_test_12hr("9:00pm", "21:00")

def test_12hour_noon():
    run_test_12hr("12:00pm", "12:00")

def test_12hour_midnight():
    run_test_12hr("12:00am", "0:00")
