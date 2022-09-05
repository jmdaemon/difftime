import re

def diff(upper, lower):
    ''' Return the absolute difference between two numbers '''
    return abs(upper - lower)

# Regex constants
meridiem_regex = r'([ap]m)'

# Data Structures
class Time:
    def __init__(self, hh, mm, ss):
        self.hh = hh
        self.mm = mm
        self.ss = ss

class Interval:
    def __init__(self, beg: Time, end: Time):
        self.beg = beg
        self.end = end

    def difftime(self):
        hh = diff(self.end.hh, self.beg.hh)
        mm = diff(self.end.mm, self.beg.mm)
        ss = diff(self.end.ss, self.beg.ss)
        return (hh, mm, ss)

def split_time(time: str, meridem, with_seconds: bool):
    ''' Split the time input into a list '''
    split = time.split(meridem)[0].split(':')
    hh = int(split[0])
    mm = int(split[1])
    ss = 0 if not with_seconds else int(split[2])
    return (hh, mm, ss)

def format_time(hh: str, mm: str, ss: str, with_seconds: bool):
    ''' Format the list of times into a string '''
    # 07:0 -> 07:00
    if (int(mm) == 0):
        mm = f'0{mm}'

    result: str
    if with_seconds:
        if (ss == 0):
            ss = f'0{ss}'
        result = f'{hh}:{mm}:{ss}'
    else:
        result = f'{hh}:{mm}'
    return result

def to_24_hour(hours: int, meridem: str):
    ''' Converts hours from 12 hr to 24 hour time format '''
    hh = hours
    # Convert the time
    # 12:00am == 0:00
    if (hh == 12 and meridem == 'am'):
        hh -= 12
    # 11:59pm == 23:59
    elif (hh == 12 and meridem == 'pm'):
        pass
    elif (meridem == 'pm'):
        hh += 12
    return hh

def from_12_to_24_hour(time: str, meridem: str, with_seconds: bool):
    ''' Convert a 12 hour time to 24 hour time '''
    (hh, mm, ss) = split_time(time, meridem, with_seconds)
    hh = to_24_hour(hh, meridem)
    return format_time(str(hh), str(mm), str(ss), with_seconds)

def convert_24_hour(time: str, with_seconds: bool):
    matches = re.search(meridiem_regex, time)
    matchgroups = '' if matches == None else matches.group()

    result = ''
    match matchgroups:
        case 'am'   : result = from_12_to_24_hour(time, 'am', with_seconds)
        case 'pm'   : result = from_12_to_24_hour(time, 'pm', with_seconds)
        case _      : result = time
    return result

def to_time(timestr: str, with_seconds: bool) -> Time:
    ''' Convert a time string to a Time object '''
    (hh,mm,ss) = (0, 0, 0)

    # Optional seconds parameter
    if with_seconds:
        (hh, mm, ss) = timestr.split(':')
    else:
        (hh, mm) = timestr.split(':')

    # Convert to Time object
    time = Time(int(hh), int(mm), int(ss))
    return time

def seconds_to_time(secs: int) -> tuple:
    ''' Converts a duration of seconds into a tuple containing hours, minutes and seconds '''
    hh = (secs / 3600 % 24)
    mm = (secs / 60) % 60
    ss = (secs % 60)
    return (hh, mm, ss)

def time_to_seconds(time: tuple) -> int:
    ''' Convert a time into a duration of seconds '''
    (hh,mm,ss) = time
    time_delta = 0
    time_delta += hh * 3600
    time_delta += mm * 60
    time_delta += ss
    return time_delta
