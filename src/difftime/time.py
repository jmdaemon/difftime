import re
import logging

def diff(upper, lower):
    ''' Return the absolute difference between two numbers '''
    return abs(upper - lower)

# Regex constants
meridiem_regex = r'([ap]m)'
num_regex = r'([\d]+)'

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

def get_time(time: str) -> int:
    matches     = re.search(num_regex, time)
    matchgroups = matches.group()
    return int(matchgroups)

def convert_time(time: str, meridem: str, offset: int, with_seconds: bool):
    result = ''
    # if the time is already in am, return the time
    split = time.split(':')
    logging.info(f'Time Split: {split}')
    hh = int(split[0])
    mm = get_time(split[1])

    # 12:00pm == 12:00 in 24 hour time
    # 12:00am == 00:00 in 24 hour time
    if (hh == 12 and meridem == 'pm'):
        hh = 0

    # To convert twelve hour 
    hh += offset
    if with_seconds:
        ss = get_time(split[2])
        result = f'{hh}:{mm}:{ss}'
    else:
        result = f'{hh}:{mm}'
    return result

def convert_24_hour(time: str, with_seconds: bool):
    logging.info(f'Converting Time {time} to 24 Hour')

    matches     = re.search(meridiem_regex, time)
    # matchgroups = matches.group() if matches != None or matches != '' else ''
    matchgroups = ''
    if matches:
        matchgroups = matches.group()
    if matchgroups == None:
        matchgroups = ''
    logging.info(f'Match Groups: {matchgroups}')

    result = ''
    match matchgroups:
        case 'am': result = convert_time(time, 'am', 0, with_seconds)
        case 'pm': result = convert_time(time, 'pm', 12, with_seconds)
        case _: result = time
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
