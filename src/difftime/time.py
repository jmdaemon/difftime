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

def convert_24_hour(time: str, with_seconds: bool):
    logging.info(f'Converting Time {time} to 24 Hour')

    matches     = re.search(meridiem_regex, time)
    matchgroups = matches.group()
    logging.info(f'Match Groups: {matchgroups}')

    result = ''
    match matchgroups:
        case 'am':
            # if the time is already in am, return the time
            return time
        case 'pm':
            # if the time is in pm, convert the time
            logging.info(f'Time Split: {time.split(":")}')
            if with_seconds:
                (hh, mm, ss) = time.split(':')
                mm = get_time(mm)
                ss = get_time(ss)
                hours = int(hh) + 12
                result = f'{hours}:{mm}:{ss}'
            else:
                (hh, mm) = time.split(':')
                mm = get_time(mm)
                hours = int(hh) + 12
                result = f'{hours}:{mm}'
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
