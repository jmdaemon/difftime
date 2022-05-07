import os
import re
import argparse
import logging

meridiem_regex = r'([ap]m)'
num_regex = r'([\d]+)'

def diff(upper, lower):
    ''' Return the absolute difference between two numbers '''
    return abs(upper - lower)

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
    # pattern = re.compile(meridiem_regex)
    # matches = pattern.match(time)
    # matchgroups = pattern.match(time).group(0)
    logging.info(f'Converting Time {time} to 24 Hour')

    matches     = re.search(meridiem_regex, time)
    matchgroups = matches.group()
    logging.info(f'Match Groups: {matchgroups}')

    result = ''
    # match matches.group(0):
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
                # (hh, mm) = time.split(':')
                # print(time.split(':'))
                (hh, mm) = time.split(':')
                mm = get_time(mm)
                hours = int(hh) + 12
                result = f'{hours}:{mm}'
    return result


def to_time(timestr: str, with_seconds: bool) -> Time:
    # Initial variables
    hh = 0
    mm = 0
    ss = 0

    # Optional seconds parameter
    if with_seconds:
        (hh, mm, ss) = timestr.split(':')
    else:
        (hh, mm) = timestr.split(':')

    # Convert to Time object
    hours = int(hh)
    minutes = int(mm)
    seconds = int(ss)

    time = Time(hours, minutes, seconds)
    return time

def seconds_to_time(secs: int) -> tuple:
    ''' Converts a duration of seconds into a tuple containing hours, minutes and seconds '''
    hh = (secs / 3600 % 24)
    mm = (secs / 60) % 60
    ss = (secs % 60)
    return (hh, mm, ss)

# Set up logging
loglevel = os.environ.get("LOGLEVEL")
loglevel = loglevel if loglevel is not None else logging.ERROR
logging.basicConfig(level=loglevel)

# Initialize command line interface
parser = argparse.ArgumentParser(description='Calculate differences in time intervals')
parser.add_argument('interval_list', help='The list of intervals: E.g \'9:30pm-\'10:30pm,\'10:30pm-11:30pm\'')
parser.add_argument('-s', default=False, required=False,
                    help='Use hh:mm:ss times instead of just hh:mm')

# Parse command line arguments
args = parser.parse_args()
interval_list = args.interval_list
with_seconds = args.s

# '10:30pm-11:30pm,9:30pm-10:00pm' -> ('10:30pm-11:30pm', '9:30pm-10:00pm')
intervals = interval_list.split(',')

# Holds the amount of time passed between intervals, in units of seconds
time_delta = 0

for interval in intervals:
    # ('10:30pm-11:30pm', '9:30pm-10:00pm') -> (('10:30pm', '11:30pm'), ('9:30pm', 10:00pm))
    beg, end = interval.split('-')

    # ('10:30pm', '11:30pm') -> ('22:30', '23:30')
    beg24h = convert_24_hour(beg, with_seconds)
    end24h = convert_24_hour(end, with_seconds)
    logging.info(f'beg24h: {beg24h}')
    logging.info(f'end24h: {end24h}')

    # ('22:30', '23:30') -> (Time(22, 30, 00), Time(23,30,00))
    begTime = to_time(beg24h, with_seconds)
    endTime = to_time(end24h, with_seconds)

    intval = Interval(begTime, endTime)
    (hh,mm,ss) = intval.difftime()
    time_delta += hh * 3600
    time_delta += mm * 60
    time_delta += ss

(hh,mm,ss) = seconds_to_time(time_delta)
if with_seconds:
    # print(f'{hh} hours {mm} minutes {ss} seconds')
    print('%d hours %d minutes %d seconds' % (hh, mm, ss))
else:
    print('%d hours %d minutes' % (hh, mm))
    # print(f'{hh} hours {mm} minutes')

# for time in times:
    # (beg, end) = time.split('-')
