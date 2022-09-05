# Difftime Modules
from difftime.time import(
    Interval,
    convert_24_hour,
    to_time,
    seconds_to_time,
    time_to_seconds,
    Time
)
from difftime import __version__

# Standard Library
import os
import argparse
import logging

def parse_time(time: str, with_seconds: bool) -> Time:
    time_24hr = convert_24_hour(time, with_seconds)
    result = to_time(time_24hr, with_seconds)
    return result

def main():
    # Set up logging
    loglevel = os.environ.get("LOGLEVEL")
    loglevel = loglevel if loglevel is not None else logging.ERROR
    logging.basicConfig(level=loglevel)

    # Initialize command line interface
    parser = argparse.ArgumentParser(description='Calculate differences in time intervals')
    parser.add_argument('interval_list', help='The list of intervals: E.g \'9:30pm-\'10:30pm,\'10:30pm-11:30pm\'')
    parser.add_argument('-s', default=False, required=False,
                        help='Use hh:mm:ss times instead of just hh:mm')
    parser.add_argument('-v', action='version',
                    version='%(prog)s v{version}'.format(version=__version__))

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
        begtime, endtime = interval.split('-')
        beg = parse_time(begtime, with_seconds)
        end = parse_time(endtime, with_seconds)

        intval = Interval(beg, end)
        time_delta += time_to_seconds(intval.difftime())

    (hh,mm,ss) = seconds_to_time(time_delta)
    if with_seconds:
        print('%d hours %d minutes %d seconds' % (hh, mm, ss))
    else:
        print('%d hours %d minutes' % (hh, mm))
