# Difftime Modules
from difftime.time import(
    Interval,
    convert_24_hour,
    to_time,
    seconds_to_time,
    time_to_seconds
)

# Standard Library
import os
import argparse
import logging

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
        time_delta += time_to_seconds(intval.difftime())
        # (hh,mm,ss) = intval.difftime()

        # time_delta += hh * 3600
        # time_delta += mm * 60
        # time_delta += ss

    (hh,mm,ss) = seconds_to_time(time_delta)
    if with_seconds:
        print('%d hours %d minutes %d seconds' % (hh, mm, ss))
    else:
        print('%d hours %d minutes' % (hh, mm))
