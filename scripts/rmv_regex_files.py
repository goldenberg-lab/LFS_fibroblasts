"""
This script will delete all files matching the given regex in all subdirectories from the working directory for the
script. The print flag will dictate that it will print all file names that would be deleted if the print flag is not
there.
"""

import os
import glob
import argparse
import sys
import re

parser = argparse.ArgumentParser()
parser.add_argument("--regex", help="regex to match files with", required=True)
parser.add_argument("--print", help="only print which files would be deleted leaving all files unchanged",
                    action='store_true')
args = parser.parse_args()

paths = [f for f in glob.iglob(os.getcwd() + '/**', recursive=True) if re.match(args.regex, f) and os.path.isfile(f)]

if args.print:
    print(paths)
else:
    for p in paths:
        os.remove(p)
        try:
            os.rmdir(os.path.dirname(p))
        except [FileNotFoundError, PermissionError]:
            continue
