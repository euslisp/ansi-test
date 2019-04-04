#!/usr/bin/env python

import re
from os import walk


fh = open("REPORT.org","w")

f = []

# Gets dirnames
root = '../tests/'
(_, dirnames, _) = walk(root).next()

# for name in dirnames:
    # fh.write("*** " + name + '\n')

for directory in dirnames:
    (_, _, filenames) = walk(root + '/' + directory).next()
    fh.write("*** " + directory + ' [%]' + '\n')
    for file in filenames:
        fh.write("**** " + file + ' [%]' + '\n')

        fi = open(root + '/' + directory + '/' + file, "r")
        for line in fi:
            if re.match('^\([^ ]*\-test', line) or re.match('^\(deftest', line):
                fh.write("  - [ ] " + line)
