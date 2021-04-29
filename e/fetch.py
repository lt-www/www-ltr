#!/usr/bin/python3

import subprocess

print("Content-type: text/html\n\n")

print("<p>Fetching from github.com/lt-www...</p>\n\n")

subprocess.call(["git","--git-dir","../.git","--work-tree","..","pull", "https://github.com/lt-www/www-ltr","--quiet"])

print("<p>Fetch complete</p>\n\n")
