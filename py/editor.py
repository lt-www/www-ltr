#!/usr/bin/env python

import cgi
import cgitb
import datetime
import io
import os.path
import subprocess

cgitb.enable()

def rem_cr (s) :
    return s.replace("\r","")

print("Content-type: text/html; charset=utf-8")
print("")

h_begin = """
<html>
<head>
<title>text editor</title>
</head>
<body>
"""

def h_editor (fn,txt) :
    return ("""
    <p>q_fn=%(fn)s</p>
    <form accept-charset="utf-8" method="post" enctype="multipart/form-data" action="index.cgi">
    <textarea name="f_text" rows="30" cols="80">%(txt)s</textarea>
    <br />
    <input type="hidden" name="f_filename" value="%(fn)s" />
    <input type="submit" name="f_edit" value="f_edit" />
    </form>
    """ % {'fn': fn, 'txt': txt})

def h_result (fn,txt) :
    return ("""
    <p>fn=%(fn)s; file stored</p>
    <pre>%(txt)s</pre>
    """ % {'fn': fn, 'txt': txt})

h_end = """
    </body>
    </html>
    """

# q = query, f = form

fs = cgi.FieldStorage()
q_fn = fs.getvalue("t", "")

if q_fn != "" and os.path.isfile(q_fn) :

    q_fp = io.open(q_fn, mode="r", encoding="utf-8")
    file_txt = q_fp.read()
    q_fp.close()
    print(h_begin)
    print(h_editor(q_fn, file_txt))
    print(h_end)

else:

    f_fn = fs.getvalue("f_filename", "")
    result_txt = fs.getvalue("f_text", "").decode("utf-8")
    f_fp = io.open(f_fn, mode="w", encoding="utf-8")
    f_fp.write(rem_cr(result_txt))
    f_fp.close()
    print(h_begin)
    print(h_result(f_fn, result_txt))
    print(h_end)
#    commit_msg = datetime.datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%S')
#    subprocess.call(["git", "commit", f_fn, "-m", commit_msg, "--quiet"])
