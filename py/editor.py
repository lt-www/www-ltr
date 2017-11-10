#!/usr/bin/env python

import cgi
import cgitb
import io
import os.path

cgitb.enable()

print "Content-type: text/html; charset=utf-8"
print

print """
<html>
<head>
<title>text editor</title>
</head>
<body>
"""

# q = query, f = form

fs = cgi.FieldStorage()
q_fn = fs.getvalue("t","")

if q_fn != "" and os.path.isfile(q_fn) :

    q_fp = io.open(q_fn, mode="r", encoding="utf-8")
    file_txt = q_fp.read()

    print("""
  <p>q_fn=%(q_fn)s</p>
  <form accept-charset="utf-8" method="post" enctype="multipart/form-data" action="index.cgi">
  <textarea name="f_text" rows="30" cols="80">%(file_txt)s</textarea>
  <br />
  <input type="hidden" name="f_filename" value="%(q_fn)s" />
  <input type="submit" name="f_edit" value="f_edit" />
  </form>
</body>
</html>
    """ % {'q_fn': q_fn, 'file_txt': file_txt})

else:

    f_fn = fs.getvalue("f_filename","")
    result_txt = fs.getvalue("f_text", "").decode("utf-8")
    f_fp = io.open(f_fn, mode="w", encoding="utf-8")
    f_fp.write(result_txt)
    f_fp.close()


    print("""
  <p>f_fn=%(f_fn)s; write ednded"</p>
  <pre>%(result_txt)s</pre>
</body>
</html>
    """ % {'f_fn': f_fn, 'result_txt': result_txt})
