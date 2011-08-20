# -*- coding: utf-8 -*-

import sys


INPUT_CHARSET = 'gbk'
OUTPUT_CHARSET = 'gbk'


def writeFile (lines, file_index):
  file_name = '%04d.html' % (file_index,)
  with open(file_name, 'w') as fp:
    fp.write((u'<html>\r\n<head><title>%s</title></head>\r\n<body>\r\n' % (lines[0],)).encode(OUTPUT_CHARSET))
    fp.write((u'<h1>%s</h1>\r\n' % (lines[0],)).encode(OUTPUT_CHARSET))
    for line in lines[1:]:
      fp.write((u'<p>%s</p>\r\n' % (line,)).encode(OUTPUT_CHARSET))
    fp.write(u'</body>\r\n</html>\r\n')
  return file_name


def readFile (fp, title_file_mapping, next_file_index):
  new_file = False
  new_file_lines = None
  for line in fp:
    txt_line = line.strip()
    if len(txt_line) == 0:
      if new_file_lines and len(new_file_lines) > 0:
        file_name = writeFile(new_file_lines, next_file_index)
        title_file_mapping.append((new_file_lines[0], file_name,))
        next_file_index = next_file_index + 1
      new_file = True
      new_file_lines = []
    else:
      if new_file:
        new_file = False
      new_file_lines.append(line.decode(INPUT_CHARSET))
  if new_file_lines and len(new_file_lines) > 0:
    file_name = writeFile(new_file_lines, next_file_index)
    title_file_mapping.append((new_file_lines[0], file_name,))
    next_file_index = next_file_index + 1
  return next_file_index


def main ():
  title_file_mapping = []
  file_index = 1
  input_files = sys.argv[1:]
  for input_file in input_files:
    with open(input_file, 'r') as fp:
      file_index = readFile(fp, title_file_mapping, file_index)

  with open('toc.html','w') as fp:
    fp.write(u'<html>\r\n<head><title>Table of Contents</title></head>\r\n<body>\r\n'.encode(OUTPUT_CHARSET))
    fp.write(u'<h1>Table of Contents</h1>\r\n<ul>\r\n'.encode(OUTPUT_CHARSET))
    for title, file_name in title_file_mapping:
      fp.write((u'<li><a href="%s">%s</a></li>\r\n' % (file_name, title,)).encode(OUTPUT_CHARSET))
    fp.write(u'</ul>\r\n</body>\r\n</html>\r\n'.encode(OUTPUT_CHARSET))


if __name__ == '__main__':
  main()


# Local Variables: **
# comment-column: 56 **
# indent-tabs-mode: nil **
# python-indent: 2 **
# End: **
