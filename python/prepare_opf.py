# -*- coding: utf-8 -*-

import os
import re
import sys


PAGE_NAVI_DEFS = {
  'emacs': ('''<ul class="menu">''', '</ul>'),
  'sicp': ('''<div class="navigation">''', '</div>')
}

# None or key in PAGE_NAVI_DEFS
PAGE_NAVI = 'sicp'

RE_ANCHOR = re.compile('''<[aA]([^>]+)>([^<]+)</[ \t]*[aA]>''')
RE_ATTRS = re.compile('''([-_a-zA-Z0-9]+)[ \t]*=[ \t]*['"]([^'"]+)['"]''')


def parse_links (content):
  global RE_ANCHOR, RE_ATTRS
  links = []
  for link in RE_ANCHOR.finditer(content):
    attrs = link.group(1)
    desc = link.group(2)
    href = None
    for attr in RE_ATTRS.finditer(attrs):
      if attr.group(1).lower() == 'href':
        href = attr.group(2)
        break
    if href:
      if href.startswith('http://') or \
            href.startswith('https://') or \
            href.startswith('/'):
        continue
      else:
        links.append((href, desc, [],))
  return links


def read_file (file):
  fo = open(file)
  try:
    return ' '.join([line.strip() for line in fo.readlines()])
  finally:
    fo.close()


def parse_page_links (file):
  global PAGE_NAVI_DEFS, PAGE_NAVI

  content = read_file(file)
  if not PAGE_NAVI in PAGE_NAVI_DEFS:
    return parse_links(content)
  start, end = PAGE_NAVI_DEFS[PAGE_NAVI]

  sPos = 0
  if start:
    sPos = content.find(start)
  if sPos > -1:
    ePos = -1
    if end:
      ePos = content.find(end, sPos)
    if ePos == -1:
      return parse_links(content[sPos:])
    else:
      return parse_links(content[sPos:ePos])
  else:
    return []


def parse_toc_links (file):
  content = read_file(file)
  return parse_links(content)


def baselink (link):
  pos = link.find('#')
  if pos > -1:
    return link[:pos]
  else:
    return link


def walk (root_link, visited_links, link_seq):
  base_root_link = baselink(root_link[0])
  links = parse_page_links(base_root_link)
  root_link[2].extend(links)
  for link in links:
    base_link = baselink(link[0])
    if not base_link in visited_links:
      visited_links.add(base_link)
      link_seq.append(base_link)
      walk(link, visited_links, link_seq)


OPF_PART1 = '''
<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://www.idpf.org/2007/opf" version="2.0" unique-identifier="BookId">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:title>Kindle User's Guide</dc:title>
    <dc:language>en-us</dc:language>
    <meta name="cover" content="My_Cover"/>
    <dc:identifier id="BookId" opf:scheme="ISBN">9781375890815</dc:identifier>
    <dc:creator>Amazon.com</dc:creator>
    <dc:publisher>Amazon.com</dc:publisher>
    <dc:subject>Reference</dc:subject>
    <dc:date>2009-11-17</dc:date>
    <dc:description>An overview of all the Amazon Kindle features and how to use them. 3rd Edition.</dc:description>
  </metadata>
  <manifest>
'''
OPF_PART2 = '''
    <item id="My_Table_of_Contents" media-type="application/x-dtbncx+xml" href="%s.ncx"/>
    <item id="My_Cover" media-type="image/gif" href="cover.gif"/>
  </manifest>
'''
OPF_PART3 = '''
  <spine toc="My_Table_of_Contents">
'''
OPF_PART4 = '''
  </spine>
  <guide>
    <reference type="toc" title="Table of Contents" href="toc.html"></reference>
    <reference type="text" title="Welcome" href="Welcome.html"></reference>
  </guide>
</package>
'''

OPF_PART_ITEM = '''    <item id="item%s" media-type="%s" href="%s"></item>
'''
OPF_PART_ITEMREF = '''    <itemref idref="item%s"/>
'''

# MIME = {
#   '.html': 'application/xhtml+xml',
#   '.png' : 'image/png',
#   '.jpg' : 'image/jpeg',
#   '.gif' : 'image/gif'
# }

# def mime (link):
#   global MIME
#   pos = link.rfind('.')
#   if pos > -1 and link[pos:] in MIME:
#     return MIME[link[pos:]]
#   else:
#     return 'application/xhtml+xml',


def makeopf (link_seq, name):
  global OPF_PART1, OPF_PART2, OPF_PART3, OPF_PART4
  global OPF_PART_ITEM, OPF_PART_ITEMREF
  fo = open(name+'.opf','w')
  fo.write(OPF_PART1)
  start = 11
  c = start
  for link in link_seq:
    fo.write(OPF_PART_ITEM % (c, 'application/xhtml+xml', link,))
    c += 1
  fo.write(OPF_PART2 % (name,))
  fo.write(OPF_PART3)
  for i in xrange(start, c):
    fo.write(OPF_PART_ITEMREF % (i,))
  fo.write(OPF_PART4)
  fo.close()


def main ():
  argv = sys.argv
  if len(argv) == 1:
    argv = ['index.html', 'Default']
  elif len(argv) == 2:
    argv = [argv[1], 'Default']
  else:
    argv = [argv[1], argv[2]]

  print 'Parsing', argv[0], 'to generate', argv[1]+'.opf'

  oldcwd = os.getcwd()
  input_parts = argv[0].split(os.sep)
  if len(input_parts) > 1:
    os.chdir(os.sep.join(input_parts[:-1]))
    index_link = input_parts[-1]
  else:
    index_link = input_parts[0]

  visited_links = set()
  link_seq = []
  links = parse_toc_links(index_link)
  for link in links:
    base_link = baselink(link[0])
    if not base_link in visited_links:
      visited_links.add(base_link)
      link_seq.append(base_link)
      walk(link, visited_links, link_seq)

  makeopf(link_seq, argv[1])



if __name__ == '__main__':
  main()


# Local Variables: **
# comment-column: 56 **
# indent-tabs-mode: nil **
# python-indent: 2 **
# End: **
