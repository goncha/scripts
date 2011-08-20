# -*- coding: utf-8 -*-

from lxml import etree
from subprocess import call

import os
import re
import shutil
import sys


def parseHTML (fp, encoding='utf-8'):
  parser = etree.HTMLParser(encoding=encoding)
  return etree.parse(fp, parser)


def generateHTMLNode():
  html = etree.Element('html')
  head = etree.SubElement(html, 'head')
  etree.SubElement(head, 'meta', attrib={
      'http-equiv': 'Content-Type',
      'content': 'text/html;charset=UTF-8'})
  return html


def cleanHTML (file):
  file_html = None
  with open(file, 'r') as fp:
    file_html = parseHTML(fp)
  doc = file_html.xpath('/html/body/center/table/td')[0]
  clean_nodes = doc.getchildren()[3:-3]
  html = generateHTMLNode()
  body = etree.SubElement(html, 'body')
  body.extend(clean_nodes)
  with open('tmp' + os.sep + file, 'w') as fp:
    fp.write(etree.tostring(html,
                            pretty_print=True,
                            encoding='utf-8',
                            xml_declaration=False))


def cleanHTMLs (sections):
  for sec in sections:
    for art in sec[1]:
      cleanHTML(art[1])


def generateWelcome (coverfile):
  html = generateHTMLNode()
  body = etree.SubElement(html, 'body')
  img = etree.SubElement(body, 'img')
  img.attrib['src'] = coverfile
  with open('tmp' + os.sep + 'welcome.html', 'w') as fp:
    fp.write(etree.tostring(html,
                            pretty_print=True,
                            encoding='utf-8',
                            xml_declaration=False))

def generateTOC (sections):
  html = generateHTMLNode()
  body = etree.SubElement(html, 'body')
  etree.SubElement(body,'h2').text = 'Table of Contents'

  for sec in sections:
    etree.SubElement(body, 'h4').text = sec[0]
    for art in sec[1]:
      etree.SubElement(body, 'a', attrib={'href':art[1]}).text = art[0]
      etree.SubElement(body, 'br')

  with open('tmp' + os.sep + 'toc.html', 'w') as fp:
    fp.write(etree.tostring(html,
                            pretty_print=True,
                            encoding='utf-8',
                            xml_declaration=False))


def generateNavPoint (nav_point_node, label, source):
  content = etree.Element('content', attrib={'src' : source})
  text_element = etree.Element('text')
  text_element.text = label
  nav_label = etree.SubElement(nav_point_node, "navLabel")
  nav_label.append(text_element)
  nav_point_node.append(content)


def generateNCX (id, title, sections):
  mbp_namespace = 'http://mobipocket.com/ns/mbp'

  ncx_namespace = 'http://www.daisy.org/z3986/2005/ncx/'
  ncx_nsmap = { None: ncx_namespace, 'mbp': mbp_namespace }

  ncx = etree.Element('ncx',
                      nsmap=ncx_nsmap,
                      attrib={'version' : '2005-1',
                              '{http://www.w3.org/XML/1998/namespace}lang' : 'en-US'})

  head = etree.SubElement(ncx, 'head')
  etree.SubElement(head,'meta',
                   attrib={'name' : 'dtb:uid',
                           'content' : id })
  etree.SubElement(head,'meta',
                   attrib={'name' : 'dtb:depth',
                           'content' : '2' })
  etree.SubElement(head,'meta',
                   attrib={'name' : 'dtb:totalPageCount',
                           'content' : '0' })
  etree.SubElement(head,'meta',
                 attrib={'name' : 'dtb:maxPageNumber',
                         'content' : '0' })

  title_text_element = etree.Element('text')
  title_text_element.text = title
  author_text_element = etree.Element("text")
  author_text_element.text = title

  etree.SubElement(ncx,'docTitle').append(title_text_element)
  etree.SubElement(ncx,'docAuthor').append(author_text_element)

  nav_map = etree.SubElement(ncx,'navMap')

  nav_point_periodical = etree.SubElement(nav_map, 'navPoint',
                                          attrib={'class': 'periodical', 'id': 'periodical', 'playOrder': '0'})
  generateNavPoint(nav_point_periodical, 'Table of Contents', 'toc.html')

  i = 2
  section = 0
  for sec in sections:
    section = section+1
    nav_point_section = etree.SubElement(nav_point_periodical,"navPoint",
                                         attrib={"class" : "section",
                                                 "id" : ('sec_' + str(section)),
                                                 "playOrder" : str(i) })
    generateNavPoint(nav_point_section, sec[0], sec[1][0][1])
    i = i+1

    article = 0
    for art in sec[1]:
      article = article + 1
      nav_point_article=  etree.SubElement(nav_point_section,"navPoint",
                                           attrib={"class" : "article",
                                                   "id" : ('art_' + str(section) + '_' + str(article)),
                                                   "playOrder" : str(i) })
      generateNavPoint(nav_point_article, art[0], art[1])
      i += 1

  with open('tmp' + os.sep + 'nav.ncx',"w") as fp:
    fp.write('<?xml version="1.0" encoding="utf-8"?>\n')
    fp.write('<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">\n')
    fp.write(etree.tostring(ncx,
                            pretty_print=True,
                            encoding="utf-8",
                            xml_declaration=False))


MIME = {
  'html' : 'application/xhtml+xml',
  'jpg' : 'image/jpeg',
  'gif' : 'image/gif',
  'png' : 'image/png',
  'ncx' : 'application/x-dtbncx+xml'
}
def media_type (ext):
  global MIME
  return MIME[ext]

def generateOPF (id, title, date, cover, pages, resources):
  opf_namespace = 'http://www.idpf.org/2007/opf'
  dc_namespace = 'http://purl.org/dc/elements/1.1/'
  dc_metadata_nsmap = { 'dc' : dc_namespace }
  dc = '{{{0}}}'.format(dc_namespace)

  package = etree.Element('{{{0}}}package'.format(opf_namespace),
                          nsmap={None:opf_namespace},
                          attrib={'version':'2.0',
                                  'unique-identifier':id})
  metadata = etree.Element('metadata')
  package.append(metadata)

  etree.SubElement(metadata,'meta',attrib={'name':'cover',
                                              'content':cover[:-4]})
  dc_metadata = etree.Element('dc-metadata',
                              nsmap=dc_metadata_nsmap)
  metadata.append(dc_metadata)

  etree.SubElement(dc_metadata,dc+'title').text = title
  etree.SubElement(dc_metadata,dc+'language').text = 'en-us'
  etree.SubElement(dc_metadata,dc+'creator').text = title
  etree.SubElement(dc_metadata,dc+'publisher').text = title
  etree.SubElement(dc_metadata,dc+'subject').text = "News"
  etree.SubElement(dc_metadata,dc+'date').text = date
  etree.SubElement(dc_metadata,dc+'description').text = '{0} on {1}'.format(title, date)

  x_metadata = etree.Element('x-metadata')
  metadata.append( x_metadata )
  etree.SubElement(x_metadata,'output',attrib={'encoding':'utf-8',
                                               'content-type':'application/x-mobipocket-subscription-magazine'})

  manifest = etree.SubElement(package,'manifest')

  files = []
  files.extend(pages)
  files.extend(resources)
  for f in files:
    item_id = re.sub('\..*$','',f)
    extension = re.sub('^.*\.','',f)
    etree.SubElement(manifest,'item',
                     attrib={'id' : item_id,
                             'media-type' : media_type(extension),
                             'href' : f})

  spine = etree.SubElement(package, 'spine',
                           attrib={'toc' : 'nav'})
  for p in pages:
    item_id = re.sub('\..*$', '', p)
    etree.SubElement(spine, 'itemref',
                     attrib = {'idref' : item_id})


  guide = etree.SubElement(package,'guide')
  etree.SubElement(guide,'reference',
                   attrib={'type':'toc',
                           'title':'Table of Contents',
                           'href':'toc.html'})
  etree.SubElement(guide,'reference',
                   attrib={'type':'text',
                           'title':'Welcome',
                           'href':'welcome.html'})

  with open('tmp' + os.sep + 'default.opf', 'w') as fp:
    fp.write(etree.tostring(package,
                            pretty_print=True,
                            encoding='utf-8',
                            xml_declaration=True))


def main (argv):
  if len(argv) < 2:
    return

  basedir = argv[1]
  os.chdir(basedir)
  try:
    os.mkdir('tmp')
  except:
    pass

  indexfile = 'index.html'
  index_html = None
  with open(indexfile, 'r') as fp:
    index_html = parseHTML(fp, encoding='gbk')

  doc_title = index_html.xpath('/html/body/center/table/font/b/text()')[0].strip()
  doc_date_long = index_html.xpath('/html/body/center/table/text()')[0].strip()
  doc_cover = index_html.xpath('/html/body/center/div/img')[0].attrib['src']
  doc_date = doc_cover[0:4] + '-' + doc_cover[4:6] + '-' + doc_cover[6:8]
  doc_id = (doc_title + ' ' + doc_date).replace(' ','_')
  doc_sections = []
  doc_title_long = doc_title + ' ' + doc_date_long

  for ch in index_html.xpath('/html/body/center/table/td/table'):
    ch_title = ch.xpath('td/font/a/text()')[0].strip()
    ch_articles = []
    art = ch.getnext()
    while art.tag == 'p':
      art_title = art.xpath('a/text()')[0].strip()
      art_file = art.xpath('a')[0].attrib['href']
      ch_articles.append([art_title, art_file,])
      art = art.getnext()
    doc_sections.append((ch_title, ch_articles))

  cleanHTMLs(doc_sections)

  generateWelcome(doc_cover)
  generateTOC(doc_sections)

  doc_pages = ['toc.html', 'welcome.html']
  doc_resources = ['nav.ncx']
  for sec in doc_sections:
    for art in sec[1]:
      doc_pages.append(art[1])
  doc_resources.extend([f for f in os.listdir('.') \
                          if f.endswith('.gif') or f.endswith('.jpg')])
  for f in doc_resources[1:]:
    shutil.copyfile(f, 'tmp'+os.sep+f)
  generateOPF(doc_id, doc_title, doc_date, doc_cover, doc_pages, doc_resources)
  generateNCX(doc_id, doc_title, doc_sections)

  os.chdir('tmp')
  call(['kindlegen', '-c2', '-o', 'The_Economist_' + doc_cover[:-4] + '.mobi', 'default.opf'])



if __name__ == '__main__':
  main(sys.argv)

# Local Variables: **
# comment-column: 56 **
# indent-tabs-mode: nil **
# python-indent: 2 **
# End: **
