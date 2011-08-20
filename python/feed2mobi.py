#!/usr/bin/env python
# -*- coding: utf-8 -*-

import datetime
import os
import os.path
import sqlite3
import sys

from optparse import OptionParser
from StringIO import StringIO

from operator import itemgetter
from itertools import groupby
from subprocess import call

from lxml import etree
from lxml.etree import LxmlSyntaxError

def generateHTMLNode():
  html = etree.Element('html')
  head = etree.SubElement(html, 'head')
  # This is important for kindlegen to decode utf-8 files
  etree.SubElement(head, 'meta', attrib={
      'http-equiv': 'Content-Type',
      'content': 'text/html;charset=UTF-8'})
  return html


class Feed(object):

  NSS = {
    'content': 'http://purl.org/rss/1.0/modules/content/',
    'dc': 'http://purl.org/dc/elements/1.1/'
    }

  def __init__(self, url):
    self._url = url

  def fetch(self):
    print 'Fetching', self._url
    self._rss = etree.parse(self._url)
    if not ('rss' == self._rss.getroot().tag and \
              '2.0' == self._rss.getroot().attrib.get('version')):
      raise 'Only RSS 2.0 is accepted'


  def getNodeText(self, parent, *nodes):
    for node in nodes:
      val = parent.xpath(node+'/text()', namespaces=self.NSS)
      if val:
        return val[0]


  def writefile(self, path, title, content):
      parser = etree.HTMLParser(encoding='utf-8')
      html = etree.parse(StringIO(content.encode('utf-8')), parser)
      # Remove empty links and images
      for img in html.xpath('//img'):
        img.getparent().remove(img)
      for a in html.xpath('//a'):
        if not a.text:
          a.getparent().remove(a)
        # else:
        #   a.tag='span'
        #   a.attrib.clear()

      cont_nodes = html.xpath('/html/body')[0].getchildren()
      html = generateHTMLNode()
      body = etree.SubElement(html, 'body')
      etree.SubElement(body,'h2').text = title
      body.extend(cont_nodes)
      with open(path, 'w') as fo:
        fo.write(etree.tostring(html,
                                pretty_print=True,
                                encoding='utf-8',
                                xml_declaration=False))

  def writefiles(self, feedDir, feedId, conn):
    c = conn.cursor()
    for item in reversed(self._rss.xpath('/rss/channel/item')):
      guid = self.getNodeText(item, 'guid', 'link')
      title = self.getNodeText(item, 'title')
      pubDate = self.getNodeText(item, 'pubDate')
      content = self.getNodeText(item, 'content:encoded', 'description')
      creator = self.getNodeText(item, 'dc:creator')

      itemId = None
      c.execute('SELECT id, pub_date FROM feed_item WHERE guid=?', (guid,))
      cItem = c.fetchone()
      if cItem:
        cId, cPubDate = cItem
        if cPubDate != pubDate:
          c.execute('UPDATE feed_item SET pub_date=?, creator=?, title=?, is_new=? WHERE id=?',
                    (pubDate, creator, title, True, cId))
          itemId = cId
        else:
          continue
      else:
        c.execute('INSERT INTO feed_item (feed_id, guid, title, creator, pub_date) VALUES (?,?,?,?,?)',
                  (feedId, guid, title, creator, pubDate))
        itemId = c.execute('SELECT id FROM feed_item WHERE guid=?', (guid,)).fetchone()[0]

      itemPath = os.path.join(feedDir, str(itemId)+'.html')
      self.writefile(itemPath, title, content)
      conn.commit()
      print guid, 'saved in file', itemPath

    c.close()


  def _get_title(self):
    txt = self._rss.xpath('/rss/channel/title/text()')
    return txt[0] if len(txt)>0 else None
    return self._title
  title = property(_get_title)

  def _get_url(self):
    return self._url
  url = property(_get_url)

  def _get_lastBuildDate(self):
    txt = self._rss.xpath('/rss/channel/lastBuildDate/text()')
    return txt[0] if len(txt)>0 else None
  lastBuildDate = property(_get_lastBuildDate)

  def __repr__(self):
    return '<Feed "%s">' % self._url


class KindleGen(object):

  TITLE = 'Feed2Mobi'
  TOC = 'feed2mobi.html'
  NCX = 'feed2mobi.ncx'
  OPF = 'feed2mobi.opf'

  MIME = {
    'html' : 'application/xhtml+xml',
    'jpg' : 'image/jpeg',
    'gif' : 'image/gif',
    'png' : 'image/png',
    'ncx' : 'application/x-dtbncx+xml'
    }

  def __init__(self, conn, program='kindlegen', clean=True):
    self._conn = conn
    self._program = program
    self._clean = clean

  def execute(self):
    c = self._conn.cursor()
    items = c.execute('''SELECT b.id, a.id, b.title, a.title, a.creator
FROM feed_item a join feed b on a.feed_id=b.id
WHERE a.is_new=1
ORDER BY b.id, a.id''').fetchall()
    if not len(items):
      return

    items = [(ftitle, title, creator, os.path.join(str(fid), str(iid)+'.html'), iid) \
               for (fid, iid, ftitle, title, creator) in items]

    genDate = datetime.datetime.now().strftime('%Y-%m-%d')
    genId = self.TITLE+'_'+genDate

    self.generateTOC(items)
    self.generateOPF(genId, genDate, items)
    self.generateNCX(genId, items)

    call([self._program, '-c2', '-o', genId + '.mobi', self.OPF])

    c.executemany('UPDATE feed_item SET is_new=0 WHERE id=?',
                  [(item[4],) for item in items])
    self._conn.commit()
    c.close()

  def generateTOC(self,items):
    html = generateHTMLNode()
    body = etree.SubElement(html, 'body')
    etree.SubElement(body,'h2').text = 'Table of Contents'

    for section, articles in groupby(items, itemgetter(0)):
      etree.SubElement(body, 'h4').text = section
      for article in articles:
        etree.SubElement(body, 'a', attrib={'href':article[3]}).text = article[1]
        etree.SubElement(body, 'br')

    with open(self.TOC, 'w') as fo:
      fo.write(etree.tostring(html,
                              pretty_print=True,
                              encoding='utf-8',
                              xml_declaration=False))


  def generateOPF(self, genId, genDate, items):
    opf_namespace = 'http://www.idpf.org/2007/opf'
    dc_namespace = 'http://purl.org/dc/elements/1.1/'
    dc_metadata_nsmap = { 'dc' : dc_namespace }
    dc = '{{{0}}}'.format(dc_namespace)

    package = etree.Element('{{{0}}}package'.format(opf_namespace),
                            nsmap={None:opf_namespace},
                            attrib={'version':'2.0',
                                    'unique-identifier':genId})
    metadata = etree.Element('metadata')
    package.append(metadata)

    # etree.SubElement(metadata,'meta',attrib={'name':'cover',
    #                                           'content':cover[:-4]})
    dc_metadata = etree.Element('dc-metadata',
                                nsmap=dc_metadata_nsmap)
    metadata.append(dc_metadata)

    etree.SubElement(dc_metadata,dc+'title').text = self.TITLE
    etree.SubElement(dc_metadata,dc+'language').text = 'en-us'
    etree.SubElement(dc_metadata,dc+'creator').text = self.TITLE
    etree.SubElement(dc_metadata,dc+'publisher').text = self.TITLE
    etree.SubElement(dc_metadata,dc+'subject').text = "News"
    etree.SubElement(dc_metadata,dc+'date').text = genDate
    etree.SubElement(dc_metadata,dc+'description').text = '{0} on {1}'.format(self.TITLE, genDate)

    x_metadata = etree.Element('x-metadata')
    metadata.append( x_metadata )
    etree.SubElement(x_metadata,'output',attrib={'encoding':'utf-8',
                                                 'content-type':'application/x-mobipocket-subscription-magazine'})

    manifest = etree.SubElement(package,'manifest')

    files = [self.TOC] + [item[3] for item in items]
    for f in  files + [self.NCX]:
      etree.SubElement(manifest, 'item',
                       attrib={'id' : f,
                               'media-type' : self.MIME[f.split('.')[-1]],
                               'href' : f})

    spine = etree.SubElement(package, 'spine',
                             attrib={'toc' : self.NCX})
    for f in files:
      etree.SubElement(spine, 'itemref',
                       attrib = {'idref' : f})


    guide = etree.SubElement(package,'guide')
    etree.SubElement(guide,'reference',
                     attrib={'type':'toc',
                             'title':'Table of Contents',
                             'href':self.TOC})
    etree.SubElement(guide,'reference',
                     attrib={'type':'text',
                             'title':'Welcome',
                             'href':self.TOC})

    with open(self.OPF, 'w') as fo:
      fo.write(etree.tostring(package,
                              pretty_print=True,
                              encoding='utf-8',
                              xml_declaration=True))


  def generateNavPoint (self, nav_point_node, label, source):
    content = etree.Element('content', attrib={'src' : source})
    text_element = etree.Element('text')
    text_element.text = label
    nav_label = etree.SubElement(nav_point_node, "navLabel")
    nav_label.append(text_element)
    nav_point_node.append(content)


  def generateNCX (self, genId, items):
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
                             'content' : genId })
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
    title_text_element.text = self.TITLE
    author_text_element = etree.Element("text")
    author_text_element.text = self.TITLE

    etree.SubElement(ncx,'docTitle').append(title_text_element)
    etree.SubElement(ncx,'docAuthor').append(author_text_element)

    nav_map = etree.SubElement(ncx,'navMap')

    nav_point_periodical = etree.SubElement(nav_map, 'navPoint',
                                            attrib={'class': 'periodical', 'id': 'periodical', 'playOrder': '0'})
    self.generateNavPoint(nav_point_periodical, 'Table of Contents', self.TOC)

    i = 1
    scount = 0
    for section, articles in groupby(items, itemgetter(0)):
      scount = scount+1
      articles = list(articles)
      nav_point_section = etree.SubElement(nav_point_periodical,"navPoint",
                                           attrib={"class" : "section",
                                                   "id" : ('sec_' + str(scount)),
                                                   "playOrder" : str(i) })
      self.generateNavPoint(nav_point_section, section, articles[0][3])
      i += 1

      acount = 0
      for article in articles:
        acount += 1
        nav_point_article = etree.SubElement(nav_point_section,"navPoint",
                                             attrib={"class" : "article",
                                                     "id" : ('art_' + str(scount) + '_' + str(acount)),
                                                     "playOrder" : str(i) })
        self.generateNavPoint(nav_point_article, article[1], article[3])
        i += 1

    with open(self.NCX, 'w') as fo:
      fo.write('<?xml version="1.0" encoding="utf-8"?>\n')
      fo.write('<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">\n')
      fo.write(etree.tostring(ncx,
                              pretty_print=True,
                              encoding="utf-8",
                              xml_declaration=False))



if __name__ == '__main__':
  parser = OptionParser('Usage: %prog [options] [feed_url ...]')
  parser.add_option('--add', dest='add', action='store_true',
                    help='Add URLs of feeds into database')
  parser.add_option('--fetch', dest='fetch', action='store_true',
                    help='Fetch latest items of feeds')
  parser.add_option('--mobi', dest='mobi', action='store_true',
                    help='Call kindlegen to build .mobi')
  options, args = parser.parse_args()


  basepath = os.path.join(os.path.expanduser('~'), '.feed2mobi')
  try:
    os.mkdir(basepath)
  except:
    pass

  conn = sqlite3.connect(os.path.join(basepath, 'feed2mobi.db'))

  c = conn.cursor()
  c.execute('''
CREATE TABLE IF NOT EXISTS feed
(
id INTEGER PRIMARY KEY AUTOINCREMENT,
title TEXT NOT NULL,
url TEXT NOT NULL UNIQUE,
last_build_date TEXT
)
''')
  c.execute('''
CREATE TABLE IF NOT EXISTS feed_item
(
id INTEGER PRIMARY KEY AUTOINCREMENT,
feed_id INTEGER NOT NULL,
guid TEXT UNIQUE,
title TEXT NOT NULL,
is_new INTEGER DEFAULT 1,
creator TEXT,
pub_date TEXT,
CONSTRAINT fk_feed_id FOREIGN KEY (feed_id) REFERENCES feed (id)
)
''')
  c.close()

  if options.add:
    c = conn.cursor()
    for url in args:
      if c.execute('SELECT 1 FROM feed WHERE url=?', (url,)).fetchone():
        print url, 'already added'
      else:
        feed = Feed(url)
        try:
          feed.fetch()
        except LxmlSyntaxError as (msg,):
          print 'XML Parsing error:', msg
          continue
        except:
          print 'Unexpected error:', sys.exc_info()[0]
          continue

        print feed.lastBuildDate
        c.execute('INSERT INTO feed (title, url) values (?, ?)',
                  (feed.title, feed.url))
        conn.commit()
        print url, 'added'
    c.close()

  if options.fetch:
    c = conn.cursor()

    c.execute('SELECT id, url, title, last_build_date FROM feed')
    feeds = c.fetchall()

    for feedId, url, title, lastBuildDate in feeds:
      feed = Feed(url)
      try:
        feed.fetch()
      except LxmlSyntaxError as (msg,):
        print 'XML Parsing error:', msg
        continue
      except:
        print 'Unexpected error:', sys.exc_info()[0]
        continue

      if feed.lastBuildDate and feed.lastBuildDate == lastBuildDate:
        continue

      feedDir = os.path.join(basepath, str(feedId))
      try:
        os.mkdir(feedDir)
      except:
        pass
      feed.writefiles(feedDir, feedId, conn)

      c.execute('UPDATE feed SET last_build_date=? WHERE id=?', (feed.lastBuildDate, feedId))
      conn.commit()

      if feed.title != title:
        c.execute('UPDATE feed SET title=? WHERE id=?', (feed.title, feedId))
        conn.commit()

  if options.mobi:
    os.chdir(basepath)
    kindlegen = KindleGen(conn)
    kindlegen.execute()

  conn.close()


# Local Variables: **
# comment-column: 56 **
# indent-tabs-mode: nil **
# python-indent: 2 **
# End: **
