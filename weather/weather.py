#! /usr/bin/env python

import sys
import urllib
from xml.dom.minidom import parse

urlarea = 'http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query=' + sys.argv[1]
domarea = parse(urllib.urlopen(urlarea))
wu_idpws = str(domarea.getElementsByTagName('id')[0].toprettyxml()).strip("<id>\n").strip("</id>\n").replace("![CDATA[","").replace("]]","").strip()

url = 'http://api.wunderground.com/weatherstation/WXCurrentObXML.asp?ID=' + wu_idpws
dom = parse(urllib.urlopen(url))

current_node = dom.firstChild
location_node = current_node.childNodes[7]
wu_location = location_node.childNodes[1]

wu_temp = str(dom.getElementsByTagName('temperature_string')[0].toprettyxml()).strip("<temperature_string>\n").strip("</temperature_string>\n")
degrees_f = u"\u00B0".join(wu_temp.split(' ')[0:2]).strip()

print degrees_f
