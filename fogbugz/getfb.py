#!/usr/bin/env python
# This script will download attachments from FogBugz issue tracking system. To run it use
## python getfb.py <startnumber> <endnumber>
# This will download the cases between the start number and the end number
# This was written as a one-time throw away script so it is likely full of bugs and could be written much better
# but it got the job done. Use at your own risk!!!!

## Copyright notice
# Copyright (C) 2007, Cosmin Stejerean (http://www.offbytwo.com)
#
# You are free to use this code under the terms of the Creative Commons Attribution license
# available at http://creativecommons.org/licenses/by/3.0/
# so long as you include the following notice 'includes code from Cosmin Stejerean (http://www.offbytwo.com)'

import re
import os
import sys
from mechanize import Browser
re_att = re.compile("ref=\"(.*pgDownload\S+ixAttachment\S+sFileName=(.*))\"")
re_nocase = re.compile("does not exist or has been deleted")
re_amp = re.compile("\&amp;")
base_url = "***************" # URL to access FogBugz
base_path = "/Users/cosmin/fb/"
log = open("fbdownload.log", 'w')

def setup():
	print "Setting up browser!"
	br = Browser()
	br.open("***URL to fog bugz***")
	br.follow_link(url_regex=re.compile("pgLogon"))
	br.select_form(name="formWithPerson")
	br["sPerson"] = "**********" #username of account to use
	br["sPassword"] = "*********" #password of account to use
	br.submit()
	print "Logged in"
	return br

def get_attachments(response_text):
	pos = 0
	urls = []
	while True:
		m = re_att.search(response_text[pos:])
		if not m:
			break
		else:
			pos = pos + m.end()
			url = re_amp.sub("&",m.group(1))
			filename = m.group(2)
			urls.append((url, filename))
			
	return urls

def download(case_number, urls, br):
	location = os.path.join(base_path, str(case_number))
	ret = True
	try:
		if not os.path.isdir(location):
			os.mkdir(location)
	except:
		log.write("Cannot create directory for case %s\n", case_number)
		log.flush()
		return False
		
	for (url, filename) in urls:
		try:
			download_url = "%s%s" % (base_url, url)
			path = os.path.join(location, filename)
			out = open(path, 'wb')
			br.open(download_url)
			buf = br.response().read()
			out.write(buf)
			log.write("Downloaded case %s file %s size %s\n" % (case_number, filename, len(buf)))
			log.flush()
			out.flush()
			out.close()
		except:
			log.write("Error downloading case %s file %s\n" % (case_number, filename))
			log.flush()
			ret = False
		
	return ret

br = None

for x in range(int(sys.argv[1]),int(sys.argv[2])):
	if not br:
		br = setup()
		
	try:
		br.open("%sdefault.asp?pre=preMultiSearch&pg=pgList&pgBack=pgSearch&search=2&searchFor=%s" % (base_url, x))
	except:
		pass
		
	r = br.response()
		
	try:
		if re_nocase.search(r.read()):
			log.write("Case %s doesn't exist\n" % x)
			log.flush()
			continue
		else:
			r.seek(0)
			text = r.read()
			urls = get_attachments(text)
			if len(urls) > 0:
				log.write("Found attachments for case %s\n" % x)
				log.flush()
				download(x, urls, br)
			else:
				print "No attachments for case %s" % x
	except Exception, e:
		br.close()
		br = setup()
		log.write("Re-setup at %s\n" % x)
		log.flush()
		continue

print "DONE!"
log.flush()
log.close()
