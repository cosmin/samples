#!/usr/bin/env python2.5

import re
import os
import pygame
import signal
import simplejson
import subprocess
import sys
import threading
import tarfile
import time
import urllib2

from uilib import DisplayManager, ProgressBar, MessageWidget
import remover

REMOTE_SERVER = "http://localhost:8181"
DOWNLOAD_PATH = os.path.expanduser('/tmp/pygameapp-releases')
CURRENT_VERSION = 1

class RefreshThread(threading.Thread):

    def __init__(self, display_manager):
        threading.Thread.__init__(self)
        self._stopevent = threading.Event()
        self.display_manager = display_manager
        self.setDaemon(True) # so we don't have to wait for it to finish

    def run(self):
        while not self._stopevent.isSet():
            try:
                self.display_manager.refresh()
            except AttributeError:
                break
            time.sleep(0.03)
            
    def join(self, timeout = None):
        self._stopevent.set()
        threading.Thread.join(self, timeout)

def attachment_filename(response):
    rex = re.compile(r'filename="(.*)"')
    filenames = rex.findall(response.headers.get('content-disposition'))
    if len(filenames):
        return filenames[0]
    else:
        return None

def setup():
    pygame.init()
    dm = DisplayManager(800,480)
    mainloop = RefreshThread(dm)
    mainloop.start()
    pb = ProgressBar()
    pb.value = 0
    ma = MessageWidget(400, 20, font_size=16)
    dm.add_widget(pb, left=100, top=280, name = "progress")
    dm.add_widget(ma, left=300, top= 250, name = "message")

    return dm, mainloop

def client_needs_update(data):
    return int(data['version']) > CURRENT_VERSION

def fetch_update(data, dm):
    pb = dm.by_name("progress")
    ma = dm.by_name("message")

    ma.message = "Downloading update..."
    try:
        con = urllib2.urlopen(REMOTE_SERVER + data['url'])
        size = data["size"]
        assert con.headers.get('content-type') == 'application/x-compressed'
        filename = attachment_filename(con)
        assert filename is not None, "We wrote the server, this is not right"
        out = open(os.path.join(DOWNLOAD_PATH, os.path.basename(filename)), 'wb')
                    
        BLOCKSIZE = 1024
        total_read = 0
        value = 0
        last_value = 0
        while True:
            recv = con.read(BLOCKSIZE)
            if not recv:
                pb.value = 90
                ma.message = "Extracting contents..."
                time.sleep(2)
                out.flush()
                out.close()
                return out.name, data['version']
            out.write(recv)
            total_read += len(recv)
            
            value = int(round(total_read * 90.0 / size))
            if value > last_value:
                # only update the progress bar when value moves by 1 or more
                pb.value = value
                
            last_value = value

    except urllib2.URLError:
        ma.message = "Error downloading update"
        time.sleep(1)

def extract_archive(path, to):
    tf = tarfile.open(path)
    tf.extractall(to)

def check_update(path_to_update):
    f = os.path.join(path_to_update, "main.py")
    if subprocess.call([f,  "--self-check"]) == 0:
        return True
    else:
        return False
    
def main():
    if not os.path.exists(DOWNLOAD_PATH):
        os.mkdir(DOWNLOAD_PATH)
        print "Created download location %s" % DOWNLOAD_PATH
        
    if os.path.isdir(DOWNLOAD_PATH) and os.access(DOWNLOAD_PATH, os.W_OK):
        print "Updates will be saved to %s" % DOWNLOAD_PATH
    else:
        print "Error: %s is not a writable folder" % DOWNLOAD_PATH
        return 1
    
    dm, mainloop = setup() # get display manager
    try:
        pb = dm.by_name("progress")
        ma = dm.by_name("message")
        
        ma.message = "Contacting update server.."
        time.sleep(1)

        try:
            resp = urllib2.urlopen(REMOTE_SERVER+"/version/")
            data = simplejson.loads(resp.read())
        except urllib2.URLError:
            ma.message = "Error: cannot access update server."
            pb.value = 100
            time.sleep(2)
            return

        if client_needs_update(data):
            filepath, version = fetch_update(data, dm)
            update_path = os.path.join(DOWNLOAD_PATH, 'v%s' % version)
            if os.path.exists(update_path):
                # careful not to remove critical system files
                remover.recursive_rmdir(update_path)
            os.mkdir(update_path)
            extract_archive(filepath, update_path)
            os.unlink(filepath)
            pb.value = 100
            ma.message = "Verifying update..."
            time.sleep(1)
            if check_update(update_path):
                ma.message = "Update complete."
                mainloop.join(3)
                dm.quit()
                remove_path = os.path.dirname(remover.__file__)
                start_path = os.path.join(update_path, 'main.py')
                print "Remove %s" % remove_path
                print "Start %s" % start_path
                remover.main(remove_path, start_path)
                return
            else:
                ma.message = "Update verification failed"
                remover.recursive_rmdir(update_path)
                pb.value = 100
                time.sleep(1)
                
        else:
            ma.message = "No updates found."
            pb.value = 100
            time.sleep(1)
    finally:
        dm.quit()

if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1] == "--self-check":
        # check that we're installed and able to run properly
        dm, mainloop = setup()
        dm.quit()
        sys.exit(0)
    else:
        main()
    

