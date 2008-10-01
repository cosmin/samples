from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

VERSION_DATA = dict(version = None, filepath = None) # just an example

import sys, os
import logging
import simplejson

log = logging.getLogger(__name__)

class UpdateHandler(BaseHTTPRequestHandler):
    """
    Handler updates, urls are:
      /version/ => get the latest version data
      /download/<version>/ => download the file for this version
    """
        

    def do_GET(self):
        if self.path == "/version/":
            self.send_version()
        elif self.path.startswith('/download/'):
            ver = self.path.split('/')[1]
            self.send_file(ver)
        else:
            self.send_response(404)
            # using text/html instead of json since it's easier to open in browser
            self.send_header('Content-type', 'text/html')
            self.end_headers()
            self.wfile.write("Request path %s not found" % self.path)
            return

    def send_version(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()
        ver = VERSION_DATA["version"]
        data = dict(version = ver,url = "/download/%s/" % ver, size = VERSION_DATA['size'])
        if VERSION_DATA['hash']:
            data['hash'] = VERSION_DATA['hash']
        self.wfile.write(simplejson.dumps(data))
        return

    def send_file(self, version):
        f = open(VERSION_DATA['path'], 'rb')
        self.send_response(200)
        self.send_header('Content-type', 'application/x-compressed')
        self.send_header('Content-Disposition', 'attachment; filename="%s"' % VERSION_DATA["filename"])
        self.end_headers()
        while True:
            data = f.read(1024)
            if not data:
                break
            self.wfile.write(data)
            self.wfile.flush()
        return

def check_file(filepath):
    if not (os.path.exists(filepath) and os.path.isfile(filepath)):
        log.error("Path %s does not exist, or is not a file" % filepath)
        return False
    if not os.access(filepath, os.R_OK):
        log.error("Cannot read from the file %s" % filepath )
        return False
    else:
        return os.path.basename(filepath)

def main(version, filepath, sig = None):
    filename = check_file(filepath)
    if not filename:
        return
    else:
        size = os.stat(filepath)[os.path.stat.ST_SIZE]
        VERSION_DATA.update({'filename': filename, 'path' : filepath, 'version': version,
                             'hash': sig, 'size' : size})
        
    try:
        server = HTTPServer(('localhost', 8181), UpdateHandler)
        log.info('started update server...')
        server.serve_forever()
    except KeyboardInterrupt:
        log.info('shuttint down update server.')
        server.socket.close()

if __name__ == "__main__":
    if len(sys.argv) == 4:
        main(version=sys.argv[1], filepath=sys.argv[2], sig = sys.argv[3])
    else:
        main(version=sys.argv[1], filepath=sys.argv[2])
