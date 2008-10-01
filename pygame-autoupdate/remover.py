#!/usr/bin/env python2.5

import os
import sys
import time

def recursive_rmdir(start_path):
    """The name is deceiving, we're not using recursion"""
    
    for path, dirs, files in os.walk(start_path, False):
        for filename in files:
            os.unlink(os.path.join(path, filename))
        for dirname in dirs:
            os.rmdir(os.path.join(path, dirname))
    os.rmdir(start_path)

def main(path, new_path):
    recursive_rmdir(path)
    try: 
        pid = os.fork() 
        if pid > 0:
            # exit first parent
            sys.exit(0) 
    except OSError, e: 
        print >>sys.stderr, "fork #1 failed: %d (%s)" % (e.errno, e.strerror) 
        sys.exit(1)

    # decouple from parent environment
    os.chdir("/") 
    os.setsid() 
    os.umask(0) 
    
    # do second fork
    try: 
        pid = os.fork() 
        if pid > 0:
            # exit from second parent, print eventual PID before
            sys.exit(0)
        else:
            os.execl(new_path)
    except OSError, e: 
        print >>sys.stderr, "fork #2 failed: %d (%s)" % (e.errno, e.strerror) 
        sys.exit(1) 

if __name__ == "__main__":
    if len(sys.argv) == 3:
        main(sys.argv[1], sys.argv[2])
    else:
        print "Usage remover.py path/to/remove path/to/spawn"
