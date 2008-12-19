#!/usr/bin/env python

import logging
import tempfile
import os, sys
import re
import subprocess

from nose.plugins import Plugin
from nose.util import tolist

ENGINES = ('mysql',)

engine_re = '(' + '|'.join(ENGINES) + ')'

r = engine_re + r'://(?:([\w]+)(?::([\w]+))?@)?([\w.-]+)(?::([\d]{1,5}))?'
regex = re.compile(r)

def split_dsn(dsn):
    m = regex.match(dsn)
    if not m:
        raise ValueError('Cannot split DSN from %s' % dsn)
    else:
        return dict(zip(('engine','username','password','host','port'),
                        m.groups()))


def make_option_maker(parser, cmdline_prefix="", env_prefix="", dest_prefix=""):
    """Return a function that can be used to add_option with saner syntax.

    Arguments:
      parser: the parser object that will be used by the returned function
      cmdline_prefix: will be used together with the name to build the command
          line option. a suffix of -- and a prefix of - will be added so you
          do not need to include those (--cmdline_prefix-<name>)
      env_prefix: will be used together with the name to specify the environment
          variable from which the option will default (ENV_PREFIX_<NAME>)
      dest_prefix: will be used together with name to specify the variable name
          in which the value of the option will be stored
      """
    
    env_prefix = env_prefix.upper()
    
    if cmdline_prefix and not cmdline_prefix.endswith("-"):
        cmdline_prefix += "-"

    if env_prefix and not env_prefix.endswith("_"):
        env_prefix += "_"

    if dest_prefix and not dest_prefix.endswith("_"):
        dest_prefix += "_"
        
    def option_maker(name, action = "store", help = "", default = None):
        """Use the given name, action and help string to add an option"""

        if default is None:
            _default = os.environ.get(env_prefix + name.upper())
        else:
            _default = default
            
        parser.add_option("--" + cmdline_prefix + name.lower(),
                          action = action, dest = dest_prefix + name.lower(),
                          default= _default,
                          help = help + " [" + env_prefix + name.upper() + "]")

    return option_maker
                          
                          
class DatabaseSnapshotPlugin(Plugin):
    """Plugin that takes a snapshot of Daisy before tests start and restores it
    """

    enabled = True
    score = 1

    def options(self, parser, env=None):
        Plugin.options(self, parser, env)
        omaker = make_option_maker(parser, dest_prefix = "dbdump",
                                   env_prefix = "NOSE_DBDUMP",
                                   cmdline_prefix = "dbdump")

        omaker('databases', action="append", help="Dump the selected databases")
        omaker('output', help="Path to store the database dump")
        omaker('dsn', help="engine://[user[:password]@]hostname[:port]")
        omaker('keep', action='store_true', help="Don't delete the database dump")

    def configure(self, options, config):
        self.conf = config
        self.databases = []

        if options.dbdump_databases:
            for lst in [tolist(x) for x in options.dbdump_databases]:
                self.databases.extend(lst)
                
        self.dsn = options.dbdump_dsn
        self.output = options.dbdump_output
        self.keep = options.dbdump_keep

        if not (self.databases and self.dsn):
            self.enabled = False
            return

        if not self.output:
            f,name = tempfile.mkstemp(suffix=".sql", prefix="nose-dbdump-")
            os.close(f)
            self.output = name

        for k,v in split_dsn(self.dsn).items():
            self.__dict__[k] = v


    def begin(self):
        """Take a snapshot of the database"""

        c = "mysqldump %s --databases %s > %s" % (self._get_connection_params(),
                                                  ' '.join(self.databases),
                                                  self.output)
        r = os.system(c)
        assert r == 0, "Got %s for %s" % (r, c)

    def _get_connection_params(self):
        c = ''
        if self.engine == 'mysql':
            if self.username:
                c += ' --user=%s' % self.username
            if self.password:
                c += ' --password=%s' % self.password

            if self.host == 'localhost' and not self.port:
                pass
            else:
                c += ' --host=%s' % self.host

            if self.port:
                c += ' --port=%s' % self.port

            return c


    def finalize(self, result):
        """Restore database to snapshot, if any"""
        
        c = "mysql %s < %s" % (self._get_connection_params(), self.output)
        #print c
        r = os.system(c)
        assert r == 0, "Got %s for %s" % (r, c)
        if not self.keep:
            print "Removing output from %s" % self.output
            os.remove(self.output)
        else:
            print "Leaving output at %s" % self.output

