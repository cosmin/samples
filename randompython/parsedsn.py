import re

regex = re.compile(r':(mysql|postgres)://(?:([\w]+)(?::([\w]+))?@)?([\w.-]+)(?::([\d]{1,5}))?')

def parsedsn(string):
    return regex.match(string)
