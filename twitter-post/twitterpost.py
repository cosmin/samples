import os
import urllib2
import urllib

# change the following to your values
TWITTER_USERNAME = ''
TWITTER_PASSWORD = ''

BITLY_USERNAME = ''
BITLY_API_KEY = ''

MESSAGE_PREFIX = ''

def post_to_twitter(message):
    os.system('curl -u %s:%s -d status="%s" http://twitter.com/statuses/update.xml' % (
              TWITTER_USERNAME,
                    TWITTER_PASSWORD,
              message))

def shorten(url):
    q = urllib.urlencode(dict(version='2.0.1',login=BITLY_USERNAME, apiKey=BITLY_API_KEY, longUrl=url))
    url = 'http://api.bit.ly/shorten?' + q
    data = eval(urllib2.urlopen(url).read())
    return data['results'].values()[0]['shortUrl']

def notify(video_name, video_url):
    short_url = shorten(video_url)
    max_video_len = 140 - len(' from @' + TWITTER_USERNAME) - len(MESSAGE_PREFIX) - len(short_url)
    if len(video_name) > max_video_len:
        video_name = video_name[:max_video_len - 3] + '...'
    message = '%s %s %s' % (MESSAGE_PREFIX, video_name, short_url)
    post_to_twitter(message)
    return True
