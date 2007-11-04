import re, urllib2

BASE_URL = 'http://metrarail.com/'
LINE_LIST_URL = BASE_URL + 'schedule.html'


INBOUND = 0
OUTBOUND = 1

PARSING_RULES = {
    'http://metrarail.com/Sched/cnw_n/cnwn.shtml' : {
        'FROM-Monday-TO-Friday-Inbound' : 'http://metrarail.com/Sched/cnw_n/cnwn_wki.shtml',
        'FROM-Monday-TO-Friday-Outbound' : 'http://metrarail.com/Sched/cnw_n/cnwn_wko.shtml',
        'ONLY-Saturday-Inboud' : 'http://metrarail.com/Sched/cnw_n/cnwn_sai.shtml',
        'ONLY-Saturday-Outbound' : 'http://metrarail.com/Sched/cnw_n/cnwn_sao.shtml',
        'ONLY-Sunday-Inbound' : 'http://metrarail.com/Sched/cnw_n/cnwn_sui.shtml',
        'ONLY-Sunday-Outbound' : 'http://metrarail.com/Sched/cnw_n/cnwn_suo.shtml'
        },
    
    'http://metrarail.com/Sched/md_n/md_n.shtml' : {
        'FROM-Monday-TO-Friday-Inbound' : 'http://metrarail.com/Sched/mdn_n/mdn_wki.shtml',
        'FROM-Monday-TO-Friday-Outbound' : 'http://metrarail.com/Sched/mdn_n/mdn_wko.shtml',
        'ONLY-Saturday-Inboud' : 'http://metrarail.com/Sched/mdn_n/mdn_sai.shtml',
        'ONLY-Saturday-Outbound' : 'http://metrarail.com/Sched/mdn_n/mdn_sao.shtml',
        'ONLY-Sunday-Inbound' : 'http://metrarail.com/Sched/mdn_n/mdn_sui.shtml',
        'ONLY-Sunday-Outbound' : 'http://metrarail.com/Sched/mdn_n/mdn_suo.shtml'
        },
    
}

def parse(url_parsing_rules):
    data = {}
    
    days = {
        'Monday' : 1,
        'Tuesday' : 2,
        'Wednesday' : 3,
        'Thursday' : 4,
        'Friday' : 5,
        'Saturday' : 6,
        'Sunday' : 7
    }
    
    for i in url_parsing_rules.keys():
        k = i.copy()
        if k.endswith('-Inbound'):
            direction = INBOUND
            k = k.replace('-Inbound', '')
        elif k.endswith('-Outbound'):
            direction = OUTBOUND
            k = k.replace('-Outbound', '')
        else:
            raise ValueError('The supplied key %s does not match expected format' % k)
        
        if k.startswith('FROM-'):
            k = k.replace('FROM-', '')
            (start, end) = k.split('-TO-')
            for d in days:
                if d
            
        elif k.startswith('ONLY-'):
            k = k.replace('ONLY-', '')
            
    

def populate_schedule_listing():
    
    
    

def getLineListing(line_list_url = LINE_LIST_URL, line_list_re = LINE_LIST_RE, base_url = BASE_URL):
    """
    Read the url for the station listing and pulls in a list of tuples
    each tuple has the following format
    * i[0] = partial url (add BASE_URL to get full url)
    * i[1] = line name
    * i[2] = line description (<START> to <DESTINATION> Suburban Service)
    * i[3] = date last updated, can be helpful to determine when to reparse information
    """
    line_list_re = re.compile('<tr.+?<td.+?<a\shref=\"(Sched/\S+?)\".+?<b>(.+?)</b>.+?size=.*?\"1\">(.+?)</font>.+?size=\"2\">(.+?)</font>')
    
    html_data = urllib2.urlopen(line_list_url).read().replace('\n','').replace('\t','').replace('\r','') #get rid of unecessary whitespace
    while html_data.find('  ') > -1:
        html_data = html_data.replace('  ', ' ') #HTML doesn't care about multiple spaces, we do
    return data = line_list_re.findall(html_data)
    


def getLineInformation(line):
    """
      Read information for the given line where line is a tuple in the format returned from getLineListing()
    """
    
    pass

def getStationList(line_url):
    '''
    Return a list of tuples with each tuple representing information on one station in the system
    Each tuple has the following format
    
    i[0] = partial_url (add base url of line to fetch)
    i[1] = station_name
    i[2] = fare_code (so far seems to be between A and M)
    '''
    html_data = urllib2.urlopen(line_url).read()
    station_start_re = re.compile('STATION\s+?ZONE\s+?ADDRESS\s+?PHONE')
    
    #find the beginning of the station listing and take only subsequent data
    station_list_data = station_start_re.split(html_data)[1].replace('href=\r\n', 'href=') 
    
    # don't read address and phone numbers from here since it is poorly formatted
    station_re = re.compile('<a href=\"(.+?)\">(.+?)</a>\s+?([A-Z])')
    
    return data = station_re.findall(station_list_data)
    

