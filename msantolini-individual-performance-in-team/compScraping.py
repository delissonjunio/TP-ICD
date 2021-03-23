#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-
import requests
from bs4 import BeautifulSoup
import pandas as pd
import unicodedata
import re


r = requests.get('http://www.sports-reference.com/olympics/countries/')
html_doc = r.text
soup = BeautifulSoup(html_doc,'html.parser')

tags = soup.find_all('a')

def is_country(href):
    return href and re.compile("countries").search(href) \
            and str(href) != "/olympics/countries/"

tags = soup.find_all(href=is_country)

trigrams = [ str(t['href'].split('/')[-2]) for t in tags ]
countries = [ str(t.text) for t in tags ]
        
fname = 'Olympics.tsv'
fout = open(fname,'w')
fout.write('Trigram\tCountry\tYear\tEvent\t' + \
        'Rank\tAthlete\tGender\tAge\tSport\tGold' + \
        '\tSilver\tBronze\tTotal\tAthlete_unique_url\n')
fout.close()

for itri, tri in enumerate(trigrams):
    print tri

    r = requests.get('http://www.sports-reference.com/olympics/countries/' + tri + '/')
    html_doc = r.text
    soup = BeautifulSoup(html_doc,'html.parser')

    tags00 = soup.find_all(href=lambda h: h and re.compile(tri).search(h))
    tags0 = [ t for t in tags00 if len(str(t['href']).split('/')) > 6 ]

    for t in tags0:
        ss = str(t.text).split(' ')
        year = ss[0]
        event = ss[1]

        r = requests.get('http://www.sports-reference.com/olympics/countries/' + \
                tri + '/' + event.lower() + '/' + year + '/')

        #if r.status_code != 200:
            #break
        html_doc = r.text
        soup = BeautifulSoup(html_doc,'html.parser')

        #print(soup.prettify())

        tags = soup.find_all('td')

        i = 0
        tag = tags[i]
        while tag.has_attr('csk'):
            #print(tag)
            rank = str(tags[i].string)
            name0 = tags[i+1]['csk']
            name = unicodedata.normalize('NFKD', name0).encode('ascii','ignore')
            ref = str(tags[i+1].find('a')['href'])
            
            gender = str(tags[i+2].string)
            age = str(tags[i+3].string)
            sport = str(tags[i+4].string)
            Ngold = str(tags[i+5].string)
            if Ngold == 'None':
                Ngold = '0'
            Nsilver = str(tags[i+6].string)
            if Nsilver == 'None':
                Nsilver = '0'
            Nbronze = str(tags[i+7].string)
            if Nbronze == 'None':
                Nbronze = '0'
            Nall = str(tags[i+8].string)
            if Nall == 'None':
                Nall = '0'

            fout = open(fname,'a')
            fout.write(
                    tri + '\t' + \
                    countries[itri] + '\t' + \
                    year + '\t' + \
                    event + '\t' + \
                    rank + '\t'+ \
                    name + '\t' + \
                    gender + '\t' + \
                    age + '\t' + \
                    sport + '\t' + \
                    Ngold + '\t' + \
                    Nsilver + '\t' + \
                    Nbronze + '\t' + \
                    Nall + '\t' + \
                    ref + '\n')
            fout.close()
            i = i + 9
            tag = tags[i]


