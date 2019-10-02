#!/usr/bin/env python
# coding: iso-8859-1

import sys, operator
from collections import Counter
import csv

import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sklearn import preprocessing
from sklearn import linear_model


##  ##  ##  ##  ##  

datadir = '/accounts/grad/ckearns/Documents/research/school_reviews/data_nyc/'
csvfilename = 'fulldata_StatenIsland.csv'
datafile = datadir + csvfilename

#https://github.com/dbamman/anlp19/blob/master/4.classification/CheckData_TODO.ipynb
#https://realpython.com/python-csv/

#Need to tokenize data 

with open(datafile) as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    labelCounts=Counter()
    zeroLength=0
    total=0
    for row in csv_reader:
        if total == 0:
            print(f'Column names are {", ".join(row)}')
            total += 1
        else:
            text = row[5]
            rating = row[2]
            if text == 'NA': 
                text = ''
            if len(text) == 0: 
                zeroLength += 1
            total += 1
            labelCounts[rating]+=1
    print ("File: %s, Total docs: %s, Total zero length: %s" % (csvfilename, total, zeroLength))
    for label in sorted(labelCounts):
        print ("\t%s %s" % (label, labelCounts[label]))
    print()




stop

with open(datafile) as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            print(f'Column names are {", ".join(row)}')
            line_count += 1
        else:
            #print(f'\t{row[0]} works in the {row[1]} department, and was born in {row[2]}.')
            line_count += 1
    print(f'Processed {line_count} lines.')
 
print(csv_reader[0:1])

