import sys

lines = []

lines = open(sys.argv[1]).read()

import regex
import pandas as pd

PA = regex.compile('./([0-9]{8})_')
PB = regex.compile('./([0-9]{8}_[0-9]{6})/')
PC = regex.compile('/([A-Za-z]+[-]?[0-9]+)')
N = regex.compile(':([0-9]+[.][0-9]+)')
Date = PA.findall(lines)
Plate = PB.findall(lines)
Strain = PC.findall(lines)
Time = N.findall(lines)
table = {'DATE' : Date, 'PLATE' : Plate, 'TIME' : Time, 'STRAIN' : 
Strain}
df = pd.DataFrame(table)
df = df[['DATE', 'PLATE', 'TIME', 'STRAIN']]

del lines

## read a file as table
gDat = pd.read_table(sys.argv[1], header=None, names = ["v1", "ID", 
"BIAS", "SPEED", "MORPHWIDTH", "MIDDLINE", "AREA", "loc_x", "loc_y"], 
delim_whitespace=True)

## delete column(v1) from table
newgDat = gDat.drop('v1', axis=1)
del gDat
Newdata = pd.merge(df, newgDat, left_index=True, right_index=True, 
how='outer')

import atpy

Newdata.to_csv("newmerged.file", sep='\t')
