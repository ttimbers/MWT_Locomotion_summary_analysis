##sys library is required for arguments 
import sys

##open a file as strings: This process is necessary for the ""regex.findall"" function.
lines = []

lines = open(sys.argv[1]).read()

## If file size is small, it is possible to use re library.
## To use regex library, download and install the library. 
## regex is a faster regular expression library than re library.
## pandas is dataframe library
import regex
import pandas as pd

## compile a regular expression pattern. It will speed up the ""findall"" function upto 100 times.
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

## To save memory and avoid to recall recent data
del lines

## read a file as table
gDat = pd.read_table(sys.argv[1], header=None, names = ["v1", "ID", 
"BIAS", "SPEED", "MORPHWIDTH", "MIDDLINE", "AREA", "loc_x", "loc_y"], 
delim_whitespace=True)

## delete column(v1) from the table
newgDat = gDat.drop('v1', axis=1)
del gDat

## merge two tables to one table
Newdata = pd.merge(df, newgDat, left_index=True, right_index=True, 
how='outer')

## export revised table to newfile

Newdata.to_csv("newmerged.file", sep='\t')
