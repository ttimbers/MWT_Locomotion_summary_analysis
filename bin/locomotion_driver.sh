## This is the driver script that will call modulars scripts to attack each chunk
## of the problem
##
## Set working directory to project's root directory
##
## Requires the following input from the user:
##		$1: webdav server URL 
##		$2: path on webdav where .zip folders should be saved (including where it is 
##			mounted on your computer (e.g /path_on_webdav)
##		$3: gigabytes of memory to be used to run Choreography (dependent upon
##			the machine you are using

## Connect to webdav (so you can backup files)
## you will be prompted for your webdav username and password
mount_webdav -i  $1 /
## for window users, simply do "cd path of webdav driver"

## zip all folders all MWT data folders in directory to be analyzed
## must be in "data" working directory 
cd data
for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done

## destroy/delete unzipped MWT folder
for uncompressed in */; do rm -r $uncompressed/; done

## copy .zip files to a webdav server
## Note - this is very slow...
cp *.zip $2

## call choreography to analyze the MWT data (each .zip in the folder data)
## error: Exactly one filename required
##  Use --help to list valid options.
for zipfolder in *.zip; do Chore -Xmx$3g --shadowless -p 0.027 -M 2 -t 20 -S -N all -o fDpesSlLwWaAmMkbPcdxyuvor1234 --plugin Reoutline::despike --plugin Respine --plugin MeasureReversal::all $zipfolder; done

## move unzipped folder into a new directory (called chore_data)
mv */ $(mkdir chore_data)

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
##grep -r '[0-9]' $(find ./data -name '*.dat') > merged.file
cd chore_data
for filename in $(find . -name '*.dat'); do grep -H '[0-9]' $filename >> merged.file; done
cd ../..

## Use regular expressions in R to parse apart the information in the filepath
## so we can get data, plate ID and strain as delimited columns
## call R script with the command line using an argument for the filename we want to parse
## After data is parsed, figures are plotted and stats are done and saved in results 
## directory
rscript bin/Column_identification.R data/chore_data/merged.file 
