## This is the driver script that will call modulars scripts to attack each chunk
## of the problem
##
## Set working directory to project's root directory
##
## Requires the following input from the user:
##		$1: webdav server URL
##		$2: path on webdav where .zip folders should be saved
##		$3: path to chore.jar (offline analys program Choreography)

## Connect to webdav (so you can backup files)
## you will be prompted for your webdav username and password
mount_webdav -i  $1 /Volumes

## zip all folders all MWT data folders in directory to be analyzed
## must be in "data" working directory 
cd data
for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done

## destroy/delete unzipped MWT folder
for uncompressed in */; do rm -r $uncompressed/; done

## copy .zip files to a webdav server
## Note - this is very slow...
cp *.zip /Volumes/$2

## call choreography to analyze the MWT data (each .zip in the folder data)
## error: Exactly one filename required
##  Use --help to list valid options.
for zipfolder in *.zip; do java -Xmx8g -jar $3 --shadowless -p 0.027 -M 2 -t 20 -S -N all -o DbsMmexy --plugin Reoutline::despike --plugin Respine --plugin MeasureReversal::all $zipfolder; done

##For window users: for zipfolder in *.zip; do java -Xmx1g -jar $3 --shadowless -p 0.027 -M 2 -t 20 -S -N all -o DbsMmexy --plugin Reoutline::despike --plugin Respine --plugin MeasureReversal::all $zipfolder; done


## move unzipped folder into a new directory (called chore_data)
mv */ $(mkdir chore_data)

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
##grep -r '[0-9]' $(find ./data -name '*.dat') > merged.file
cd chore_data
for filename in $(find . -name '*.dat'); do grep -r '[0-9]' $filename >> merged.file; done
cd ../..

## create figures

## create stats
