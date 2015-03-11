## This is the driver script that will call modulars scripts to attack each chunk
## of the problem
##
## Set working directory to project's root directory
##
## Requires the following input from the user:
##		$1: webdav server URL
##		$2: path on webdav where .zip folders should be saved
##		$3: path to chore.jar (offline analys program Choreography)

## zip all folders all MWT data folders in directory to be analyzed
## must be in "data" working directory 
cd data
for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done
cd ..

## destroy/delete unzipped MWT folder
## Kwangjin

cd data
for A in *
do
rm -r $A/*
rmdir $A
done

## optional - copy .zip files to a webdav server
## Note: The mountpoint directory /Volumes/mntpnt/ must be created before 
## issuing the mount_webdav command. 
## you will be prompted for your webdav username and password
mount_webdav -i  $1 /Volumes/mntpnt/
mv data/20141118_161345.zip /Volumes/mntpnt/$2

## call choreography to analyze the MWT data (each .zip in the folder data)
cd data
for zipfolder in * do java -Xmx8g -jar $3 --shadowless -p 0.027 -M 2 -t 20 -S -N all -o DbsMmexy --plugin Reoutline::despike --plugin Respine --plugin MeasureReversal::all $zipfolder; done
cd ..

## move unzipped folder into a new directory (chore_data)
## Catrina

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
## Catrina

## create figures

## create stats
