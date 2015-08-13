## This is the driver script that will call modular scripts to attack each chunk
## of the problem.
##
## This is the Windows version of the script. 
## It currently requires Chore.jar to be in the data folder. 
## It also currently requires the zip command to be added to the bash shell (taken from cygwin). 
##
## This script will later be updated to work with Homebrew or a manually specified chore path.
## The script will later be updated to use a built in windows shell zip command (possibly compact?).
##
## Before running this script, set the working directory to project's root directory.
##
## Requires the following input from the user:
##		$1: gigabytes of memory to be used to run Choreography (dependent upon
##			the machine you are using
##      $2: control strain, which will be plotted first and used as a baseline for
##          radarplot strain comparisons.
##          NOTE: input is case-sensitive!
##		$3: webdav server URL
##		$4: path on webdav where .zip folders should be saved (including where it is
##			mounted on your computer (e.g /path_on_webdav)
##
##      To run analysis without backing up to webdav server,
##      do not provide webdav server URL ($3) and path ($4).
##
## Example usage of this script from the Bash Shell - with backup to webdav
## (After working directory has been set to project's root directory):
## bash bin/locomotion_driver.sh 1 N2 https://webdav.server/location folder_to_backup_to
##
## Example usage of this script from the Bash Shell - without backup to webdav
## bash bin/locomotion_driver.sh 1 N2

## Move into data folder; all data folders to be analyzed must be in this directory
cd data

## check if webdav server URL and path are both provided; if provided, zip files and move
## to webdav server.
## Otherwise, declare backed_up variable as false (used to notify user at end of script)
if [ -z $3 ] && [ -z $4 ];
then
backed_up=false
echo "Data was not backed up to webdav."
else
## Connect to webdav (so you can backup files)
## User will be prompted for username and password.
net use Z: $3

## zip all MWT data folders in directory to be analyzed
for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done

## move .zip files to a webdav server
## Note - this is very slow...
mv *.zip $4
fi

## call choreography to analyze the MWT data (each folder within the data directory)
for folder in */; do java -Xmx$1g -jar Chore.jar --shadowless -p 0.027 -M 2 -t 20 -S -N all -o fDpesSlLwWaAmMkbPcdxyuvor1234 --plugin Reoutline::despike --plugin Respine --plugin MeasureReversal::all $folder; done

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
##grep -r '[0-9]' $(find ./data -name '*.dat') > merged.file
for filename in $(find . -name '*.dat'); do grep -H '[0-9]' $filename >> merged.file; done
cd ..

## Use regular expressions in R to parse apart the information in the filepath
## so we can get data, plate ID and strain as delimited columns
## call R script with the command line using an argument for the filename we want to parse
## After data is parsed, figures are plotted and stats are done and saved in results 
## directory
rscript bin/Column_identification.R data/merged.file $2

## If script did not attempt to backup data to webdav, notify user at end of script
if [ $backed_up == false ]; then
	echo "Data was not backed up to webdav."
fi