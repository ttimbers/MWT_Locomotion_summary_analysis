## This is the driver script that will call modular scripts to attack each chunk
## of the problem
##
## Before running this script, set the working directory to project's root directory.
##
## Requires the following input from the user:
##      $1: path to folder where MWT data folders are saved (absolute or relative to MWT root directory)
##      $2: path to folder where figures and statistics will be saved (aka "results") (absolute or relative)
##		$3: gigabytes of memory to be used to run Choreography (dependent upon
##			the machine you are using)
##      $4: control strain, which will be plotted first and used as a baseline for
##          radarplot strain comparisons.
##          NOTE: input is case-sensitive!
##		$5: webdav server URL
##		$6: path on webdav where .zip folders should be saved (including where it is
##			mounted on your computer (e.g /path_on_webdav)
##
##      To run analysis without backing up to webdav server,
##      do not provide webdav server URL ($5) and path ($6).
##
## Example usage of this script from the Bash Shell - with backup to webdav
## (After working directory has been set to project's root directory):
## bash bin/locomotion_driver.sh data results 1 N2 https://webdav.server/location folder_to_backup_to
##
## Example usage of this script from the Bash Shell - without backup to webdav
## bash bin/locomotion_driver.sh data results 1 N2

## store the directory from where the script was called as the initial directory;
## this will be used later to get back to the MWT analysis root directory
initial_wd=`pwd`

## If the given results folder (where analysis results will be saved) contains previous
## analysis results, the previous results will be overwritten.
## Lines 32 to 81 of this script check if the given results folder contains any files which
## will be overwritten, and prompts the user to either agree to continue, or cancel the analysis.
## Upon cancelling the script, the user may wish to re-name the files or move them before running
## the script, or to call the script with a new folder to save the results to.

## change path into given results directory
cd $2

## Make list of results files analysis currently generates
## Note that this MUST be updated every time a plot name is changed in the RScript, or a new plot is added. 
## Otherwise the script will automatically overwrite the previously made plot.
currentPlots="path_plot.pdf radar_plot.pdf plot_distance_530_590s.pdf plot_pathlength_530_590s.pdf plot_width.pdf plot_length.pdf plot_area.pdf speedVtime.pdf"

## check if any of these files are in the directory where new result files will be saved
for plot in $currentPlots
do
  if [ -f $plot ];
  then
	## if file is already in results folder, add its name to a variable
     alreadyExistingPlots="$alreadyExistingPlots$plot\n"    
  fi
done

## if there are already existing plots which will be overwritten 
##(as indicated by a non-empty $alreadyExistingPlots variable)
## Prompt the user, asking for either "yes" to continue with analysis
## or "cancel" to cancel the analysis.
if [[ !  -z  $alreadyExistingPlots  ]];
then
	echo -e "The following files are already saved in the given results folder:\n"
	echo -e $alreadyExistingPlots
	echo "Running the analysis will OVERWRITE these files."
	echo "Are you sure you want to continue?"
	
	## if user does not input yes or cancel, prompt them for input
	while [ ! "$input_variable" = "yes" -a ! "$input_variable" = "cancel" ]
	do
		echo "Input \"yes\" to continue (overwriting these files), or \"cancel\" to cancel analysis (with no quotations)."
		read input_variable
	done
	
	## if user inputs "cancel", exit the script.
	if [ "$input_variable" = "cancel" ]
	then
		exit
	fi
	## otherwise continue running analysis
fi

## Move back to project root directory
cd $initial_wd

## Move into folder where MWT data folders are saved
cd $1

## check if webdav server URL and path are both provided; if provided, zip files and move
## to webdav server.
## Otherwise, declare backed_up variable as false (used to notify user at end of script)
if [ -z $5 ] && [ -z $6 ];
then
backed_up=false
echo "Data was not backed up to webdav."
else
## Connect to webdav (so you can backup files)
## you will be prompted for your webdav username and password
mount_webdav -i  $5 /

## zip all MWT data folders in directory to be analyzed
for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done

## move .zip files to a webdav server
## Note - this is very slow...
mv *.zip $6
fi

## Set amount of memory to be devoted to running Choreography
export MWT_JAVA_OPTIONS=-Xmx$3g

## call choreography to analyze the MWT data (each folder within the specified directory)
for folder in */; do Chore --shadowless -p 0.027 -M 2 -t 20 -S -N all -o fDpesSlLwWaAmMkbPcdxyuvor1234 --plugin Reoutline::despike --plugin Respine --plugin MeasureReversal::all $folder; done

## need to create a large file containing all data files with 
## data, plate name and strain name in each row
##grep -r '[0-9]' $(find ./data -name '*.dat') > merged.file
for filename in $(find . -name '*.dat'); do grep -H '[0-9]' $filename >> merged.file; done

## return to MWT analysis root directory to call RScript
cd $initial_wd

## Use regular expressions in R to parse apart the information in the filepath
## so we can get data, plate ID and strain as delimited columns
## call R script with the command line using an argument for the filename we want to parse
## After data is parsed, figures are plotted and stats are done and saved in results 
## directory
rscript bin/Column_identification.R $1/merged.file $2 $4

## If script did not attempt to backup data to webdav, notify user at end of script
if [ $backed_up == false ]; then
	echo "Data was not backed up to webdav."
fi