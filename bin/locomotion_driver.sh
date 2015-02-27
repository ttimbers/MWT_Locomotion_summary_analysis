##This is the driver script that will call modulars scripts to attack each chunk
##of the problem
##
##Set working directory to project's root directory

##zip all folders all MWT data folders in directory to be analyzed
##must be in "data" working directory 
cd data
for foldername in *; do cd $foldername; zip ../$foldername *; cd ..; done
cd ..

##optional challenge - copy them to server
##Tiff

##destroy/delete unzipped MWT folder
##Kwangjin

##call choreography to analyze the MWT data folders
##Tiff

##move unzipped folder into a new directory (chore_data)
##Catrina

##need to create a large file containing all data files with 
##data, plate name and strain name in each row
##Catrina

##create figures

##create stats
