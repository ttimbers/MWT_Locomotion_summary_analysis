# Summary analysis of Locomotion experiments on the Multi-worm Tracker
The aim of this project is to create software that can be used after running
a 600s basal locomotion experiment using the Multi-worm tracker (Swierczek et al., 2011)
to generate summary figures and statistics for that experiment. 

This script also backs up experiment .zip files to a webdav server specified by the
user. 

## Figures it generates
* Speed versus time over experiment duration
* Radial and boxplot pathlength figures from 530-590s
* Spontaneous reversals/minute box plot (averaged over 470-590s)
* Body size box plots (area, length and width)

## Statistics reported
* Mean initial speed (averaged over 30-45s) for each strain, and ANCOVA comparing to wild-type
* Mean final speed (averaged over 575-590s) for each strain, and ANCOVA comparing to wild-type
* Mean pathlength measured over 530-590s for each strain, and ANCOVA comparing to wild-type
* Mean number of spontaneous reversals/minute (averaged over 470-590s) for each strain, and ANCOVA comparing to wild-type
* Mean area, length and width for each strain, and ANCOVA comparing to wild-type

## How to use it

### Installation Dependencies

`Java`, `R`, R packages `ggplot2`, `plyr`, `stringr`, `gridExtra`, and `asbio`, and the Multi-worm Tracker 
Analysis software (`Chore.jar`) as a shell script in the executable path named `Chore`. 
To "easily" do this on a Mac or Linux OS, please follow the following installation 
instructions:

#### For Mac OSX
1. Install Homebrew by typing the following into the command line:


	`ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`
2. Install the Multi-worm Tracker Analysis software via Homebrew to install Chore.jar and
have it accesible as a shell script in the executable path named "Chore":


	`brew install homebrew/science/multi-worm-tracker`


#### For Linux
1. Install Linuxbrew by typing the following into the command line:


	`ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/linuxbrew/go/install)"`
2. Put brew in your executable path by adding the commands below to either `.bashrc` or 
`.zshrc`: 

	~~~
	export PATH="$HOME/.linuxbrew/bin:$PATH"
	export MANPATH="$HOME/.linuxbrew/share/man:$MANPATH"
	export INFOPATH="$HOME/.linuxbrew/share/info:$INFOPATH"
	~~~
	
3. Install the Multi-worm Tracker Analysis software via Homebrew to install Chore.jar and
have it accesible as a shell script in the executable path named "Chore":


	`brew install homebrew/science/multi-worm-tracker`


### Running the analysis

* Set working directory to project's root directory

* Call `locomotion_driver.sh` from the `Bash Shell`

* `locomotion_driver.sh` requires the following arguments from the user: (1) webdav server 
URL, (2) path on webdav where `.zip` folders should be saved, and (3) the gigabytes of 
memory to be devoted to the process (recommended as much as you have). See example below:

`bash bin/locomotion_driver.sh https://webdav.server/location folder_to_backup_to 4`

#### This code is still a work in progress. More instructions to come as code is developed further.