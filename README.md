# Simple Rshiny tool for mass spectroscopy analysis 

### Author: Boris Grinshpun ###
### Written for a friend in 2019 ###

Given a list of individual elements and their molecular weights this script uses a dynamic programming algorithm (the Coin Change problem) to produce all possible combinations that will yield the desired total molecular weight.

The tool can also accept restrictions on the minimum and maximum number of an element to exclude undesired combinations.

Finally, this tool accepts an error tolerance to consider total sums within a desired range. NOTE: This part of the tool can produce a large number of combinations leading to long run times. Use carefully.


INITIAL SETUP:
* Step 1: Open the file mass_spec_analysis.R in Rstudio
* Step 2: Edit line 11 of the file, setwd('path/to/files') and change the filepath to the location of your file

\* Make sure all files are in the same directory

----------------------------------------------
TO RUN:
Click Run App in the top right of the Rstudio window

----------------------------------------------
INPUT FILES:

     defaults.csv
         elementsfile: name of file containing information about elements
         sum: default sum when app launches
         tolerance: default tolerance when app launches

     sampleinput.csv: current name of elementsfile. Can be edited as desired, or create your own and edit defaults.csv with the name.
          Column1: Name of the element/group
          Column2: Corresponding molecular weight
          Column3: Minimum count allowed
          Column4: Maximum count allowed
          * If no minimum or maximum, use NA.
          * Use of minimums/maximums speeds up calculation. Calculation times can get long depending on the setup.

