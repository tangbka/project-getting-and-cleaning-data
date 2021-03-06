### Course Project - Getting and Cleaning Data
This repo contains the script to clean up and create a tidy data set as part of the Coursera course Getting &amp; Cleaning Data.


It contains the following files:

#####1) README.md
This file provides an overview and background of this repo.

#####2) run_analysis.R 
Script to be run in R that would create a tidy data set extracted from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
The steps I took to extract the data I need and create a tidy data set is also chronicled in this script.

#####3) CodeBook.md
This file explains the variables in the raw (and processed) data set as well as the data.

#####4) TidyData.txt
This is the cleaned data. You should see this file located in the Coursera webpage under "Course Project", if you happen to evaluate my assignment. If you follow the steps in the <u>CodeBook.md</u> and run the scrip <u>run_analysis.R</u> in R, you should obtain the same data set.


######What You Should Do?
<p>Start by reading the steps and meaning of the variables in <b>(3) CodeBook.md</b>. Then run the script <b>(4) run_analysis.R</b> in R. </p>

To load the <b>TidyData.txt</b> into R, do the following
a) Download the file onto your working directory.
b) tidy <- read.table("./TidyData.txt")
c) tidy
