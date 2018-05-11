Author: Nicole Keefner

General Description: Be able to read in 2 long-term monitoring datasets and produce an html file containing background information on the topic, data collection methods, and an analysis. These files are meant to be run through program R/R Studio.

README.md: File that describes the contents of the Benthic_Analysis directory.

benthicmeans_clean_all.csv: File that contains long-term monitoring information on percent cover of corals, substrate, and other benthic cover in the British Virgin Islands.

ORIGINAL_Guana_Sponge_data_for_analysis.csv: File that contains long-term monitoring information on sponge counts in the British Virgin Islands.

Raw_Data.R: File that contains the R code that is used in the R Markdown file. It reads in the benthicmeans_clean_all.csv and the ORIGINAL_Guana_Sponge_data_for_analysis.csv. This is useful for exploring the data and testing analyses prior to adding lines to the R Markdown file.

.Rhistory: File that stores commands from the Raw_Data.R after the user runs the script.

Benthic_Analysis.Rmd: File that uses the code drafted in the Raw_Data.R file to outline a report file- Evaluating the effects of space and time on sponge and coral communities in the British Virgin Islands.html. In order to produce the html file, this R Markdown file must be knitted to html. If a PDF is preferred, this file may also be knitted to PDF.

Evaluating the effects of space and time on sponge and coral communities in the British Virgin Islands.html: File that is produced by knitting the Benthic_Analysis.Rmd. This is the output of the analysis in the form of an academic report. When this file is created, the folder with the same name is also produced.

/Evaluating the effects of space and time on sponge and coral communities in the British Virgin Islands: Folder that contains a file ____. This folder is produced when the html file of the same name is produced.
