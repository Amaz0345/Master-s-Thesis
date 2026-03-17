# The scripts can be run in order, as they are numbered, except 0 is workable only with full access to the Women's, Men's and Household (PR) # Datasets of DHS 2019-20. Data is hosted on GWDG OwnCloud. Download manually from: https://owncloud.gwdg.de/index.php/s/q11668N0KOUsjUK
#
# Save all downloaded files into your /data folder:
# project_root/data/
#
# The following files are expected:
expected_files <- c(
  "IAPR74DT.dta",   # PR data  
  "IAIR74DT.dta",   # individual recode (women)
  "IAMR74DT.dta",   # men's recode
  "data_2019.dta"   # cleaned dataset
)
# Alternatively, since the files are quite large, you can download only data_2019 and run the scripts from 01.setup onwards. 

# For script 05.Figure4, there are essentially 3 parts to the code. The first part calls and constructs the shapefile for India from a GitHub repository. The second part recalls district names and codes from the PR Data of the DHS 2019-20. This second part need not be run if you do not have access to the PR Data, and can be run after the creation of treatment_lookup.rds. Once you open treatment_lookup.rds in R, you can run the rest (part 3) of the code. 

# For the Appendix, it is most compatible with Overleaf. I did not have LaTeX on my laptop version of RStudio, so I preferred to work on the Appendix there. I used LaTeX instead of Markdown or exporting to Word like I did the main text results mainly so that I could learn how it is used.
Also, the balance table takes a while to run. May depend on the processing power of your computer. It is not an error-- it just takes time. I would suggest a tea/coffee break while you wait for it to load.  