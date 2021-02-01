

This is the R code for the analysis in Berg and McColl (2021), "No projected global drylands expansion under greenhouse warming".

This code is archived here publicly for reproducibility purposes. Please feel free to use, but not modify. Please contact Alexis Berg (alexis_berg@fas.harvard.edu; alberg82@gmail.com) for any issues/questions.

This study is based on CMIP5 and ISIMIP modeling results, which the scripts in this repository allow to read and plot. 

THe main directory contains some routines to read CMIP5 data for the Historical and RCP8.5 runs, calculate mean climate variables, and plot all figures in the paper except Figures 4, 6, S4 and S5.

The directory RAD_PHYS contains files to read and plot data from the CMIP5 "1%" experiments, that is, 1pctCO2, esmFdbk1 and esmFixclim1 (renamed CTL, RAD and pHYS in the paper, respectively).

THe directory ISIMIP contains files to read and plot data from the ISIMIP offline experiments with Dynamic Global Vegetation Models forced by climate model outputs, with and without CO2 change after 2005. 

For clarity these three ensembles of scripts should be runs in different workspace. 

The scripts reading the model ouputs sometimes point to data stored at the LDEO climate library, which thus should be accessible online, and sometimes on data stored locally - in the latter case, you'll have to download the data yourself first (e.g., from ESGF) and modify the paths accordingly. 



