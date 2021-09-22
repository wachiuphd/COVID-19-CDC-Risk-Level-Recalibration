# COVID-19-CDC-Risk-Level-Recalibration

* All function scripts are in the Functions folder.
* Data files are in the Data folder
* Results are in the Results folder

## Upload CovidActNow data using API, process and clean data

* Run the script update_CovidActNow_data.R -- this creates a few reference files for fips codes, as well as saving the test positivity data in an Rdata file "TestPositivity.Rdata" in the Data folder
* Run the script upddate_prevalence_incidence.R -- this processes the CDC risk levels, as well as calculating prevalence and incidence.  It then cleans the data by removing data with NA for cases or positivity, then removing data with non-positive values for cases or positivity.  It saves the full dataset in the Prevalence_Incidence_all.Rdata file, and the cleaned data in the Prevalence_Incidence_cleaned.Rdata file.  This latter file is used for all analyses
* The script load_data.R loads the data files saved previously

## Analysis and Results

### IU breakpoints best corresponding to CDC risk levels

* Probit_Ordered_Regression.R -- this runs the regression and saves the breakpoints to the Results folder in the file CDC_probit_breakpoints.csv
* calibrate_data.R -- creates the "true" values for IU and loads the IU breakpoints
* plotconfusion.R -- this is a function to plot the confusion matrix

### CDC category errors

* CDC_category_errors.R -- this file analyzes the results in terms of the performance of the CDC risk levels in predicting prevalence IU.  It outputs several plots of the confusion matrices, summary statistics of performance, as well as Figure 1A.  All output files (except Rdata file for Fig1A) start with "CDC_Category".

### Recalibrated breakpoints 

* Probit_Reclibration.R -- this runs the regression to find recalibrated break points for cases and positivity. Note that "Moderate" and "Substantial" categories for IU are combined.  Puts the results in the file Rec_probit_breakpoints.csv in the Results folder

### Recalibration category errors

* recalibrate_data.R -- script that creates the modified CDC risk levels with Moderate and Substantial combined, as well as the true and predicted recalibrated levels (run from other scripts)
* CDC_Mod_category_errors.R -- this file analyzes the results in terms of performance of the CDC risk levels, modified to combine "Moderate" and "Substantial", in predicting levels of prevalence IU.   It outputs several plots of the confusion matrices, summary statistics of performance, as well as onbe panel of Figure 1B.  All output files (except Rdata file for part of Fig1B) start with "CDC_Category_Mod".  Note that Appendix figure A1 is the file "CDC_Category_Mod_errors_density"
* Recalibration_errors.R -- this file analyzes the results in terms of performance of the recalibrated risk levelsrisk levels, modified to combine "Moderate" and "Substantial", in predicting levels of prevalence IU.   It outputs several plots of the confusion matrices, summary statistics of performance, as well as the complete Figure 1.  All output files (except Rdata file for part of Fig1B and the full Fig1) start with "Rec_Category".

