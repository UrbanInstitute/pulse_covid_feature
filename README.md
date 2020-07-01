# Tracking COVID-19’s Effects by Race and Ethnicity
 
This repository contains the code and data needed to calculate estimates of
various COVID-19 impacts on US adults and their households by geography and
race/ethnicity as well as the corresponding standard errors and significance of
these estimates. The scripts read in data from the federal [Household Pulse
Survey public use
files](https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html)
to estimate COVID-19 imppacts on few selected variables for the US, all 50 states and the District of
Columbia, and the 15 largest metropolitan statistical areas (MSAs). The output
data files of this repo power Urban's interactive Tracking COVID-19’s Effects by
Race and Ethnicity data feature visualizing impacts of COVID-19 on US adults and
their households by geography and race/ethnicity. You can view the code for
creating the web application at this Github repo.

The output data (and data dictionaries) can also be accessed and downloaded on
the [Urban Data Catalog](https://datacatalog.urban.org/dataset/census-pulse-public-use-files).

These numbers are estimates and may not equal the actual totals in each
geography. We highly recommend interpreting these results as *relative impacts
of COVID-19*  which can be used to inform race-conscious solutions that account for the pandemic's
disparate impacts by race and ethnicity.

For more information, see the technical appendix from our interactive data feature.

## Required R Libraries:
- `tidyverse`
- `readxl`
- `testit`
- `tigris`
- `stringr`
- `httr`
- `here`
- `srvyr`
- `survey`
- `fastDummies`
- `aws.s3`

## Directory Structure:
- `scripts/` stores the relevant scripts
    - `01_generate_weekly_puf_data.R`: Downloads Pulse Survey public use files, creates race/ethnicity variable and indicator variables, and appends replicate weights. The data produced from this file is stored on Urban's Data Portal.
    - `02_generate_group_vs_avg_standard_errors.R`: Calculates means and standard errors for each indicator/geography/week/race combination and significance of difference between each population and subgroup mean. Outputs data file that powers the Tracking COVID-19’s Effects by Race and Ethnicity feature. 
    - `03_upload_to_s3.R`: Uploads data and metadata to AWS S3 bucket.
    - `04_perform_qc_checks.R`: Conducts quality control checks on the results output from scripts 02 and 03. 
- `data/` stores the data
    - `raw-data/`: stores the raw Pulse Survey public use files including the data, replicate weights, and data dictionary.
    - `intermediate-data/`: stores the processed public use data and the intermediate outputs of the standard error calculations.
    - `final-data/`: stores the final data file produced by `02_generate_group_vs_avg_standard_errors.R` that powers the data feature.

## Contact
Contact Alena Stern at astern [at] urban.org or Ajjit Narayanan at anarayanan [at] urban.org

