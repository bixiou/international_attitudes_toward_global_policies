# Reproduction code for "International Majorities Genuinely Support Global Redistributive and Climate Policies" 

LICENSE: GNU AFFERO GENERAL PUBLIC LICENSE Version 3

DOI of the github repository: [![DOI](https://zenodo.org/badge/702713860.svg)](https://zenodo.org/doi/10.5281/zenodo.11202245)

# Authors:
- Adrien Fabre, CNRS and CIRED, adrien.fabre@cnrs.fr (corresponding author).
- Thomas Douenne, University of Amsterdam.
- Linus Mattauch, TU Berlin.

# Summary:
- The code is in R.
- The main file is code_global/3_paper.R: it enables researchers to reproduce all the empirical findings of the paper section by section, including figures and tables.
- This script will first run code_global/0_Rprofile.R. Then, it either (A) prepares the data by running code_global/1_relabel_rename.R and code_global/preparation.R; or (B) loads code_global/after_preparation.RData.
- We recommend users to create an R Project within code_global/.

# List of files
code_global/: All code, exclusively in R.
code_global/.RData: Final datasets.
code_global/0_Rprofile.R: First file to run in R: loads packages and defines custom functions.
code_global/1_relabel_rename.R: Called in preparation.R, defines the variable names.
code_global/2_preparation.R: Cleanses and prepares the dataset.
code_global/3_paper.R: Reproduces the paper.
code_global/conjoint_analysis: Reproduces the conjoint analysis.
code_global/map_GCS_incidence.R: Estimates the effects of international climate policies and plots the associated maps.
code_global/zipcodes.R: Summarizes relevant information on zipcodes from raw official data. Used to code the respondent's region in Qualtrics.

conjoint_analysis/: Files used to code the conjoint analysis in Qualtrics and analyze its results. Cf. code_global/conjoint_analysis.R for reproduction instructions.

data/: All data files, including codebooks.
data/EU.csv: Raw data from Complementary Survey EU.
data/US1.csv: Raw data from Complementary Survey US1.
data/US2.csv: Raw data from Complementary Survey US2.
data/fields: Data of open-ended fields converted to .xlsx and classified, with translations.
data/global_survey.RData: Cleaned data of the global survey (see https://github.com/bixiou/intl_climate_attitudes for code and raw data).

figures/: Figures generated from render.R.
figures/all: Figures on the merged sample.
figures/country_comparison: Figures comparing the results by country.
figures/[country_code/wave]: Figures on the sample restricted to [country/wave].
figures/OECD: Figures from the Global Survey.
figures/maps: Maps of net gains from international climate policies.

paper/: LaTeX and PDF files of the main article, online appendices and country-specific descriptive statistics, as well as the pre-registration plan.

questionnaire/: Questionnaire files, both in Qualtrics and MS Word format.
questionnaire/specificities.xlsx: File synthesizing all figures used in the questionnaires and their sources: sociodemographic quotas, wording of policies in different languages, income thresholds, wealth tax estimates, etc.
questionnaire/IRB_approval.pdf: UvA's IRB approval.

tables/: LaTeX tables exported from paper.R.

xlsx/: Export of the data underlying each figure.

# Data and Code Availability Statements
## Complementary Survey Data
Fully available. 
EU: data/EU.csv
US1: data/US1.csv
US2: data/US2.csv

## Global Survey Data
Fully available data, code, and figures.
URL: https://github.com/bixiou/intl_climate_attitudes


# Computational requirements
Software requirement: R.
The following software and language versions were used: RStudio 2024.04.1 748; R 4.3.1.
A particular version (0.99.22) of the R package "memisc" is needed. Similarly, a patched version of the R package "stargazer" is required. If another version of "memisc" or "stargazer" is installed, 0_Rprofile.R will automatically uninstall it and install the appropriate version.

# Reproduction time
Duration necessary to install all packages (on a laptop): 20 min.
Duration of full reproduction (on a laptop): 7 min.