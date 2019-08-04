# flexible-bart
Code to support the analysis in "Flexible modeling of the causal effect of ozone pollution on violent crime with BART"

## Data files

### Crime_data.R

Data used for the analysis in a data frame structure with each row representing a single day in one of 10 cities:

- Outcome: aggrevated assault, simple assault, larceny and auto theft counts and logged rates

- Exposure: dichotomized (dicho.oz) and continuous ozone (max.oz) concentrations 

- Confounders: Maximum temperature, PM concentration, UV Index, date, holiday and weekend indicators, date and city indicators

### Dog_data.R

Data for the sensitivity analysis on ozone concentration and dog bites. Same structure as Crime_data, though the only outcome investigated is number of reported dog bites. 

## Analysis files

### Primary_analysis.R

Code for the primary analysis (3.2) with a binary exposure as well as sensitivity analyses (3.5 and 3.6). 

### Secondary_analysis.R

Code for the secondary analysis (3.3) with a continuous exposure (estimating a dose-response relationship) as well as sensitivity analyses (3.6 and 3.7). 

### Dog_analysis.R

Code for sensitivity analysis with dog-bite outcome data (3.6).

## Figure code

### Tables_code.R

### Primary_figures_code.R

### Dose_response_code.R

### Misc_figures_code.R


