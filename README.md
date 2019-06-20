# flexible-bart
Code to support the analysis in "Flexible modeling of the causal effect of ozone pollution on violent crime with BART"

## Data files

### Crime_data.R

Data used for the analysis in a data frame structure with each row representing a single day in one of 10 cities:

- Outcome: aggrevated assault, simple assault, larceny and auto theft counts and logged rates

- Exposure: dichotomized (dicho.oz) and continuous ozone (max.oz) concentrations 

- Confounders: Maximum temperature, PM concentration, UV Index, date, holiday and weekend indicators

### Dog_data.R

Data for the sensitivity analysis on ozone concentration and dog bites. Same structure as Crime_data, though the only outcome investigated is number of reported dog bites. 

## Analysis files

### Crime_analysis.R

Code for the primary analysis described in Section 3, as well as sensitivity analyses []. 

### Dog_analysis.R

Code for sensitivity analysis []. 


