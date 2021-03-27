# Does VIE affect survival or behaviour of termites?

R code for analysis of survival and behavioural data from a study of the effect of the visible implant elastomer on survival and behaviour of termites.

## Survival analysis
Data were collected every day for 28 days, then weekly for a further 5 weeks. Data record the number of deaths (and survivors) in each housing 'dish' on each census day. Data were reformatted for analysis to show the number of days termites survived and to include 'status' - 1, dead; 0, alive - and whether data were censored - 1, censored; 0, not censored.

Six packages are used (loaded in the 'Preamble' section):
* survival - for log-rank tests,
* ggplot2 - for plots,
* coxme - for mixed effect Cox proportional hazards test.
* powerSurvEpi - for power analysis.
* coxed - for simulations.

The 'Basic plots' section creates plots of raw data: density plots and histograms for each group (control and treatment) separately; mean number alive in each dish on each census day for both groups.

The 'Log-rank test' section plots Kaplan-Meier survival curves and includes a log-rank test to determine if there is a statistically significant difference between the survival curves for the control and treatment groups.

The 'Mixed effect Cox prop. hazards' section contains code for a mixed effects Cox proportional hazards model with the data.

The 'Short-term effects' section has code for log-rank and Cox models for data between day 1 and day 10 of the census.

The 'Power' section has code for post-hoc power analysis that uses the results of the Cox model. This is done in two ways: 1) analytically, using the ```powerCT.default()``` function; 2) with simulations, using the ```sim.survdata()``` function.

## Behaviour analysis
