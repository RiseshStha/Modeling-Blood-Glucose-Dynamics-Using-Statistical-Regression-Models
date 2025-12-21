# Modeling-Blood-Glucose-Dynamics-Using-Statistical-Regression-Models
This repository contains R code for modeling short-term blood glucose dynamics using linear regression, generalized additive models (GAM), and nonlinear regression. The study focuses on interpretability and statistical evaluation in a health data context.

## Repository Structure
- scripts/ : R scripts for preprocessing, modeling, and evaluation
- exploration/ : R Markdown files used for exploratory analysis
- figures/ : Final figures used in the manuscript
- tables/ : Result tables

- ## Methods
We compare three statistical models:
- Linear Regression (baseline)
- Generalized Additive Models (GAM)
- Nonlinear Regression

Models are evaluated using time-aware train–test splits and standard error metrics
(RMSE, MAE).


## Data Availability
The dataset used in this study was provided by the university for academic purposes
and contains anonymized and aggregated health features.
Due to data-sharing restrictions, the raw data cannot be publicly released.


## Reproducibility
All analyses were conducted in R.
Package versions and session information are provided to support reproducibility.


## Disclaimer
This code is intended for research and educational purposes only and is not intended
for clinical decision-making or medical use.

## License
This project is licensed under the MIT License.


