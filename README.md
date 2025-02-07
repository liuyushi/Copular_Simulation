# Copular_Simulation
# Empirical-Simulation
Empirical Simulation for Complex Clinical Trial Data in Drug Development	

## Introduction

Welcome to the Empirical Simulation for Complex Clinical Trial Data (ESCCD) programs. This suite of tools is designed to simulate realistic multivariate clinical trial data of mixed types, leveraging historical data to achieve desired summary statistics. This "read me" file provides an overview of the methodology, installation instructions, and guidelines for usage.

## Abstract
Simulating realistic data for multivariate variables of mixed types is in high demand in clinical trial design, yet remains a challenge in clinical development. While it is difficult to model the multivariate data based on existing well-known probability distributions, leveraging existing historical individual data could be an attractive approach. Two methods are generally used. The first approach is to resample historical data and apply a linear transformation to match the required mean and standard deviation. The second approach uses weighted sampling of historical individual-patient data to create new samples with the desired mean. There are two drawbacks for the weighted sampling approach: the simulated data have the exact same range as the historical data, and it may be challenging to obtain the desired means simultaneously for multiple variables. This novel method proposes a flexible framework that can simulate realistic multivariate data of mixed data types (e.g., continuous, binary, count) based on historical data to have desired summary statistics including mean, standard deviation, minimum, and maximum. The new method will allow easy simulation of clinical trial data based on historical data at the design stage to optimize the study design and statistical analysis models.

## Usage Guidelines
The ESCCD programs offer a robust and flexible framework for simulating clinical trial data. Below are the primary functionalities and usage instructions:

### Loading Historical Data
Ensure that your historical data is properly formatted and cleaned before loading. The data should be in a wide format, with each row representing an individual patient and each column representing a variable.

### Choosing Simulation Parameters
Specify the desired summary statistics for your simulation, including mean, standard deviation, minimum, and maximum values for each variable. These summary statistics will guide the simulation process to get the optimal parameter that closely matches your requirements.

### Running the Simulation
Plug-in the optimal parameters to generate new samples based on the specified parameters.

### Output and Analysis
You can output the simulated dataset for further analysis.

## Disclaimer
The example dataset "example.csv" provided is for illustration purposes only. It may not replicate the results from the manuscript. For confidentiality reasons, the original clinical trial data are not publicly available.
