# DC Property Sale Price Analysis

An end-to-end machine learning pipeline analyzing sale prices across three DC property types: office buildings, apartment buildings, and single-family/row homes

## What this project does

- Builds hedonic pricing models (OLS and Random Forest) to estimate how building characteristics drive sale price
- Constructs a repeat-sales analysis to measure price appreciation over time, independent of property characteristics
- Runs ward-stratified models for single-family homes so price trends and elasticities can vary by neighborhood
- Measures assessment gaps using IAAO-standard COD and PRD statistics to identify where assessed values diverge from market prices
- Projects expected 2027 sale prices with probability ranges 

## Methods

OLS regression, Random Forest, Grouped k-fold cross-validation, Repeat-sales index, Hedonic price modeling, IAAO assessment ratio analysis

## Tools

R: tidyverse, randomForest, caret, rsample, ggplot2, broom, gt

## Data

Public property transaction and assessment records from the DC CAMA database and CoStar. 
Raw data files are not included in this repository.

