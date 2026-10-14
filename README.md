# Weather Derivatives Risk Management in R

## Project Overview

This project was developed as part of a quantitative risk management course.

The goal was to analyze and price a portfolio of weather derivatives and to assess the resulting market risk using different quantitative methods.

The portfolio contains several capped weather derivatives, including:

- HDD Put
- Frost Day Call
- Snow Call
- Rainfall Put
- Dry Days Put

The analysis combines derivative pricing, tail risk measures, Monte Carlo simulation and portfolio diversification.

## Main Topics

The project covers:

- payoff modelling for capped weather derivatives
- lognormal modelling of non-negative weather indices
- expected payout calculations
- Value-at-Risk (VaR)
- Tail Value-at-Risk / Expected Shortfall
- Monte Carlo simulation
- correlated risk factors
- Cholesky decomposition
- portfolio diversification
- risk contribution analysis
- risk-based pricing

## HDD Put Analysis

The first part of the project focuses on an HDD Put.

The contract pays when the Heating Degree Day index falls below the strike level, representing a relatively warm winter. The payoff is capped to limit the maximum exposure.

The analysis includes:

- the payoff and profit profile of the contract
- the distribution of the HDD index
- expected payout
- VaR
- TVaR / Expected Shortfall
- a risk-based offer price using a return-on-capital approach

## Weather Index Modelling

The weather indices are modelled using lognormal distributions because the indices are non-negative.

For each contract, the analysis considers:

- mean and standard deviation of the underlying index
- strike level
- payoff direction
- cap level
- probability that the cap is reached

The model is used to assess how likely extreme payout scenarios are for each derivative.

## Monte Carlo Portfolio Risk

The portfolio risk is estimated using Monte Carlo simulation in R.

The simulation:

1. defines the distribution parameters for the weather indices
2. incorporates the correlation structure between the indices
3. generates correlated random variables using a Cholesky decomposition
4. transforms the simulated variables into lognormal weather indices
5. calculates the payoff of each derivative under each scenario
6. aggregates the individual contract results into a portfolio loss distribution

From the simulated loss distribution, portfolio risk measures such as VaR and TVaR are calculated.

## Portfolio Diversification

A key part of the project is the comparison between standalone risk and portfolio risk.

The different contracts are exposed to different weather drivers, including temperature, precipitation and snowfall. Because these risk factors are not perfectly correlated, combining the contracts can reduce the overall portfolio risk.

The analysis therefore illustrates how diversification affects:

- portfolio VaR
- risk contributions
- reservation prices
- expected profitability

## Risk Allocation

The broader assignment also included the allocation of portfolio VaR to individual contracts.

The objective was to estimate how much each derivative contributes to total portfolio risk and to use these contributions for portfolio-based pricing.

This allows the pricing of individual contracts to reflect their incremental contribution to overall risk rather than only their standalone risk.

## Technologies

- R
- ggplot2
- Matrix
- Monte Carlo simulation
- Cholesky decomposition
- lognormal distributions
- VaR and Expected Shortfall
- derivative payoff modelling

## Repository Structure

```text
weather-derivatives-risk-management/
├── README.md
├── src/
│   └── weather_derivatives_risk.R
└── report/
    └── risk_management_assignment_2.pdf
