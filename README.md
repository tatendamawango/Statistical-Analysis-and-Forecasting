# Statistical Analysis and Forecasting

This project covers various statistical analysis techniques and forecasting methods using R and RMarkdown notebooks, CSV datasets, and visualizations. The focus is on both descriptive and predictive analytics using statistical modeling.

## Topics Covered

- **Intro and Historical Context**
- **Matrix Algebra**
- **Computational Algorithms**
- **Principal Component Analysis (PCA)**
- **Test of Significance**
- **Measuring and Testing Multivariate Distances**
- **Statistical Approach to Problem Solving**
- **Factor Analysis**
- **Clustering**
- **Time Series Analysis and Forecasting (ARIMA, ETS, Naive, RW Drift)**

## Key Components

### 1. Data Files

- `gold_price_data.csv`, `friendship_data.csv`, `sample9.csv`, etc. contain real-world and simulated data used in analysis.
- `Bird_data.xlsx`, `Employment DATA With EU.xlsx` are external datasets analyzed using R.

### 2. R Scripts

- `project.R`: Full project pipeline for gold price analysis including plotting, decomposition, forecasting, and evaluation.
- `arma.R`, `expsmooth.R`, `decompose_stl.R`: Time series decomposition and smoothing.
- `Network analysis.R`, `Exploratory Data Analysis.R`: Insights on data relationships and EDA.
- `tslm_*`: Time series linear model exploration with dynamic, trend, seasonal, and exogenous components.

### 3. Jupyter Notebook

- `Analysis of EU Employment.ipynb`: Combines R and visual insights into employment trends across the EU.

## Project Highlights

- Forecast accuracy is evaluated through multiple models (ARIMA, ETS, Naive, Custom Weighted).
- RMSE analysis across different forecast horizons.
- Custom hybrid model combining ARIMA and ETS.
- Visualizations of ACF, PACF, decomposed series, histogram of values, and more.

## Libraries Used

- `ggplot2`, `forecast`, `moments`, `tidyr`, `readr`, `lubridate`, `dplyr`


