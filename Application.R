#------------------------------------------------------------------------------------------#
# Prepare workspace
#------------------------------------------------------------------------------------------#

rm(list = ls())
# Load functions
Scripts <- c("R/Various_functions.R", "R/Plots.R")
sapply(Scripts, source)

# Load packages and input the data
Init_fctn()

#------------------------------------------------------------------------------------------#
# Run the tests for the integration order on the log transformed series
#------------------------------------------------------------------------------------------#

Lvl_fd_plot_fctn(Data = lvlData, logData = logData, aes_list = Plot_list)

# Price exhibits exponential trend in levels and heteroscedasticity in first differences --> 
# log transformation linearizes the trend in levels and produces a relatively homoscedastic series in first differences

# I do not think that Coal requires any transformation ??

#------------------------------------------------------------------------------------------#
# Run the tests for the integration order on the log transformed series
#------------------------------------------------------------------------------------------#

# Logged price series
logPrice_I_order <- Pantula_fctn(Data = logData, Variable = "Price", d_max = 4, determ = "trend")
# Going forward use first differences of the log price series

# Level coal series
Coal_I_order <- Pantula_fctn(Data = lvlData, Variable = "Coal", d_max = 4, determ = "trend")
# Use level coal series

# To check my function:
#order_integration(dplyr::select(logData, c(Coal, Price)), max_order = 3)$order_int

#------------------------------------------------------------------------------------------#
# Control for seasonality
#------------------------------------------------------------------------------------------#

# Coal series
SlvlData <- Deseason_fctn(Data = lvlData, Variable = "Coal")

# Make plots of seasonality and de-seasoned series
Seas_plot_fctn(Data = SlvlData, Variable = "Coal", aes_list = Plot_list)

# Price series
SlogData <- Deseason_fctn(Data = logData, Variable = "dPrice")

# Make plots of seasonality and de-seasoned series
Seas_plot_fctn(Data = SlogData, Variable = "dPrice", aes_list = Plot_list)

#------------------------------------------------------------------------------------------#
# Plot of the final two series we will be working with
#------------------------------------------------------------------------------------------#

finalData <- tibble(Date = lvlData$Date,
                    Price = SlogData$deseasdPrice,
                    Coal = SlvlData$deseasCoal) %>%
  filter(!is.na(Price))

Final_series_plot_fctn(Data = finalData, aes_list = Plot_list)

#------------------------------------------------------------------------------------------#
# Estimate a VAR
#------------------------------------------------------------------------------------------#

dataMat <- as.matrix(select(finalData, -Date))

Lag_selection <- VARselect(dataMat, lag.max = 5, type = "const")$selection
Lag_selection
p <- Lag_selection["SC(n)"]
# 4 AR lags
VAR_model <- VAR(dataMat, p = p)
VAR_summary <- VAR_model %>% summary
VAR_summary

#------------------------------------------------------------------------------------------#
# VAR diagnostics
#------------------------------------------------------------------------------------------#

VAR_resid <- resid(VAR_model)

# Check the integration order of the residuals
Resid_Price_I_order <- Pantula_fctn(VAR_resid, "Price", d_max = 4, determ = "none")
Resid_Coal_I_order <- Pantula_fctn(VAR_resid, "Coal", d_max = 4, determ = "none")
# Both residuals series are I(1) (assume the integration order of the dependent variables)

# Check for serial correlation in the residuals
# graphically (univariate)
ACF_plot_fctn(Data = VAR_resid, Variable = "Price", Interval = .95, aes_list = Plot_list)
ACF_plot_fctn(Data = VAR_resid, Variable = "Coal", Interval = .95, aes_list = Plot_list)
# and quantitatively (multivariate)
Resid_PT_test <- serial.test(VAR_model, lags.pt = 12)
Resid_PT_test <- serial.test(VAR_model, lags.bg = 12, type = c("BG"))
# Seems like residual serial correlation is driven by December observations of the price series 

# Check if the VAR is stable
VAR_summary$roots
any(VAR_summary$roots >= 1)
# VAR(4) is stable

#------------------------------------------------------------------------------------------#
# Granger causality analysis
#------------------------------------------------------------------------------------------#

bruceR::granger_causality(VAR_model, test = c("Chisq"))
# Coal does not Granger cause Price
# Price Granger causes Coal

#------------------------------------------------------------------------------------------#
# Impulse response functions
#------------------------------------------------------------------------------------------#

# ortho = TRUE identifies a SVAR via short term restrictions (Slides VAR p. 46). We would assume that
# Price contemporaneously affects Coal, but Coal ONLY affects Price with a lag of at least one period.
# If ortho = FALSE we use the reduced form errors for the IRF. But those are probably contemporaneously
# correlated, which pollutes the true Impulse responses.
# Due to the Granger causality findings and some economic theory I think we can motivate the recursive
# structure of ortho = TRUE

O_irf <- irf(VAR_model, n.ahead = 12, ortho = TRUE, runs = 500, ci = .95)

# Generalized impulse respones functions (Koop et al. 1996; Pesaran & Shin 1998) do not impose a recursive
# ordering of the contemporanous correlations
G_irf <- G_irf_boot_fctn(VAR_model, n.ahead = 12, runs = 100, Interval = .95)

#------------------------------------------------------------------------------------------#
# Run cointegration tests
#------------------------------------------------------------------------------------------#

# CI Test
Trace_test <- ca.jo(dataMat, type = "trace", ecdet = "const", spec = "transitory", K = max(p, 2))
Trace_test %>% summary()

Eigen_test <- ca.jo(dataMat, type = "eigen", ecdet = "const", spec = "transitory", K = max(p, 2))
Eigen_test %>% summary()

VECM <- cajorls(Trace_test, r = 1)
VECM
