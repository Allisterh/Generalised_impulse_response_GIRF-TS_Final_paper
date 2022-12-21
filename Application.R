#------------------------------------------------------------------------------------------#
# Prepare workspace
#------------------------------------------------------------------------------------------#

rm(list = ls())
# Load functions
Scripts <- paste0("R/", list.files(path = paste0(getwd(), "/R")))
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

# Log price series
logPrice_I_order <- Pantula_fctn(Data = logData, Variable = "Price", d_max = 3, determ = "trend")
UR_table_fctn(Output = logPrice_I_order, Variable = "Price", Path = Table_path)
# Going forward use first differences of the log price series

# Level coal series
Coal_I_order <- Pantula_fctn(Data = lvlData, Variable = "Coal", d_max = 3, determ = "trend")
UR_table_fctn(Output = Coal_I_order, Variable = "Coal", Path = Table_path)
# Use level coal series

# To check my function:
# order_integration(dplyr::select(logData, c(Coal, Price)), max_order = 3)$order_int

#------------------------------------------------------------------------------------------#
# Control for seasonality
#------------------------------------------------------------------------------------------#

# Coal series
SlvlData <- Deseason_fctn(Data = logData, Variable = "Coal")
# Make plots of seasonality and de-seasoned series
Seas_plot_fctn(Data = SlvlData, Variable = "Coal", aes_list = Plot_list)

# Price series
SlogData <- Deseason_fctn(Data = logData, Variable = "Price")
# Make plots of seasonality and de-seasoned series
Seas_plot_fctn(Data = SlogData, Variable = "Price", aes_list = Plot_list)

#------------------------------------------------------------------------------------------#
# Plot of the final two series we will be working with
#------------------------------------------------------------------------------------------#

finalData <- tibble(
  Date = lvlData$Date,
  Price = SlogData$deseasPrice,
  Coal = SlvlData$deseasCoal
) %>%
  filter(!is.na(Price)) %>%
  mutate(Price = (Price - mean(Price)) * 100)

order_integration(select(finalData, -Date))$order_int

# Construct plot and a table of summary statistics
Final_series_plot_fctn(Data = finalData, aes_list = Plot_list)
Summary_stats_fctn(Data = finalData, Path = Table_path)

#------------------------------------------------------------------------------------------#
# Estimate a VAR
#------------------------------------------------------------------------------------------#

dataMat <- finalData %>%
  select(-Date) %>%
  as.matrix()

dataMat_Stat <- as_tibble(dataMat) %>%
  mutate_all(diff_mult, 1) %>%
  filter(!is.na(Price)) %>%
  as.matrix()

Lag_selection <- VARselect(dataMat_Stat, lag.max = 5, type = "none")$selection
Lag_selection
p <- Lag_selection["SC(n)"]
# 3 AR lags
VAR_model <- VAR(dataMat_Stat, p = p, type = "none")
VAR_summary <- VAR_model %>% summary()
VAR_summary

VAR_table_fctn(Output = VAR_summary, Path = Table_path)

#------------------------------------------------------------------------------------------#
# VAR diagnostics
#------------------------------------------------------------------------------------------#

VAR_resid <- resid(VAR_model)
# Plot of VAR residuals
Resid_plot_fctn(Data_tib = finalData, Resid = VAR_resid, aes_list = Plot_list)

# Check the integration order of the residuals
Resid_Price_I_order <- Pantula_fctn(VAR_resid, "Price", d_max = 4, determ = "none")
UR_table_fctn(Output = Resid_Price_I_order, Variable = "Price_Resid", Path = Table_path)
Resid_Coal_I_order <- Pantula_fctn(VAR_resid, "Coal", d_max = 4, determ = "none")
UR_table_fctn(Output = Resid_Coal_I_order, Variable = "Coal_Resid", Path = Table_path)
# Only the residual series is I(1) (assumes the integration order of the dependent variables)
# Interestingly, the residuals for coal appear stationary

# Check for serial correlation in the residuals
# graphically (univariate)
ACF_plot_fctn(Data = VAR_resid, Variable = "Price", Interval = .95, aes_list = Plot_list, Prefix = "VAR")
ACF_plot_fctn(Data = VAR_resid, Variable = "Coal", Interval = .95, aes_list = Plot_list, Prefix = "VAR")
# and quantitatively (multivariate)
Resid_PT_test <- serial.test(VAR_model, lags.pt = 12)
Serial_test_table_fcnt(Output = Resid_PT_test, Path = Table_path)
Resid_BG_test <- serial.test(VAR_model, lags.bg = 12, type = c("BG"))
Serial_test_table_fcnt(Output = Resid_BG_test, Path = Table_path)
# Both tests reject serial AC in the residuals

# Check if the VAR is stable
VAR_summary$roots
any(VAR_summary$roots >= 1)
# VAR(4) is stable

#------------------------------------------------------------------------------------------#
# Granger causality analysis
#------------------------------------------------------------------------------------------#

Granger_test <- bruceR::granger_causality(VAR_model, test = c("Chisq"))
Granger_table_fctn(Output = Granger_test, Path = Table_path)
# Coal does not Granger cause Price
# Price does not Granger cause Coal

#------------------------------------------------------------------------------------------#
# Impulse response functions
#------------------------------------------------------------------------------------------#

# ortho = TRUE identifies a SVAR via short term restrictions (Slides VAR p. 46). We would assume that
# Price contemporaneously affects Coal, but Coal ONLY affects Price with a lag of at least one period.
# If ortho = FALSE we use the reduced form errors for the IRF. But those are probably contemporaneously
# correlated, which pollutes the true Impulse responses.

O_irf <- irf(VAR_model, n.ahead = 12, ortho = TRUE, runs = 500, ci = .95)
IRF_plot_fcnt(IRF = O_irf, aes_list = Plot_list)
# Generalized impulse respones functions (Koop et al. 1996; Pesaran & Shin 1998) do not impose a recursive
# ordering of the contemporanous correlations
G_irf <- G_irf_boot_fctn(VAR_model, n.ahead = 12, runs = 500, Interval = .95)
IRF_plot_fcnt(IRF = G_irf, aes_list = Plot_list)

#------------------------------------------------------------------------------------------#
# Run cointegration tests
#------------------------------------------------------------------------------------------#

# CI Test
# Only INTERCEPT in the test regression since we are dealing with first differences
Trace_test <- ca.jo(dataMat, type = "trace", ecdet = "const", spec = "transitory", K = max(p, 2))
Trace_test %>% summary()
# Max Eigevalue test
Eigen_test <- ca.jo(dataMat, type = "eigen", ecdet = "const", spec = "transitory", K = max(p, 2))
Eigen_test %>% summary()

# Estimate the VECM
VECM_Model <- cajorls(Trace_test, r = 1)
VECM_Model %>% summary()
VECM_table_fctn(Trace = Trace_test, Output = VECM_Model, path = Table_path)

# Graphically check for serial correlation in the residuals
VECM_resid <- VECM_Model$rlm$residuals
colnames(VECM_resid) <- c("Price", "Coal")
ACF_plot_fctn(Data = VECM_resid, Variable = "Price", Interval = .95, aes_list = Plot_list, Prefix = "VECM")
ACF_plot_fctn(Data = VECM_resid, Variable = "Coal", Interval = .95, aes_list = Plot_list, Prefix = "VECM")
