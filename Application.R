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

Lvl_fd_plot_fctn(Data = lvlData, logData = logData, aes_list = Plot_list, Plot_path = Plot_path)

# Price exhibits exponential trend in levels and heteroscedasticity in first differences --> 
# log transformation linearizes the trend in levels and produces a relatively homoscedastic series in first differences

# I do not think that Coal requires any transformation ??

#------------------------------------------------------------------------------------------#
# Run the tests for the integration order on the log transformed series
#------------------------------------------------------------------------------------------#

# Logged price series
Pantula_fctn(Series = select(logData, Price), d_max = 4, determ = "trend")
# Going forward use first differences of the log price series

# Level coal series
Pantula_fctn(Series = select(lvlData, Coal), d_max = 4, determ = "trend")
# Use level coal series

# To check my function:
#order_integration(dplyr::select(logData, c(Coal, Price)), max_order = 3)$order_int


#------------------------------------------------------------------------------------------#
# Check for seasonality
#------------------------------------------------------------------------------------------#

# Coal series
Coal_decomposed <- decompose(ts(lvlData$Coal, frequency = 12))

# Seasonal component
ggplot() +
  geom_line(aes(x = lvlData$Date, y = as.numeric(Coal_decomposed$seasonal)))
# Rest
ggplot() +
  geom_line(aes(x = lvlData$Date, y = as.numeric(Coal_decomposed$x - Coal_decomposed$seasonal)))

finalData <- tibble(Date = lvlData$Date,
                     Coal = Coal_decomposed$x - Coal_decomposed$seasonal)


# Price series
Price_decomposed <- decompose(ts(logData$dPrice, frequency = 12))

# Seasonal component
ggplot() +
  geom_line(aes(x = lvlData$Date, y = as.numeric(Price_decomposed$seasonal)))
# Rest
ggplot() +
  geom_line(aes(x = lvlData$Date, y = as.numeric(Price_decomposed$x - Price_decomposed$seasonal)))

finalData$Price <- Price_decomposed$x - Price_decomposed$seasonal
finalData <- filter(finalData, !is.na(Price))

#------------------------------------------------------------------------------------------#
# Estimate a VAR
#------------------------------------------------------------------------------------------#
finalData <- tibble(Date = lvlData$Date,
                    Price = logData$dPrice,
                    Coal = logData$Coal) %>%
  filter(!is.na(Price))
dataMat <- as.matrix(select(finalData, -Date))

p <- Lag_order_fctn(dataMat, deterministic = "none")
VAR_model <- VAR(dataMat, p = p)
VAR_model %>% summary

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
