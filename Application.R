print("Marie")
print("Arman")
# EU carbon permit prices are in logged differences to get I(1), they were I(2) before
#Coal are just logged since they were already I(1)
# log to stabilize variance (get rid of heteroskedasticity)



rm(list = ls())
# Load functions
Scripts <- c("Various_functions.R")
sapply(Scripts, source)
# Load packages and data
Init_fctn()


#Pantula principle
SecondDiffPrice <- diff(Data$Price,differences = 2)
FirstDiffPrice <- diff(Data$Price)

adf(SecondDiffPrice,deterministics="intercept")
adf(FirstDiffPrice,deterministics="intercept")
adf(Data$Price,deterministics="trend")

plot(Data$Price)
plot(SecondDiffPrice)

# Plot
Data %>%
  ggplot() +
  geom_line(aes(x = Date, y = Coal, color = "Log gross inland deliveries hard coal")) +
  geom_line(aes(x = Date, y = Price / max(Price) * max(Coal), color = "Log EU Carbon Permit price")) +
  scale_y_continuous(
    name = "Log gross inland gross coal deliveries in thou. T",
    sec.axis = sec_axis(~ . * (max(Data$Price) / max(Data$Coal)), name = "Log EU Carbon Permit Price")
  ) +
  labs(color = "")

# UR Test
UR_test <- order_integration(dplyr::select(Data, -Date), max_order = 3)
UR_test$order_int
# Difference price once
Data_dprice <- Data %>%
  mutate(dPrice = diff_mult(Price, d = 1)) %>%
  filter(!is.na(dPrice))

#-------------------------------------------------#
# Not really sure if both variables have to have the same integration order
# Maybe ask Ines tomorrow
#-------------------------------------------------#

# Plot 2
Data_dprice %>%
  ggplot() +
  geom_line(aes(x = Date, y = Coal, color = "Log gross inland deliveries hard coal")) +
  geom_line(aes(x = Date, y = dPrice / max(dPrice) * max(Coal), color = "EU Carbon Permit price")) +
  scale_y_continuous(
    name = "Gross inland gross coal deliveries in thou. T",
    sec.axis = sec_axis(~ . * (max(Data_dprice$dPrice) / max(Data_dprice$Coal)), name = "First differences Log EU Carbon Permit Price")
  ) +
  labs(color = "")

# CI Test
Reduced_mat <- dplyr::select(Data_dprice, c(dPrice, Coal)) %>%
  as.matrix()
p <- VARselect(Reduced_mat, lag.max = 5, type = "trend")$selection["SC(n)"]
Trace_test <- ca.jo(Reduced_mat, type="trace", ecdet="const", spec = "transitory", K = max(p, 2))
Trace_test %>% summary()

Eigen_test <- ca.jo(Reduced_mat, type="eigen", ecdet="const", spec = "transitory", K = max(p, 2))
Eigen_test %>% summary()

VECM <- cajorls(Trace_test, r = 1)
VECM
