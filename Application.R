print("Marie")
print("Arman")
# gross inland deliveries are in logged differences to get I(1), they were I(2) before
#EU carbon permit prices are just logged since they were already I(1)
# log to stabilize variance (get rid of heteroskedasticity)



rm(list = ls())
# Load functions
Scripts <- c("Various_functions.R")
sapply(Scripts, source)
# Load packages and data
Init_fctn()
Data$Coal

#Pantula principle
SecondDiffCoal <- diff(Data$Coal,differences = 2)
FirstDiffCoal <- diff(Data$Coal)

adf(SecondDiffCoal,deterministics="intercept")
adf(FirstDiffCoal,deterministics="intercept")
adf(deterministics=
      "trend")

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
CI_test <- ca.jo(Reduced_mat, type="trace", ecdet="trend", K = 2)
CI_test %>% summary()
