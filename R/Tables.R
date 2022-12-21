#' Function that creates a table of summary statistics
#' @param Data tibble with the data
#' @param Path to where the table should be stored
#' @export Table in specified folder

Summary_stats_fctn <- function(Data, Path) {
  Summary_Stats <- select(Data, -Date) %>%
    summarytools::descr(.,
      transpose = F, stats = c("n.valid", "Mean", "sd", "min", "q1", "med", "q3", "max")
    )
  Summary_Stats_tib <- as_tibble(round(Summary_Stats, 3))
  Summary_Stats_tib$Parameters <- rownames(Summary_Stats)
  write.xlsx(Summary_Stats_tib, file = paste0(Path, "Summary_Stats.xlsx"), overwrite = TRUE)
}


#' Function that creates a table of the output of an Unit Root test
#' @param Output matrix with the test output
#' @param Variable name
#' @param Path to where the table should be stored
#' @export Table in specified folder

UR_table_fctn <- function(Output, Variable, Path) {
  write.xlsx(as_tibble(Output), file = paste0(Path, Variable, "_UR_Test.xlsx"), overwrite = TRUE)
}


#' Function that creates a table of the VAR estimates
#' @param Output from vars::Var
#' @param Path to where the table should be stored
#' @export Table in specified folder

VAR_table_fctn <- function(Output, Path) {
  # Coefficients
  Price_coef <- Output$varresult$Price$coefficients
  Price_coef <- cbind(rownames(Output$varresult$Price$coefficients), round(Price_coef, 3))
  Coal_coef <- Output$varresult$Coal$coefficients
  Coal_coef <- cbind(rownames(Output$varresult$Coal$coefficients), round(Coal_coef, 3))
  # Likelihood
  logLik <- round(Output$logLik, 3)
  # F statistics
  Price_F <- Output$varresult$Price$fstatistic
  Price_F <- rbind(rownames(Price_F), round(Price_F, 3))
  Coal_F <- Output$va$Coal$fstatistic
  Coal_F <- rbind(rownames(Coal_F), round(Coal_F, 3))
  # Residual Sd.dev
  Sigma <- round(diag(sqrt(Output$covres)), 3)
  Sigma <- rbind(colnames(Sigma), Sigma)
  # Construct a .xlsx file
  VAR_estimates <- createWorkbook()
  addWorksheet(VAR_estimates, "Price_coef")
  addWorksheet(VAR_estimates, "Coal_coef")
  addWorksheet(VAR_estimates, "logLik")
  addWorksheet(VAR_estimates, "Price_F")
  addWorksheet(VAR_estimates, "Coal_F")
  addWorksheet(VAR_estimates, "Sigma")
  writeData(VAR_estimates, "Price_coef", Price_coef)
  writeData(VAR_estimates, "Coal_coef", Coal_coef)
  writeData(VAR_estimates, "logLik", logLik)
  writeData(VAR_estimates, "Price_F", Price_F)
  writeData(VAR_estimates, "Coal_F", Coal_F)
  writeData(VAR_estimates, "Sigma", Sigma)
  saveWorkbook(VAR_estimates, file = paste0(Path, "VAR_estimates.xlsx"), overwrite = TRUE)
}


#' Function that creates a table displaying the Wald test for Granger causality
#' @param Output from bruceR::granger_causality
#' @param Path to where the table should be stored
#' @export Table in specified folder

Granger_table_fctn <- function(Output, Path) {
  # Construct the table
  Table <- Output$result %>%
    filter(Excluded != "ALL") %>%
    select(-Causality) %>%
    rename(
      "Response" = Equation,
      "Impulse" = Excluded
    )
  # Store
  write.xlsx(Table, file = paste0(Path, "Granger_Causality_Test.xlsx"), overwrite = TRUE)
}


#' Function that creates a table for the serial correlation test on residuals
#' @param Output from vars::serial.test
#' @param Path to where the table should be stored
#' @export Table in specified folder

Serial_test_table_fcnt <- function(Output, Path) {
  Test_outut <- Output$serial
  # Construct the table
  Output_tib <- tibble(
    "Test" = Test_outut$method,
    "Test statistic" = Test_outut$statistic,
    "df" = Test_outut$parameter,
    "p.value" = Test_outut$p.value
  )
  # Store
  write.xlsx(Output_tib, file = paste0(Path, substr(Test_outut$method, 1, 7), "_serial_corr_test.xlsx"), overwrite = TRUE)
}


#' Function that creates a table for VECM estimates
#' @param Trace output from the urca:ca.jo function
#' @param Output from urca::cajorls
#' @param Path to where the table should be stored
#' @export Table in specified folder


VECM_table_fctn <- function(Trace, Output, Path){
  # Extensive form of the estimates produced by ca.jo function
  alpha_mat <- round(Trace@W, 3)
  alpha_mat <- cbind(rownames(alpha_mat), alpha_mat)
  beta_mat <- round(Trace@V, 3)
  beta_mat <- cbind(rownames(beta_mat), beta_mat)
  Gamma_mat <- round(Trace@GAMMA, 3)
  Gamma_mat <- cbind(rownames(Gamma_mat), Gamma_mat)
  # Estimates of the cajorls function
  Output_sum <- summary(Output$rlm)
  Price_response <- Output_sum$`Response Price.d`$coefficients
  Price_response <- cbind(rownames(Price_response), round(Price_response, 3))
  Coal_response <- Output_sum$`Response Coal.d`$coefficients
  Coal_response <- cbind(rownames(Coal_response), round(Coal_response, 3))
  Adj_R_sq <- tibble(Price = round(Output_sum$`Response Price.d`$adj.r.squared, 3), 
                     Coal = round(Output_sum$`Response Coal.d`$adj.r.squared, 3))
  F_stat <- rbind(Output_sum$`Response Price.d`$fstatistic,
                  Output_sum$`Response Coal.d`$fstatistic) %>%
    round(3)
  F_stat <- cbind(matrix(c("Price", "Coal"), nc = 1), round(F_stat, 3))
  
  VECM_estimates <- createWorkbook()
  addWorksheet(VECM_estimates, "alpha")
  addWorksheet(VECM_estimates, "beta")
  addWorksheet(VECM_estimates, "Gamma")
  addWorksheet(VECM_estimates, "beta_norm")
  addWorksheet(VECM_estimates, "Price_response")
  addWorksheet(VECM_estimates, "Coal_response")
  addWorksheet(VECM_estimates, "Adj_R_sq")
  addWorksheet(VECM_estimates, "F_stat")
  writeData(VECM_estimates, "alpha", alpha_mat)
  writeData(VECM_estimates, "beta", beta_mat)
  writeData(VECM_estimates, "Gamma", Gamma_mat)
  writeData(VECM_estimates, "beta_norm", Output$beta)
  writeData(VECM_estimates, "Price_response", Price_response)
  writeData(VECM_estimates, "Coal_response", Coal_response)
  writeData(VECM_estimates, "Adj_R_sq", Adj_R_sq)
  writeData(VECM_estimates, "F_stat", F_stat)
  saveWorkbook(VECM_estimates, file = paste0(Path, "VECM_estimates.xlsx"), overwrite = TRUE)
}

