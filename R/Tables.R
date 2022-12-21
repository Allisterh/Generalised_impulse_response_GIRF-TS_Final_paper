#' Function that creates a table of summary statistics
#' @param Data tibble with the data
#' @param Path to where the table should be stored
#' @export Table in specified folder

Summary_stats_fctn <- function(Data, Path){
  Summary_Stats <- select(Data,-Date) %>%
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

UR_table_fctn <- function(Output, Variable, Path){
  write.xlsx(as_tibble(Output), file = paste0(Path, Variable, "_UR_Test.xlsx"), overwrite = TRUE)
}


#' Function that creates a table of the VAR estimates
#' @param Output from vars::Var
#' @param Path to where the table should be stored
#' @export Table in specified folder

VAR_table_fctn <- function(Output, Path){
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
  Coal_F <-rbind(rownames(Coal_F), round(Coal_F, 3))
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

Granger_table_fctn <- function(){}