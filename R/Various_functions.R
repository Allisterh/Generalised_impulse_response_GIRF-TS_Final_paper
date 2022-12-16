#' Function to initialize the project
#' @import .xlsx file
#' @export lvlData with all data in levels
#' @export logData with all data in logs
#' @export Plot_list with all figure aesthetics
#' @export Plot_path where all figures are stored

Init_fctn <- function() {
  # Load packages
  packages <- c("vars", "tidyverse", "readxl", "lubridate", "urca", "bootUR", "scales")
  sapply(packages, require, character.only = T)
  # Input data
  Read_data <- function(logTransform) {
    Data_temp <- read_excel("Data.xlsx") %>%
      rename(
        Coal = `Gross inland deliveries hard coal`,
        Price = `EU Carbon Price Permits`
      )
    if (logTransform == TRUE) Data_temp <- mutate_if(Data_temp, is.numeric, log)
    Datea <- Data_temp %>%
      mutate(
        Date = dmy(Date),
        dCoal = diff_mult(Coal, d = 1),
        dPrice = diff_mult(Price, d = 1)
      )
  }
  lvlData <<- Read_data(logTransform = F)
  logData <<- Read_data(logTransform = T)
  # Set aesthetics for figures
  orange <- "#FF6347"
  green <- "#00b159"
  navy <- "#2e4057"
  Text_size <- 25
  Plot_list <<- list(
    Text_size = Text_size,
    Theme_element = theme(
      legend.position = "bottom",
      legend.text = element_text(size = Text_size),
      axis.text = element_text(size = Text_size),
      axis.title = element_text(size = Text_size + 3, vjust = 1),
      axis.ticks.length = unit(.25, "cm"),
      axis.ticks = element_line(linewidth = 1),
      legend.title = element_blank(),
      legend.margin = margin(t = -25),
      strip.text = element_text(size = Text_size)
    ),
    color_sheme = c(
      "Coal deliveries" = navy, "Permit price" = orange,
      "Log Coal deliveries" = navy, "Log Permit price" = orange
    )
  )
  # Create folder for the figures
  Plot_path <<- paste0(getwd(), "/Plots/")
  if (!dir.exists(Plot_path)) {
    dir.create(Plot_path)
  }
}


#' Function to find the lag order of a VAR via the BIC criterion
#' @param Data matrix with the endogenous variables of the VAR
#' @param deterministic for the VAR
#' @export Preferred lag order

Lag_order_fctn <- function(Data, deterministic) {
  p <- VARselect(Data, lag.max = 5, type = deterministic)$selection["SC(n)"]
  return(p)
}


# Function to run the Pantula principle
#' @param Series under consideration
#' @param d_max maximum integration order
#' @export determ deterministics for the UR-test (note that deterministics are adjusted according to the differencing)

Pantula_fctn <- function(Series, d_max, determ) {
  d_max <- d_max - 1
  Pantula_result <- sapply(d_max:0, function(d) {
    # Difference the data
    Diff_series <- diff_mult(Series, d)
    # Set the deterministics
    if (determ == "trend") {
      deterministics <- ifelse(d >= 1, "intercept", "trend")
      deterministics <- ifelse(d > 1, "none", determ)
    } else if (determ == "intercept") {
      deterministics <- ifelse(d >= 1, "none", "intercept")
    } else {
      deterministics <- "none"
    }
    # Perform test on the differenced TS
    Test_result <- boot_adf(Diff_series, deterministics)
    # Collect results and check if H_0 is rejected
    Result_vec <- round(c(Test_result$statistic, Test_result$p.value), 3)
    return(Result_vec)
  })
  Pantula_result <- t(rbind(d = d_max:0, Pantula_result))
  cat(paste("=>", colnames(Series), "is integrated of order", Pantula_result[which(Pantula_result[, 3] > .05)[1], 1] + 1), "\n")
  return(Pantula_result)
}
