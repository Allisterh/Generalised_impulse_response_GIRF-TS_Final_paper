#' Function that loads required packages and installs them if not yet installed
#' @param string of a package name

package_fctn <- function(pckg) {
  if (!require(pckg, character.only = TRUE)) {
    cat("Installing required packages")
    install.packages(pckg, dep = TRUE)
  }
  require(pckg, character.only = TRUE)
}


#' Function to initialize the project
#' @import .xlsx file
#' @export lvlData with all data in levels
#' @export logData with all data in logs
#' @export Plot_list with all figure aesthetics
#' @export Plot_path where all figures are stored

Init_fctn <- function() {
  # Load packages
  packages <- c("vars", "tidyverse", "readxl", "lubridate", "urca", "bootUR", "scales", "KFAS", "car", "bruceR", "openxlsx")
  sapply(packages, package_fctn)
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
  # Create folder for the figures and tables
  Plot_path <- paste0(getwd(), "/Plots/")
  if (!dir.exists(Plot_path)) {
    dir.create(Plot_path)
  }
  Table_path <<- paste0(getwd(), "/Tables/")
  if (!dir.exists(Table_path)) {
    dir.create(Table_path)
  }
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
    color_scheme = c(
      "Coal" = navy, "Price" = orange
    ),
    Path = Plot_path
  )
}


#' Function to find the lag order of a VAR via the BIC criterion
#' @param Data matrix with the endogenous variables of the VAR
#' @param deterministic for the VAR
#' @export Preferred lag order

Lag_order_fctn <- function(Data, deterministic) {
  p <- VARselect(Data, lag.max = 5, type = deterministic) # $selection["SC(n)"]
  return(p)
}


# Function to run the Pantula principle
#' @param Data Tibble or df under consideration
#' @param Variable to be analyzed
#' @param d_max maximum integration order
#' @export determ deterministics for the UR-test (note that deterministics are adjusted according to the differencing)

Pantula_fctn <- function(Data, Variable, d_max, determ) {
  if (!is.matrix(Data)) {
    Series <- pull(Data, Variable)
  } else {
    Series <- Data[, Variable]
  }
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
  I_order <- Pantula_result[which(Pantula_result[, 3] > .05)][1] + 1
  if (is.na(I_order)) {
    cat(paste("=> Ho cannot be rejected for any order of differences for", Variable, "and d_max =", d_max), "\n")
  } else {
    cat(paste("=>", Variable, "is integrated of order", I_order), "\n")
  }
  return(Pantula_result)
}


# Function to net out deterministic monthly seasonality based on a state space model
#' @param Data Tibble or df under consideration
#' @param Variable to be analyzed
#' @export Tibble that adds the seasonality and de-seasoned variables to the input dataset

Deseason_fctn <- function(Data, Variable) {
  SS_Model <- SSModel(
    pull(Data, Variable) ~
      SSMtrend(degree = 2, Q = list(matrix(NA), matrix(0))) +
      SSMseasonal(period = 12, Q = matrix(NA), sea.type = "dummy"),
    H = matrix(0)
  )
  SS_ML <- fitSSM(SS_Model, inits = c(0, 0), method = "BFGS")
  SS_ML_output <- KFS(SS_ML$model)
  SS_smoothed <- SS_ML_output$alphahat %>% as_tibble()
  Smoothed_seas <- SS_smoothed$sea_dummy1
  deseaonParam <- paste0("seas", Variable)
  Data_out <- Data %>%
    mutate(
      Seasonality = Smoothed_seas,
      Deseaon = pull(Data, Variable) - Seasonality
    )
  colnames(Data_out)[(length(colnames(Data_out)) - 1):length(colnames(Data_out))] <- paste0(c("seas", "deseas"), Variable)
  return(Data_out)
}
