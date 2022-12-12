#' Function to initialize the project
#' @import .xlsx file
#' @export Tibble with all data

Init_fctn <- function() {
  packages <- c("vars", "tidyverse", "readxl", "lubridate", "urca", "bootUR")
  sapply(packages, require, character.only = T)
  Data <<- read_excel("Data.xlsx") %>%
    mutate(Date = dmy(Date)) %>%
    rename(Coal = `Gross inland deliveries hard coal`,
           Price = `EU Carbon Price Permits`) %>%
    mutate_if(is.numeric, log) 
}
