
#' Function to create a plot that depicts two series once in levels as well as in first differences
#' @param Data tibble with all level time series
#' @param logData tibble with all log time series
#' @param aes_list list with plot aesthetics
#' @export Plots in specified folder

Lvl_fd_plot_fctn <- function(Data, logData, aes_list) {
  Lvl_fd_plot <- function(Data, Trans = "", aes_list) {
    # Set the color scheme
    color_map <- aes_list$color_scheme
    names(color_map) <- c(paste0(Trans, "Coal deliveries"), paste0(Trans, "Permit price"))
    # To prevent warning messages
    Data <- filter(Data, !is.na(dCoal))
    # Prepare data for the plot
    lvl_Tib <- select(Data, c(Date, Coal, Price)) %>%
      mutate(Price = Price / max(Price) * max(Coal))
    fd_Tib <- select(Data, c(Date, dCoal, dPrice)) %>%
      mutate(dPrice = dPrice / max(dPrice, na.rm = T) * max(dCoal, na.rm = T))
    fd_Tib$Type <- paste0("First differences")
    lvl_Tib$Type <- ifelse(Trans == "", "Level", Trans)
    colnames(fd_Tib) <- colnames(lvl_Tib)
    # Construct the plot
    Lvl_Fd_plot <- rbind(lvl_Tib, fd_Tib) %>%
      ggplot() +
      geom_line(aes(x = Date, y = Coal, color = paste0(Trans, "Coal deliveries")), linewidth = 1) +
      geom_line(aes(x = Date, y = Price, color = paste0(Trans, "Permit price")), linewidth = 1) +
      scale_y_continuous(
        name = "Gross coal deliveries", # in thou. T
        labels = comma_format(big.mark = ",", decimal.mark = "."),
        # limits = c((min(Data$Coal, na.rm = T) * .9), max(Data$Coal, na.rm = T) * 1.1),
        sec.axis = sec_axis(~ . * (max(Data$Price, na.rm = T) / max(Data$Coal, na.rm = T)), name = "EU Carbon Permit Price"),
      ) +
      facet_grid(rows = vars(Type), scales = "free") +
      scale_x_date(name = "", date_breaks = "1 year", date_labels = c("%Y")) +
      theme_bw() +
      labs(color = "") +
      aes_list$Theme_element +
      scale_color_manual(values = color_map)
    return(Lvl_Fd_plot)
  }

  # Plot for the series in levels
  Lvl_Fd_plot <- Lvl_fd_plot(Data = lvlData, aes_list = aes_list)
  ggsave(Lvl_Fd_plot,
    filename = paste0(aes_list$Path, "Lvl_Fd.png"), height = 8, width = 14
  )

  # Plot for the series in logs
  Log_Lvl_Fd_plot <- Lvl_fd_plot(Data = logData, Trans = "Log ", aes_list = aes_list)
  ggsave(Log_Lvl_Fd_plot,
    filename = paste0(aes_list$Path, "Log_Lvl_Fd.png"), height = 8, width = 14
  )

  return(list("Lvl_Fd_plot" = Lvl_Fd_plot, "Log_Lvl_Fd_plot" = Log_Lvl_Fd_plot))
}


#' Function to create (i) a plot with the seasonality of a series (ii) compares the de-seasoned and original series
#' @param Data tibble with all data (output from the Deseason_fctn)
#' @param Variable to be analysed
#' @param aes_list list with plot aesthetics
#' @export Plots in specified folder

Seas_plot_fctn <- function(Data, Variable, aes_list) {
  Data <- filter(Data, !is.na(Data[, Variable]))
  # Seasonal component (series is defined explicitly so that it can be called dynamically)
  seas <- pull(Data, paste0("seas", Variable))
  # Construct plot
  Season_plot <- Data %>%
    ggplot(aes()) +
    geom_line(aes(x = Date, y = seas), linewidth = 1) +
    scale_x_date(name = "", date_breaks = "1 year", date_labels = c("%Y")) +
    scale_y_continuous(
      name = "Gross coal deliveries", # in thou. T
      labels = comma_format(big.mark = ",", decimal.mark = "."),
      breaks = seq(-15e02, 15e02, 5e02)
    ) +
    theme_bw() +
    labs(color = "") +
    aes_list$Theme_element
  # Save plot
  ggsave(Season_plot,
    filename = paste0(aes_list$Path, Variable, "_Seas.png"), height = 8, width = 14
  )

  # De-seasoned vs raw series
  color_map <- aes_list$color_scheme
  names(color_map) <- c("Raw series", "Seasonally adjusted")
  # Extract the x-axis series (series are defined explicitly so that they can be called dynamically)
  deseas <- pull(Data, paste0("deseas", Variable))
  var <- pull(Data, Variable)
  # Construct plot
  deSeason_plot <- Data %>%
    ggplot() +
    geom_line(aes(x = Date, y = deseas, color = "Seasonally adjusted"), linewidth = 1) +
    geom_line(aes(x = Date, y = var, color = "Raw series"), linewidth = 1) +
    scale_x_date(name = "", date_breaks = "1 year", date_labels = c("%Y")) +
    scale_y_continuous(
      name = "Gross coal deliveries", # in thou. T
      labels = comma_format(big.mark = ",", decimal.mark = ".")
    ) +
    scale_color_manual(values = color_map) +
    theme_bw() +
    labs(color = "") +
    aes_list$Theme_element
  # Save plot
  ggsave(deSeason_plot,
    filename = paste0(aes_list$Path, Variable, "_seas_adjusted.png"), height = 8, width = 14
  )

  return(list("Season_plot" = Season_plot, "deSeason_plot" = deSeason_plot))
}


#' Function to create a plot of the two series in their final states
#' @param Data tibble with all data (containing only the date and the two final series)
#' @param aes_list list with plot aesthetics
#' @export Plots in specified folder

Final_series_plot_fctn <- function(Data, aes_list) {
  # Set color scheme
  color_map <- aes_list$color_scheme
  names(color_map) <- c("Coal deliveries", "First differences permit price")
  # Scale the data to fit them in the same plot
  Data_scaled <- Data %>%
    mutate(Price = Price / max(Price) * max(Coal))
  # Construct the plot
  Final_series <- Data_scaled %>%
    ggplot() +
    geom_line(aes(x = Date, y = Price, color = "First differences permit price"), linewidth = 1) +
    geom_line(aes(x = Date, y = Coal, color = "Coal deliveries"), linewidth = 1) +
    scale_y_continuous(
      name = "Gross coal deliveries", # in thou. T
      labels = comma_format(big.mark = ",", decimal.mark = "."),
      breaks = seq(-5e03, 20e03, 5e03),
      sec.axis = sec_axis(~ . * (max(Data$Price, na.rm = T) / max(Data$Coal, na.rm = T)), name = "EU Carbon Permit Price"),
    ) +
    scale_color_manual(values = color_map) +
    scale_x_date(name = "", date_breaks = "1 year", date_labels = c("%Y")) +
    theme_bw() +
    labs(color = "") +
    aes_list$Theme_element
  # Save plot
  ggsave(Final_series,
    filename = paste0(aes_list$Path, "Final_series.png"), height = 8, width = 14
  )
  return(Final_series)
}


#' Function to create a plot of AC values
#' @param Data that holds the series under consideration
#' @param Variable to analyze
#' @param Interval confidence interval
#' @param aes_list list with plot aesthetics
#' @export Plots in specified folder

ACF_plot_fctn <- function(Data, Variable, Interval = .95, aes_list) {
  # Compute the AC values
  ACF <- acf(Data[, Variable], plot = FALSE, lag.max = 20)
  # Compute the confidence interval
  CI <- qnorm((1 + Interval) / 2) / sqrt(ACF$n.used)
  # Prepare the data for the plot
  ACF_vec <- ACF$acf[, 1, 1][-1]
  Lags <- ACF$lag[, 1, 1][-1]
  # Construct the plot
  ACF_plot <- ggplot() +
    geom_bar(aes(x = Lags, y = ACF_vec), stat = "identity", width = .5) +
    geom_line(aes(x = Lags, y = CI), linetype = "dashed", color = "#1874CD", linewidth = 1) +
    geom_line(aes(x = Lags, y = -CI), linetype = "dashed", color = "#1874CD", linewidth = 1) +
    geom_hline(yintercept = 0, color = "black", linewidth = 1) +
    theme_bw() +
    scale_y_continuous(
      breaks = seq(-1, 1, 0.1),
      name = expression(paste("ACF ", epsilon[t]))
    ) +
    scale_x_continuous(breaks = seq(0, tail(Lags, 1), by = 2)) +
    aes_list$Theme_element
  # Save plot
  ggsave(ACF_plot,
    filename = paste0(aes_list$Path, Variable, "_VAR_Resid_ACF.png"), height = 8, width = 14
  )
  return(ACF_plot)
}
