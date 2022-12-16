
#' Function to initialize the project
#' @param Data tibble with all level time series
#' @param Data tibble with all log time series
#' @param aes_list list with plot aesthetics
#' @param Plot_path folder in which to store the plots
#' @export Plots in specified folder

Lvl_fd_plot_fctn <- function(Data, logData, aes_list, Plot_path) {
  
  Lvl_fd_plot <- function(Data, Trans = "") {
    Data <- filter(Data, !is.na(dCoal))
    lvl_Tib <- select(Data, c(Date, Coal, Price)) %>%
      mutate(Price = Price / max(Price) * max(Coal))
    fd_Tib <- select(Data, c(Date, dCoal, dPrice)) %>%
      mutate(dPrice = dPrice / max(dPrice, na.rm = T) * max(dCoal, na.rm = T))
    fd_Tib$Type <- "First differences"
    lvl_Tib$Type <- "Level"
    colnames(fd_Tib) <- colnames(lvl_Tib)
    Lvl_Fd_plot <- rbind(lvl_Tib, fd_Tib) %>%
      ggplot() +
      geom_line(aes(x = Date, y = Coal, color = paste0(Trans, "Coal deliveries")), linewidth = 1) +
      geom_line(aes(x = Date, y = Price, color = paste0(Trans, "Permit price")), linewidth = 1) +
      scale_y_continuous(
        name = "Gross coal deliveries", # in thou. T
        labels = comma_format(big.mark = ",", decimal.mark = "."),
        sec.axis = sec_axis(~ . * (max(Data$Price, na.rm = T) / max(Data$Coal, na.rm = T)), name = "EU Carbon Permit Price"),
      ) +
      facet_grid(rows = vars(Type), scales = "free") +
      scale_x_date(name = "") +
      theme_bw() +
      labs(color = "") +
      aes_list$Theme_element +
      scale_color_manual(values = aes_list$color_sheme)
    return(Lvl_Fd_plot)
  }

  #------------------------------------------------------------------------------------------#
  # Plot 1
  #------------------------------------------------------------------------------------------#
  
  ggsave(Lvl_fd_plot(Data = lvlData), filename = paste0(Plot_path, "/Lvl_Fd_plot.png"), height = 8, width = 14)

  #------------------------------------------------------------------------------------------#
  # Plot 2
  #------------------------------------------------------------------------------------------#

  ggsave(Lvl_fd_plot(Data = logData, Trans = "Log "), filename = paste0(Plot_path, "/Log_Lvl_Fd_plot.png"), height = 8, width = 14)
}
