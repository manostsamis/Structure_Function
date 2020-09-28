#' Structural Heatmap
#'
#' This function creates a heatmap of structural information. Currently for probabilities only.
#' Future work will incorporate thickness heatmaps too.
#'
#' @param oct_data A dataframe with 3 columns: the probability value (non-NANs), the x coordinate and the y coordinate
#' @param nan_oct_data A dataframe with 3 columns (prob, x, y) where NAN values exist. This is needed for mask plotting
#'
#' @return A ggplot object: p
#'
#' @import ggplot2
#' @import scales
#'
#' @export

plot_oct <- function(oct_data, nan_oct_data) {

  # Define colors and shapes
  colorscale <- c("#610007", "#C42400", "#C64B00", "#C87300",
                  "#FF7F00", "#FFEC0F", "#ACCE00", "#84D000",
                  "#5DD200", "#34D400", "#0ABD00")

  # Define plot
  p <- ggplot(oct_data, aes(x, y, fill = prob))

  # Plot OCT thickness map
  p <- p + geom_tile(alpha = 0.8) # RNFL heatmap
  # p <- p + geom_tile(data = rgcp, aes(fill = prob), alpha = 0.8) # RGCP heatmap
  #
  p <- p + geom_tile(data = nan_oct_data, aes(x = x, y=y), fill = "white")


  # Specify heatmap options
  p <- p + scale_fill_gradientn(limits = c(0, 0.1),
                                oob = scales::squish, # set > 0.1 to green
                                colors = colorscale)

  # Specify chart options
  p <- p + labs(x = "", y = "") +
    theme_minimal(base_size = 8) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.position = "none") +
    scale_x_continuous(breaks = seq(from = -27, to = 27, by = 6),
                       limits = c(-27, 27)) +
    scale_y_continuous(breaks = seq(from = -21, to = 21, by = 6),
                       limits = c(-21, 21))

  # Return plot
  p
}
