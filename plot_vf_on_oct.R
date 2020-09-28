#' Structure-Function Heatmap
#'
#' This function superimposes functional data on structural heatmaps (created by plot_oct).
#'
#' @param p_oct A ggplot of the structural heatmap created by plot_oct
#' @param vf_dframe A dataframe of functional data created by get_vf_coords
#' @param plot_overlap Logical. If TRUE, it highlights VF locations with S-F agreement
#'
#' @return A ggplot object: p
#'
#' @import ggplot2
#'
#' @export

plot_vf_on_oct <- function(p_oct, vf_dframe, plot_overlap = TRUE) {

  shapes <- c("0.5" = 19, "1" = 19, "2" = 19, "5" = 19, "95" = 1)

  colors <- c("0.5" = "black", "1" = "black",
              "2" = "black", "5" = "black",
              "95" = "#595959")

  p <- p_oct + geom_point(aes(x_morphed, y_morphed,
                              color = factor(prob),
                              shape = factor(prob)),
                          size = 4,
                          data = vf_dframe,
                          show.legend = FALSE) +
    scale_shape_manual(drop = FALSE,
                       #name = legend_title,
                       values = shapes,
                       breaks = c("0.5", "1", "2", "5", "95"),
                       labels = c("< 0.5%", "< 1%", "< 2%", "< 5%", "100% (unspecified)")) +
    scale_color_manual(drop = FALSE,
                       #name = legend_title,
                       values = colors,
                       breaks = c("0.5", "1", "2", "5", "95"),
                       labels = c("< 0.5%", "< 1%", "< 2%", "< 5%", "100% (unspecified)"))

  if(plot_overlap){

    sf_overlap <- vf_dframe %>% filter(prob < 95 & oct_superpxl_prob < 0.10)

    p <- p + geom_point(data = sf_overlap, aes(x_morphed, y_morphed),
                        size = 6, shape = 5, stroke = 1,
                        color = "black")

  }

  p
}
