#' Create Structure-Function Dataframe
#'
#' This function creates a combined structure-function dataframe where it calculates
#' the Euclidean Distance between OCT pixels/data and VF locations. For each VF location,
#' the median probability of the OCT superpixel (i.e. all OCT data within the Euclidean distance)
#' is calculated and saved in the combined structure-function dataframe
#'
#' @param oct dataframe that holds the structural data
#' @param vf dataframe that holds the functional data
#' @param distance the Euclidean distance required for the creation of the OCT superpixel. Default = 0.5
#' @param morph Logical; if TRUE, the morphed VF locations are chosen
#'
#' @return sf_dframe A dataframe
#'
#' @importFrom dplyr mutate filter
#'
#' @export

create_sf_dframe <- function(oct, vf, distance = 0.5, morph = TRUE) {

  sf_dframe <- vf %>% dplyr::mutate(oct_superpxl_prob = as.numeric(NA))

  for (j in 1:nrow(sf_dframe)) {

    oct_selection <- oct %>%
      dplyr::mutate(euc_dist = sqrt((oct$x - sf_dframe$x_morphed[j])^2 + (oct$y - sf_dframe$y_morphed[j])^2))

    within_range <- oct_selection %>% dplyr::filter(euc_dist <= distance)

    if (nrow(within_range) >= 40) {

      sf_dframe$oct_superpxl_prob[j] <- median(within_range$prob)

    }
  }

  sf_dframe


}








