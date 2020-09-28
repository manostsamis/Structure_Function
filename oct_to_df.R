#' OCT to dataframe
#'
#' This function reads Topcon OCT data from a .csv file and converts this to a dataframe
#'
#' @param prob_path The file path of the .csv file that contains the structural data
#' @param x_path The file path of the .csv file that contains the x coordinates
#' @param y_path The file path of the .csv file that contains the y coordinates
#'
#' @return List of 2 OCT dataframes: one with the x,y coordinates of the non-NAN values,
#'     another with the x,y coordinates of the NAN values
#'
#' @importFrom readr read_csv
#' @importFrom tidyr gather
#' @importFrom dplyr filter mutate
#'
#' @export

oct_to_df <- function(prob_path, x_path, y_path) {
  # Import data -----------------------
  prob <- readr::read_csv(prob_path, col_names = FALSE, col_types = cols())

  x <- readr::read_csv(x_path, col_names = FALSE, col_types = cols()) %>%
    as.vector() %>%
    tidyr::gather(key = "row_name",value = "x")

  y <- readr::read_csv(y_path, col_names = FALSE, col_types = cols()) %>%
    as.vector() %>%
    tidyr::gather(key = "row_name",value = "y")

  # Check -----------------------
  stopifnot(nrow(prob) == nrow(y),
            ncol(prob) == nrow(x))

  # Set coordinates -----------------------
  colnames(prob) <- as.character(x$x)
  prob <- prob %>% dplyr::mutate(y = y$y)

  # Transform to data frame -----------------------
  oct <- prob %>% tidyr::gather(key = "x", value = "prob", -y)
  oct$x <- as.double(oct$x)

  # Clean, Reorder Columns, Return -----------------------
  nan_area <- oct %>% dplyr::filter(is.na(prob))

  oct <- oct %>% dplyr::filter(!is.na(prob))

  rnfl_list <- list("clean" = oct, "nans" = nan_area)
}
