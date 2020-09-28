library(StructureFunction)

## The following libraries are automatically loaded with the StructureFunction package
# library(tidyverse)
# library(zeallot)


## This library is optional; only needed to combine plots together
library(ggpubr)

### CSV files for an example are provided in the StructureFunction/data folder ###


# Path to .csv file that contains a matrix of 512x256
# Probability values of Widefield RNFL scans.
# Change to your local path
rnfl_path <- "StructureFunction/data/ProbRNFLWideFieldGlaucomaExample.csv"

# Path to .csv file that contains a vector of 512
# values that represent the x coordinates (in degrees)
# of each data point in Widefield RNFL scan.
# Change to your local path
x_rnfl_path <- "StructureFunction/data/xProbRNFLWideFieldGlaucomaExample.csv"

# Path to .csv file that contains a vector of 256
# values that represent the y coordinates (in degrees)
# of each data point in Widefield RNFL scan.
# Change to your local path
y_rnfl_path <- "StructureFunction/data/yProbRNFLWideFieldGlaucomaExample.csv"

# Path to .csv file that contains a matrix of
# Probability values of Widefield RGCP scans.
# The scan is a 6x6 (degrees) derived from the Widefield scan.
# The size of the matrix is (usually) 257x172 data points
# Change to your local path
rgcp_path <- "StructureFunction/data/ProbRGCPWideFieldGlaucomaExample.csv"

# Path to .csv file that contains a vector of 
# values that represent the x coordinates (in degrees)
# of each data point in Widefield RGCP scan.
# Change to your local path
x_rgcp_path <- "StructureFunction/data/xProbRGCPWideFieldGlaucomaExample.csv"


# Path to .csv file that contains a vector of 
# values that represent the y coordinates (in degrees)
# of each data point in Widefield RGCP scan.
# Change to your local path
y_rgcp_path <- "StructureFunction/data/yProbRGCPWideFieldGlaucomaExample.csv"


# Path to .csv file that contains VF data from 10-2 and 24-2
vf_path <- "StructureFunction/data/VF_TD_GlaucomaExample.csv"

# Eye orientation. Needed when combining Structure-Function Plots
eye <- "OS"

#This function reads the csvs and creates the RNFL dataframe
c(rnfl, rnfl_nan_area) %<-%oct_to_df(prob_path = rnfl_path,
                                     x_path = x_rnfl_path,
                                     y_path = y_rnfl_path)

#This function reads the csvs and creates the RGCP dataframe
c(rgcp,rgcp_nan_area) %<-% oct_to_df(prob_path = rgcp_path,
                                     x_path = x_rgcp_path,
                                     y_path = y_rgcp_path)

# Read the VF dataframe
vf <- read_csv(vf_path)

# Create a Structure-Function dataframe for RNFL
sf_rnfl <- create_sf_dframe(rnfl, vf)

# Create a Structure-Function dataframe for RGCP
sf_rgcp <- create_sf_dframe(rgcp, vf)

# Create the OCT RNFL plot 
p_rnfl <- plot_oct(rnfl, rnfl_nan_area)

# Superimpose VF lcoations, and create Structure-Function RNFL Plot
sf_rnfl <- plot_vf_on_oct(p_rnfl, sf_rnfl, plot_overlap = TRUE)

# Create the OCT RGCP plot 
p_rgcp <- plot_oct(rgcp, rgcp_nan_area)

# Superimpose VF lcoations, and create Structure-Function RGCP Plot
sf_rgcp <- plot_vf_on_oct(p_rgcp, sf_rgcp, plot_overlap =TRUE)


# Combine the two plots; plot orientation is based on eye orientation
if (eye == "OD") {
  p_combined <- ggarrange(sf_rgcp, sf_rnfl,
                          ncol = 2, nrow = 1)
} else {
  p_combined <- ggarrange(sf_rnfl, sf_rgcp,
                          ncol = 2, nrow = 1)
}


#Choose a directory to export image
#Change this path
output_dir <- "Documents"

# Create the directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save image
ggsave(filename = paste(output_dir,"SF_Plot.png", sep = "/") ,
       plot = p_combined, device = "png", height = 7, width = 17.5, units = "in", dpi = 300)
