# this file reads in and prepares data to be used in 

# data objects used in the computations are 
# dinput - data frame used for plotting and generating inputs to rjags
# d - a list used as an input to rjags

# data inputs vary by model and all files reading in data are in read_data folder
# for each model, the data can be sourced by running source("...") command
# for example, for model S1 (main model), the following should be run

source("code/read_data/S1_read_data.R")

# data for each model are read within 2_run_models.R file
