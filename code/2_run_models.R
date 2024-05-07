# this file contains code for running models in JAGS
# it uses rjags and parallelisation


library(dclone)
library(rjgas)

source("code/0_read_functions.R")





# run models ####
# run them ONE by ONE rather than all at once

# set up parallelisation
# stopCluster(cl)
cl <- makePSOCKcluster(names = 10)
tmp <- clusterEvalQ(cl = cl, expr = library(dclone))
parLoadModule(cl = cl, name = 'glm', quiet = TRUE)
parLoadModule(cl = cl, name = 'lecuyer', quiet = TRUE)

# run main model ####
# Denoted S1 in the paper #
source("code/read_data/S1_read_data.R")
m = run.model(model = "main_model.txt", data= d)
# S2: low Facebook precision
source("code/read_data/S1_read_data.R")
m = run.model(model = "m_fb_precS2.txt", data= d)
# removed 5% of Eurostat data
source("code/read_data/S3_read_data.R")
m = run.model(model = "main_model.txt", data= d)
# removed 10% of Eurostat data
source("code/read_data/S4_read_data.R")
m = run.model(model = "main_model.txt", data= d)
# removed 20% of Eurostat data
source("code/read_data/S5_read_data.R")
m = run.model(model = "main_model.txt", data= d)
# removed 2016 Eurostat data
source("code/read_data/S6_read_data.R")
m = run.model(model = "main_model.txt", data= d)
# removed 2018 Eurostat data
source("code/read_data/S7_read_data.R")
m = run.model(model = "main_model.txt", data= d)
# eurostat data only
source("code/read_data/S1_read_data.R")
m = run.model(model = "m_eurostat_onlyS8.txt", data= d)
# facebook data only
source("code/read_data/S1_read_data.R")
m = run.model(model = "m_wo_facebookS9.txt", data= d)

# Out of Sample Prediction ####
# main model with changed data inputs 
# removed 2017 Eurostat data
source("code/read_data/oo1_read_data.R")
m = run.model(model = "main_model.txt", data= d) 
# removed 2018 Eurostat data
source("code/read_data/S7_read_data.R")
m = run.model(model = "main_model.txt", data= d)

# run naive model with global drift####
# appendix B1-3
source("code/read_data/S1_read_data.R")
m = run.model(model = "naive_globalW.txt", data= d)
# run naive model####
# appendix B4-5
source("read_data/S1_read_data.R")
m = run.model(model = "naive2.txt", data= d)




