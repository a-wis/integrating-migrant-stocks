# this file contains code for running models in JAGS
# it uses rjags and parallelisation


library(dclone)


##
## jags run
##

# set up parallelisation
# stopCluster(cl)
cl <- makePSOCKcluster(names = 10)
tmp <- clusterEvalQ(cl = cl, expr = library(dclone))
parLoadModule(cl = cl, name = 'glm', quiet = TRUE)
parLoadModule(cl = cl, name = 'lecuyer', quiet = TRUE)




# run main model ####
# Denoted S1 in the paper #
(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/main_model.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1

# save(m, file = "results/m_main_model.RData", compress = "xz")


# run naive model with global drift####
# appendix B1-3
# run model
(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/naive_globalW.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso", "w"))


s1 <- Sys.time() - s0
s1

# save(m, file = "results/m_naive_globalW.RData", compress = "xz")


# run naive model####
# appendix B4-5
# run model
(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/naive2.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1

# save(m, file = "results/m_naive2.RData", compress = "xz")




# Sensitivity Models S2-S9 ####

## S2 ####
(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/m_fb_precS2.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1

## S3-S7 ####
# here, the model used is a main model but with varied input data, see file 1_read_data.R, line 264

(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/main_model.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1

## S8 ####
#eurostat only

(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/m_eurostat_onlyS8.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1


## s9 ####
# without facebook 

(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/m_wo_facebookS9.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1



# Out of Sample Prediction ####
# main model with changed data inputs 
(s0 <- Sys.time())
m <- jags.parfit(cl = cl,   
                 n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                 thin=10,
                 data = d, model = "./code/main_model.txt", 
                 params = c("y1", "tau_y1", "sigma_y1", 
                            "beta", "mu_beta", "beta1", "mu_beta1",
                            "tau_beta", "tau_beta1", "sigma_beta1","sigma_beta2", "sigma_beta3", 
                            "eurostat",
                            "gamma_eurostat", "tau_eurostat", "intercept_eurostat", "slope_eurostat",
                            "gamma_fbmau","gamma_fbmau_2016", "gamma_fbmau_2017", 
                            "gamma_fbmau_2018", "gamma_fbmau_2019", "gamma_fbdau_2018", "gamma_fbdau_2019",
                            "tau_fbmau", "intercept_fbmau", "slope_fbmau", 
                            "gamma_fbdau", "tau_fbdau", "intercept_fbdau", "slope_fbdau",
                            "kappa_fbmau_2019", "kappa_fbdau_2019",
                            "gamma_lfs", "tau_lfs","intercept_lfs", "slope_lfs",
                            "gamma_census", "tau_census", 
                            "gamma_ons", "tau_ons", "gamma_nso", "tau_nso"))


s1 <- Sys.time() - s0
s1
