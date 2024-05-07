# functions to be read before analysis

add_node <- function(D){
  D %>%
    mutate(node_id = str_extract(string = Parameter, pattern =  "\\[.*?\\]"),
           node_id = str_extract(string = node_id, pattern = "[:digit:]+"),
           node_id = as.numeric(node_id))
}


add_corridor <- function(D, n = 756, corridor = corridors){
  D %>%
    mutate(corridor = node_id %% n,
           corridor = ifelse(corridor == 0, n, corridor)) %>%
    left_join(corridors)
}

y_stats <- function(D, type = "total"){
  if(type == "total"){
    g <- D %>% 
      group_by(Chain, Iteration, year) %>%
      summarise(total_chit = sum(exp(value))) %>%
      group_by(year)
  }
  
  if(type == "imm"){
    g <- D %>% 
      group_by(Chain, Iteration, year, dest) %>%
      summarise(total_chit = sum(exp(value))) %>%
      ungroup() %>%
      rename(name = dest) %>%
      group_by(year, name)
  }
  
  
  
  if(type == "emi"){
    g <- D %>% 
      group_by(Chain, Iteration, year, orig) %>%
      summarise(total_chit = sum(exp(value))) %>%
      ungroup() %>%
      rename(name = orig) %>%
      group_by(year, name)
  }

  
  
  if(type == "bilat"){
    g <- D %>% 
      group_by(Chain, Iteration, year, orig, dest) %>%
      summarise(total_chit = sum(exp(value))) %>%
      group_by(year, orig, dest)
  }
  
  
  g %>%
    summarise(total = median(total_chit),
              # upp50 = quantile(x = value, probs = 0.75),
              # lwr50 = quantile(x = value, probs = 0.25),
              # upp80 = quantile(x = value, probs = 0.9),
              # lwr80 = quantile(x = value, probs = 0.1)
              sd = sd(total_chit), 
              upp50 = quantile(x = total_chit, probs = 0.75),
              lwr50 = quantile(x = total_chit, probs = 0.25), 
              upp80 = quantile(x = total_chit, probs = 0.9),
              lwr80 = quantile(x = total_chit, probs = 0.1)) 
}



# saveresults <- function(D, type = "total"){
#   if(type == "total"){
#     table_name <- paste0("results/", model_name,"_tot_y.csv")
#     write.csv(D, table_name, row.names = F)
#   }
#   if(type == "imm"){
#     table_name <- paste0("results/", model_name,"_imm_y.csv")
#     write.csv(D, table_name, row.names = F)
#   }
#   if(type == "emi"){
#     table_name <- paste0("results/", model_name,"_emi_y.csv")
#     write.csv(D, table_name, row.names = F)
#   }
#   if(type == "bilat"){
#     table_name <- paste0("results/", model_name,"_bilat_y.csv")
#     write.csv(D, table_name, row.names = F)
#   }
# }


# wrapper function for running models via JAGS
run.model <- function(model, data, n.chains = 3, n.adapt = 1e02, n.update = 5e02, n.iter = 1e03, 
                      thin=10){
  
  m <- jags.parfit(cl = cl,   
                   n.chains = n.chains, n.adapt = n.adapt, n.update = n.update, n.iter = n.iter, 
                   thin=thin,
                   data = data, model = paste0("./code/models/",model), 
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
                              "gamma_ons", "tau_ons", "gamma_nso", "tau_nso","w"))
  
  return(m=m)  
  
}