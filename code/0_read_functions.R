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
