
#' Build a vector of size year_start : time_horizon with 0 for years not simulated
#'
#' @param revenues_vector (scalar vector) revenues vector
#' @param years_to_simulate (int vector) years to simulate
#' @param year_start (int) starting year of the NPV vector
#' @param replace (bool) if TRUE replace by zero when year is not in years_to_simulate
#'
#' @return revenues vector prepared for NPV computation
#' @export
build_vector_npv <- function(revenues_vector,
                             years_to_simulate,
                             year_start,
                             replace = T){
  if (replace) mask = ifelse(year_start:time_horizon %in% years_to_simulate, 1, 0) else mask = rep(1,length(year_start:time_horizon))
  count = 1
  
  for (i in 1:length(mask) ){
    if (mask[i] == 1) {
      mask[i] = revenues_vector[count]
      count = count + 1
    }
  }
  return(mask)
}


#' Build the dataframe of total net revenues by cluster x years
#'
#' @param revenues_total_net (dataframe) of net revenues
#' @param years_to_simulate (vector of int) with years to simulate
#' 
#' @return dataframe of net revenues by clusters and years prepared for investment decision
#' @importFrom dplyr %>%
#' @export
reformat_df_revenues_total <- function(revenues_total_net,
                                       years_to_simulate){
  
  tmp = t(data.frame(revenues_total_net))
  rownames(tmp) = unlist(lapply(rownames(tmp), gsub, pattern = '.', replacement = ' ', fixed = TRUE))
  year = years_to_simulate[1]
  
  #Insert columns of missing years with zeros
  df = as.data.frame(do.call(rbind, lapply(rownames(tmp), function(x) build_vector_npv(revenues_vector = as.vector(tmp[x,]),
                                                                                       years_to_simulate = years_to_simulate, 
                                                                                       year_start = year,
                                                                                       replace = FALSE))))
  
  rownames(df) = tmp %>% rownames(.) %>% lapply(., gsub, pattern = '.', replacement = ' ', fixed = TRUE) %>% unlist(.)
  colnames(df) = year:time_horizon
  
  return(df) 
}

#' Get data processed for metric (NPV, PI, IRR) computation
#'
#' @param cluster (str) cluster name
#' @param years_to_simulate (vector of int) with years to simulate
#' @param power_plants (dataframe) with costs data
#' @param dataset_revenues (list of dataframe) with the revenues of each cluster for each year in years_to_simulate
#' @param area (str) country code
#'
#' @return list of data : revenues eom, revenues crm, costs om, costs investissements,
#' @export
get_data_for_metric <- function(cluster,
                                years_to_simulate,
                                power_plants,
                                dataset_revenues, 
                                area) {
  
  df = power_plants
  
  #Get data from EOM, capacity markets and hypothesis
  investment_costs = df[cluster,"investment_cost_euros_MW_year"] 
  om_costs = df[cluster, "operational_cost_euros_MW_year"]
  
  revenues_MW = c()
  revenues_crm_MW = c()
  operational_costs = c()
  
  for (y in years_to_simulate){
    market_info_supply = dataset_revenues[[as.character(y)]][[area]]$market_info_supply
    revenues_MW = c(revenues_MW, market_info_supply %>% filter(cluster_name == cluster) %>% select(revenues_MW) %>% as.numeric(.)) 
    
    if (capacity_mechanism$enabled){
      if (area %in% names(CRM)){
        # TODO : build crm functions to get this line works for every clusters
        revenues_crm_MW = c(revenues_crm_MW, market_info_supply %>% filter(cluster_name == cluster) %>% select(revenues_crm_MW) %>% as.numeric(.))
      }
      else {
        revenues_crm_MW = c(revenues_crm_MW, 0)
      }
    }
    
    else {
      revenues_crm_MW = c(revenues_crm_MW, 0)
    }
    
    
    if (!is.na(revenues_MW[length(revenues_MW)])) {
      #Build the vector of costs with values only for available years of the clusters (taking here into account that a cluster could close over years)
      operational_costs = c(operational_costs, om_costs)
    }
    else{ 
      operational_costs = c(operational_costs, 0)
    }
  }  
  
  #Fill na values
  revenues_MW[is.na(revenues_MW)] = 0
  revenues_crm_MW[is.na(revenues_crm_MW)] = 0
  
  return(list(revenues_MW = revenues_MW,
              revenues_crm_MW = revenues_crm_MW,
              om_costs = operational_costs,
              investment_costs = investment_costs))
  
}

