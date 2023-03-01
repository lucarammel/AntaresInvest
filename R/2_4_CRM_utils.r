#' Meet supply and demand curves and simulate the auction
#'
#' @param market_info_supply (dataframe) dataframe from compute_bids()
#' @param load (float)
#' @param area (str) country code
#'
#' @return auction info list
#' @export
simulation_auction <- function(market_info_supply,
                               load,
                               area){
  
  total_capacity = max(market_info_supply$capacity_cumulative)
  
  # Build curves
  offer_curve = build_stairs_curve(market_info_supply = market_info_supply, 
                                   total_capacity = total_capacity)   
  out = build_demand_curve(load = load,
                           area = area)
  
  demand_curve = out$demand_curve
  demand_points = out$demand_points
  
  # Get information on auction and simulate it
  capacity_retained = which(offer_curve$y - demand_curve$y > 0)[1]
  if (!is.na(capacity_retained)){
    clearing_price = offer_curve$y[capacity_retained]
    capacity_not_retained = total_capacity - capacity_retained
    marginal_bidder = market_info_supply[market_info_supply$bid == clearing_price,]$cluster_name
  }
  else{
    capacity_retained = total_capacity
    clearing_price = CRM[[area]]$price_cap_crm$all
    capacity_not_retained = 0
    marginal_bidder = 'CONE'
  }
  # Reshape demand_points to use it in graphs
  demand_points = demand_points %>% mutate(capacity_cumulative = cumsum(capacity_certified_MW),
                                           capacity_certified_MW = NULL)
  
  return(
    auction_info = list(capacity_retained = capacity_retained,
                        capacity_not_retained = capacity_not_retained,
                        clearing_price = clearing_price,
                        marginal_bidder = marginal_bidder,
                        demand_points = demand_points))
}

#' Simulate the supply curve 
#' 
#' @param market_info_supply (data.table) dataframe from compute_bids()
#' @param total_capacity (float) the total certified capacity
#' 
#' @export
build_stairs_curve = function(market_info_supply, 
                              total_capacity){
  
  x = 1:as.integer(total_capacity)
  y = c()
  
  for (i in 1:nrow(market_info_supply)){
    y = c(y, rep(market_info_supply$bid[i], market_info_supply$capacity_certified_MW[i]))
  }
  
  y = y[1:length(x)] # avoid mismatch between length due to non integer capacity certified
  
  return(data.frame(x = x, 
                    y = y))
}

#' Simulate the supply curve 
#' 
#' @param load (float) load to define the inelastic demand
#' @param area (str) country code
#' 
#' @importFrom magrittr %>% %<>%
#' @export

build_demand_curve = function(load = NA,
                              area){
  
  # Compute the stair part of the affine demand curve
  if (demand$type == 'slope_break'){
    demand_points = data.frame(capacity_certified_MW = demand$slope_break$capacity, 
                               price = demand$slope_break$price)
    
    #demand_points = get_missing_data_demand(obligation = load, 
    #                                        demand_points = demand_points)
    
    demand_points$capacity_cumulative = demand_points$capacity_certified_MW %>% cumsum(.)
    total_capacity = tail(demand_points$capacity_cumulative, n = 1)
    x = 1:total_capacity
    y = c()
    
    for (i in 1:(nrow(demand_points) - 1) ){
      a =  (demand_points[i+1, 'price'] - demand_points[i, 'price']) / (demand_points[i+1,'capacity_cumulative'] - demand_points[i,'capacity_cumulative'])
      b = demand_points[i+1, 'price'] - a * demand_points[i+1, 'capacity_cumulative']
      
      y = c(y, sapply(1: demand_points$capacity_certified_MW[i+1], function(x) {a*(x + demand_points[i, 'capacity_cumulative']) + b}))
    }
  }
  
  else if (demand$type == 'inelastic'){
    price_cap = CRM[[area]]$price_cap_crm$all
    demand_points = data.frame(capacity_certified_MW = c(0,load), 
                               price = c(price_cap, 0))
    
    y = rep(price_cap, load)
    a =  - price_cap
    b =  - a * (load +1)
    y = c(y, sapply(1:1, function(x) {a*(x + load) + b}))
    
    x = 1:length(y)
    
  }
  
  return(list(demand_points = demand_points,
              demand_curve = data.frame(x = x, 
                                        y = y)))
}

#' Compute the obligation for capacity market
#'
#' @param data_antares list of antaresRead datatable with all data from output antares simulation
#' @param zone (str) country code
#'
#' @importFrom reshape2 dcast
#' @importFrom magrittr %>%
#'
#' @return obligation
#' @export
compute_obligation <- function(data_antares,
                               zone){
  
  obligation = data_antares$areas %>% dcast(.,timeId~mcYear, value.var = "LOAD") %>% select(-timeId) %>% 
    as.matrix(.) %>% as.vector(.) %>% sort(., decreasing = TRUE) %>% 
    .[reliability_standard[[zone]]*mc_number] %>% multiply_by(CRM[[zone]]$coeff_securite)
  
  return(obligation)
}

get_missing_data_demand <- function(obligation, 
                                    demand_points){
  # Function to get the parameter computed by the model used to build demand curve 
  if (demand$type == 'linear'){
    demand_points[2,'capacity'] = obligation
  }
  
  return(demand_points)
}
