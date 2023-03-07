#' Capacity market simulation mother function
#'
#' @param year_simulated (str) year simulated
#' @param obligation (scalar) obligation computed by obligation()
#' @param power_plants (dataframe) with costs hypothesis by clusters
#' @param capacity_certified (scalar) certified capacity by cluster
#' @param clusters_not_supported (str vector) clusters names candidates to the revenues analysis and investments.
#' @param zone (str) country code
#' @param market_info_supply (dataframe) dataframe of market supply side information
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate
#' @importFrom plyr rbind.fill
#' 
#' @return auction and the supply side information.
#' @export
simulation_mecanism_capacity <- function(year_simulated,
                                         obligation,
                                         power_plants,
                                         capacity_certified,
                                         clusters_not_supported,
                                         zone,
                                         market_info_supply, 
                                         spot = NA,
                                         LOLD = NA,
                                         revenues = NA) {
  
  
  ## Estimating the margin average thanks to the abacus
  margin = LOLE_margin_equation[[zone]]$a*log(LOLD) + LOLE_margin_equation[[zone]]$b ### MARGIN = a.ln(LOLD)+b
  
  # Compute the bids and capacity certified by technology
  out = compute_bids(clusters_not_supported = clusters_not_supported,
                     power_plants = power_plants,
                     capacity_certified = capacity_certified, 
                     market_info_supply = market_info_supply,
                     revenues = revenues, 
                     margin = margin,
                     obligation = obligation,
                     spot = spot,
                     zone = zone)
  
  ro_strike_price = out$ro_strike_price    
  market_info_supply = out$market_info_supply
  
  if (reliability_options$enabled & !reliability_options$by_capacity){ 
    print(glue('===> RO activated with a strike price : {ro_strike_price} euros/MWh'))
  }
  else if (reliability_options$enabled & reliability_options$by_capacity) {
    print(glue('==> RO activated with strike price by capacity.'))
  }
  ## Auction simulation
  auction_info = simulation_auction(market_info_supply = market_info_supply,
                                    load = obligation,
                                    area = zone)
  clearing_price = auction_info$clearing_price
  capacity_retained = auction_info$capacity_retained
  
  # Computing CRM revenues
  # Option : price cap by technology applied here
  if (!capacity_mechanism$price_cap_by_technology){
    market_info_supply %<>% mutate(revenues_crm_MW = ifelse(bid < clearing_price, # retained by the market
                                                            capacity_certified_MW * clearing_price / capacity_MW, 
                                                            ifelse(
                                                              bid == clearing_price, # marginal bidder
                                                              (capacity_cumulative - capacity_retained) * clearing_price / capacity_MW,
                                                              0)))
  } else{
    #Get price of the auction as minimum of the price and price cap set for each cluster
    df =  market_info_supply  %>% .[!rownames(.) %in% c('a_zero_bidders'),] # getting rid temporarly of this fictive line
    
    price_cap_by_technology = sapply(df$cluster_name, function(c) {CRM[[zone]]$price_cap_crm[[get_cluster_type(c)]]})
    clearing_price_capped = clearing_price %>% rep(., df %>% nrow(.)) %>% pmin(., price_cap_by_technology) %>% setNames(df$cluster_name)
    
    market_info_supply %<>% mutate(revenues_crm_MW = ifelse(bid < clearing_price, # retained by the market
                                                            capacity_certified_MW * clearing_price / capacity_MW, 
                                                            ifelse(
                                                              bid == clearing_price, # marginal bidder
                                                              (capacity_cumulative - capacity_retained) * clearing_price_capped[cluster_name] / capacity_MW,
                                                              0)))
  }
  # Option : RO, correct revenues obtained by compute bids 
  if (reliability_options$enabled){
    market_info_supply %<>% mutate(rendered_revenues_MW = ifelse(bid > clearing_price, 
                                                                 0, 
                                                                 ifelse(
                                                                   bid == clearing_price, 
                                                                   (capacity_cumulative - capacity_retained) * rendered_revenues_MW / capacity_certified_MW,
                                                                   rendered_revenues_MW)))
  }
  # Build supply curve from existing data and add a CONE artificial bidder to make sure graphs have intersection
  supply_curve = market_info_supply[,c('cluster_name', 'bid', 'capacity_MW', 'capacity_cumulative')] %>% 
    set_rownames(NULL) %>% rbind(., data.frame(cluster_name = 'CONE', 
                                               bid = CRM[[zone]]$price_cap_crm$all, 
                                               capacity_MW = 0, 
                                               capacity_cumulative = .[nrow(.),'capacity_cumulative']))
  demand_curve = auction_info$demand_points
  auction_info$demand_points = NULL 
  auction_info %<>% as.data.frame(.)
  
  market_info_supply %<>% mutate(capacity_cumulative = NULL) %>%
    .[!rownames(.) %in% c('a_zero_bidders'),] %>% set_rownames(NULL)
  
  return(
    list(market_info_supply = market_info_supply,
         auction_info = auction_info,
         ro_strike_price = ro_strike_price,
         supply_curve = supply_curve,
         demand_curve = demand_curve)
  )
}

#' Compute the bids of the supply side and make the market_info_supply side dataframe
#'
#' @param clusters_not_supported (str vector) clusters names candidates to the revenues analysis and investments.
#' @param power_plants (dataframe) of costs hypothesis
#' @param capacity_certified (scalar) certified capacity by clusters
#' @param market_info_supply (datatable) summary of information on supply side
#' @param zone (str) zone
#' @param spot 
#' @param clusters_modulation
#' @param revenues (list of dataframes)
#'
#' @importFrom dplyr %>% arrange
#'
#' @return market_info_supply a dataframe with info concerning the supply side (revenues, bids capacity.. )
#' @export
compute_bids <- function(clusters_not_supported,
                         power_plants, 
                         capacity_certified, 
                         market_info_supply,
                         margin, 
                         obligation,
                         revenues,
                         spot,
                         zone) {
  
  if (reliability_options$enabled) {df = data.frame(capacity_certified_MW = numeric(),
                                                    bid = numeric(),
                                                    revenues_crm_MW = numeric(), 
                                                    rendered_revenues_MW = numeric())} 
  else {df = data.frame(capacity_certified_MW = numeric(),
                        bid = numeric(),
                        revenues_crm_MW = numeric())}
  
  
  if (reliability_options$enabled & reliability_options$dynamic_strike_price$enabled){
    ### We have to compute the max variable cost
    max_variable_cost = clusters_not_supported %>% sapply(.,function(c){ power_plants[c,"marginal_cost"] * clusters_modulation[cluster == c]$marginalCostModulation } ) %>%
      max(.) %>% as.numeric(.)
    ro_strike_price = max_variable_cost + reliability_options$dynamic_strike_price$surplus
  } 
  else if(reliability_options$enabled & !reliability_options$dynamic_strike_price$enabled) {
    if (!reliability_options$by_capacity) ro_strike_price = RO_strike_price_country[[zone]] 
    else{
      ro_strike_price = lapply(clusters_not_supported, function(c) {RO_strike_price_technology[[get_cluster_type(c)]]}) %>% 
        setNames(clusters_not_supported)
    }
  }
  else {ro_strike_price = NULL}
  
  for (c in clusters_not_supported) {
    
    capacity_MW = market_info_supply[c,'capacity_MW']
    capacity_certified_cluster = capacity_certified[c]
    rendered_revenues_MW = 0 #initialize to not get the variable empty
    revenues_MW = market_info_supply[c,'revenues_MW'] 
    cost_om = market_info_supply[c, 'cost_om']
    
    if (reliability_options$enabled){
      production = revenues[[c]]$production
      certified_production = (production >= 0) * capacity_certified_cluster 
      
      if (!reliability_options$by_capacity){
        rendered_revenues_MW = (certified_production * pmax(as.matrix(spot-ro_strike_price), 0)) %>% colSums(.) %>%
          divide_by(capacity_MW)  %>% mean(.)
      } else {
        rendered_revenues_MW = (certified_production * pmax(as.matrix(spot-ro_strike_price[[c]]), 0)) %>% colSums(.) %>%
          divide_by(capacity_MW)  %>% mean(.)
      }
      revenues_MW = revenues_MW - rendered_revenues_MW 
    }
    
    # Compute the bid 
    bid = min(-(capacity_MW/capacity_certified_cluster) * min(revenues_MW - cost_om, 0), CRM[[zone]]$price_cap_crm$all)
    
    if (reliability_options$enabled) {
      new_row = data.frame(capacity_certified_MW = capacity_certified_cluster,
                           bid = bid,
                           revenues_crm_MW = 0, 
                           rendered_revenues_MW = rendered_revenues_MW)} 
    else {
      new_row = data.frame(capacity_certified_MW = capacity_certified_cluster,
                           bid = bid,
                           revenues_crm_MW = 0)}
    
    df = rbind(df, new_row)
  }
  
  # Bind data from energy only & capacity mechanisms
  market_info_supply = cbind(market_info_supply, df)
  
  # Nuclear peak clusters have to bid as the other nuclear plants
  market_info_supply[grepl('peak',market_info_supply$cluster_name),]$bid = market_info_supply[grepl('nuclear', market_info_supply$cluster_name),]$bid[1]
  
  #Get capacity which bids to zero
  total_studied_capacity = market_info_supply$capacity_certified_MW %>% sum(.) %>%
    {. + capacity_certified['interconnections']} # take into account interconnections

  total_capacity_zero_bid = margin  + obligation - total_studied_capacity 
  
  row_zero_bidders = data.frame(cluster_name = 'a_zero_bidders', # we use a_ only to order zero bidders and place it on the top
                                group = 'other',
                                capacity_MW = total_capacity_zero_bid, 
                                capacity_certified_MW = total_capacity_zero_bid) 
  
  market_info_supply = rbind.fill(market_info_supply, row_zero_bidders)
  market_info_supply[is.na(market_info_supply)] = 0
  
  # Ordering by decreasing order and making the cumulative sum 
  market_info_supply = market_info_supply %>% arrange(bid, cluster_name) %>% 
    mutate(capacity_cumulative = cumsum(capacity_certified_MW)) %>% set_rownames(.$cluster_name)
  
  
  return(list(market_info_supply = market_info_supply,
              ro_strike_price = ro_strike_price))
}

