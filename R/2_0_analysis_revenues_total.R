

#' Mother function to compute the revenues from energy only market and capacity market
#'
#' @param year_simulated (str) year simulated
#' @param area (str) country code
#' @param power_plants (dataframe) of powerfleet hypothesis
#' @param clusters_not_supported (str vector)clusters names to compute and consider and revenues / investment decisions
#' @param data_antares antares read data from an antares run
#' @importFrom glue glue
#'
#' @return list revenues, power_plants, market_info_supply, auction_info
#' @export
analysis_revenues_total <- function(year_simulated,
                                    area,
                                    power_plants,
                                    clusters_not_supported,
                                    data_antares){
  
  # Revenues energy only
  print(glue('\n\nRevenues by cluster for [{toupper(area)}] - [{year_simulated}]'))
  revenues = analysis_revenues(power_plants = power_plants,
                               clusters_not_supported = clusters_not_supported,
                               data_antares = data_antares,
                               area = area)
  
  market_info_supply = build_market_info_supply(clusters_not_supported = clusters_not_supported,
                                                power_plants = power_plants, 
                                                revenues = revenues, 
                                                area = area)
  
  if (capacity_mechanism$enabled & area %in% names(CRM)){
    
    if (area == 'fr'){
      #Capacity market, supply and demand side then auction simulation
      print(glue("\n=> Capacity market of the year {year_simulated}.\n"))
      
      print(glue("\n==> Computing the obligation."))
      
      obligation = compute_obligation(data_antares = data_antares, 
                                      zone = area)
      
      print(glue('\n ==> Computing the availability on PP1 days.'))
      
      capacity_certified = compute_availability_total(power_plants = power_plants,
                                                      clusters_not_supported = clusters_not_supported,
                                                      data_antares = data_antares)
      
      print(glue('\n ==> Simulating the auction of capacity market.'))
      LOLD = dataset[[year_simulated]][['fr']]$data_antares$areas_annual %>% filter(area == 'fr') %>% select('LOLD') %>% as.numeric(.)
      
      out = simulation_mecanism_capacity(year_simulated = year_simulated,
                                         obligation = obligation,
                                         power_plants = power_plants,
                                         capacity_certified = capacity_certified,
                                         clusters_not_supported = clusters_not_supported,
                                         zone = area,
                                         market_info_supply = market_info_supply,
                                         LOLD = LOLD,
                                         spot = dataset[[year_simulated]][[area]]$spot,
                                         revenues = revenues)
      
      market_info_supply = out$market_info_supply
      auction_info = out$auction_info
      crm_supply_curve = out$supply_curve
      crm_demand_curve = out$demand_curve
      ro_strike_price = out$ro_strike_price
    }
    
    return(list(revenues = revenues,
                market_info_supply = market_info_supply, 
                auction_info = auction_info,
                ro_strike_price = ro_strike_price,
                crm_supply_curve = crm_supply_curve,
                crm_demand_curve = crm_demand_curve))
  }
  else{print(glue('\n=> No capacity market implemented or activated.'))}
  return(list(revenues = revenues,
              market_info_supply = market_info_supply))
}


#' Build the market info supply dataframe
#'
#' @param clusters_not_supported (str vector) clusters not supported 
#' @param power_plants (dataframe) of clusters costs hypothesis
#' @param revenues list of dataframe revenues by cluster  
#'
#' @return market info on the supply side
#' @export
build_market_info_supply <- function(clusters_not_supported,
                                     power_plants, 
                                     revenues, 
                                     area){
  
  market_info_supply = data.frame(cluster_name = character(),
                                  group = character(),
                                  revenues_MW = numeric(),
                                  cost_om = numeric(), 
                                  energy_revenues_MW = numeric(),
                                  heat_revenues_MW = numeric(),
                                  system_service_revenues_MW = numeric(),
                                  capacity_MW = numeric())
  
  for (c in clusters_not_supported) {
    # Get the cluster group
    cluster_group = get_cluster_type(c)
    
    #Compute the capacity of the cluster without OA (ie part of the production used not for power production)
    capacity_MW = (power_plants[c,"unitcount"]*(1 - power_plants[c,"oa_share"]))*power_plants[c,"nominalcapacity"]
    
    #Compute operation costs from margins
    cost_om = power_plants[c,"operational_cost_euros_MW_year"] 
    
    #Compute energy only revenues, cvar revenues 
    revenues_MW = sort(revenues[[c]]$energy_revenues_annual_MW + revenues[[c]]$heat_revenues_annual_MW + revenues[[c]]$system_service_revenues_annual_MW)
    revenues_cvar_MW = mean(revenues_MW[1:floor(quantile_cvar * mc_number)])
    
    #Compute the bid 
    new_row = data.frame(cluster_name = c,
                         group = cluster_group,
                         revenues_MW = mean(revenues_MW),
                         cost_om = cost_om, 
                         energy_revenues_MW = mean(revenues[[c]]$energy_revenues_annual_MW),
                         heat_revenues_MW = mean(revenues[[c]]$heat_revenues_annual_MW),
                         system_service_revenues_MW = revenues[[c]]$system_service_revenues_annual_MW,
                         capacity_MW = capacity_MW)           
    
    market_info_supply = rbind(market_info_supply, new_row)
  }
  
  rownames(market_info_supply) = market_info_supply$cluster_name
  
  if (!(capacity_mechanism$enabled & area %in% names(CRM))) market_info_supply$revenues_crm_MW = 0
  if (!capacity_mechanism$enabled | !reliability_options$enabled) market_info_supply$rendered_revenues_MW = 0
  return(market_info_supply)
}
