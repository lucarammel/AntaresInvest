#' Mother function preparing and computing the analysis of the energy only market for every clusters.
#'
#' @param power_plants (dataframe) with costs data for clusters
#' @param clusters_not_supported (str vector) clusters interesting for the analysis of revenues and investments
#' @param data_antares antaresRead data for the antares run obtained via read_simulation_data()
#' @param area (str) country code
#'
#' @importFrom dplyr %>%
#' @importFrom reshape2 dcast
#'
#' @return revenues list of revenues for each cluster
#' @export
analysis_revenues <- function(power_plants,
                              clusters_not_supported,
                              data_antares,
                              area) {  
  print(glue('\n=> Computing energy-only, heat and service system revenues'))
  # 1. ------------------- Retrieve useful data for analysis ---------------------------#
  
  data_areas = data_antares$areas
  data_clusters = data_antares$clusters
  data_clusters_RES = data_antares$clusters_RES
  data_clusters_modulation = data_antares$clusters_modulation
  
  # 2. -------------------- Create spot price matrix with data price datatable -------#
  
  spot = data_areas %>% dcast(.,timeId~mcYear, value.var = "MRG. PRICE") %>% select(-c('timeId')) 
  
  spot[spot < 0] = 0 # Not take into account the negative prices.
  
  # 3. ------------------- Capacity retreatment knowing modulation ------------------------#
  
  data_clusters_modulation = modulate_capacity(clusters_not_supported = clusters_not_supported,
                                               power_plants = power_plants,
                                               data_clusters_modulation = data_clusters_modulation)

  # 4. ------------------- Compute revenues for each cluster ------------------------#
  revenues = lapply(clusters_not_supported, function(c) analysis_revenues_clusters(c = c,
                                                                                   power_plants = power_plants,
                                                                                   data_clusters_modulation = data_clusters_modulation,
                                                                                   data_clusters = data_clusters,
                                                                                   data_clusters_RES = data_clusters_RES,
                                                                                   data_batteries = data_antares$batteries,
                                                                                   spot = spot,
                                                                                   area = area)) %>% setNames(., clusters_not_supported)
  return(revenues)
}

#' Compute the revenues for a cluster c on the energy only market and take into account the heat remuneration
#'
#' @param c (str) cluster name
#' @param power_plants (dataframe) with costs data for clusters
#' @param data_clusters_modulation antaresRead table for custers modulation
#' @param data_clusters antaresRead data table for thermal clusters
#' @param data_clusters_RES antaresRead data table for renewables clusters
#' @param data_batteries antaresRead table for batteries
#' @param spot spot prices
#'
#' @importFrom reshape2 dcast
#' @importFrom dplyr select rename
#' @return annual & hourly revenues and hourly production for the cluster c
#' @export
analysis_revenues_clusters <- function(c,
                                       power_plants,
                                       data_clusters_modulation,
                                       data_clusters,
                                       data_clusters_RES,
                                       data_batteries,
                                       spot, 
                                       area) {       
                     
  # Capacity
  capacity = power_plants[c, 'unitcount'] * power_plants[c, 'nominalcapacity'] * (1 - power_plants[c, 'oa_share'])
  
  # Production
  cluster_type = get_cluster_type(c)
  if (!cluster_type %in% renewables_type){
    production = data_clusters[cluster == c, .(mcYear, timeId, production)] %>% 
      dcast(., timeId~mcYear, value.var = "production") %>% select(-timeId)

    # Minimum generation (partial must-run)
    min_generation = matrix(rep(data_clusters_modulation[cluster == c]$minGen, mc_number), 
                            nrow = antares_hours, byrow = FALSE)
    
    #Production sold on the energy only market
    production = pmax(as.matrix(production - min_generation), 0)
    
    #Other production (heat, steam, industrial process ...)
    production_other = min_generation

    marginal_cost = power_plants[c,"marginal_cost"] * data_clusters_modulation[cluster == c]$marginalCostModulation
  }
  else{
    production = data_clusters_RES[cluster == c, .(mcYear, timeId, production)] %>% 
      dcast(., timeId~mcYear, value.var = "production") %>% select(-timeId)

    marginal_cost = power_plants[c, 'marginal_cost']
  }
  
  if (grepl("batteries", c)) {
    # Get the battery drain 
    data_batteries = data_batteries %>% rename('soutirage' = 'FLOW LIN.')
    soutirage = data_batteries[, .(mcYear, timeId, soutirage)] %>%
      dcast(., timeId~mcYear, value.var = "soutirage") %>% select(-timeId)    
    production = production - soutirage
  }
    
  #Revenues types
  energy_revenues = energy_revenues_cluster(c = c,
                                            marginal_cost = marginal_cost,
                                            production = production, 
                                            spot = spot)

  if (!cluster_type %in% renewables_type){
    # No heat revenues for non-thermal clusters (eg renewables)
    heat_revenues = heat_revenues_cluster(c = c,
                                          heat_production = production_other, 
                                          power_plants = power_plants)
  }
  else{
    heat_revenues = matrix(0, ncol = mc_number, nrow = antares_hours)
  }

  system_service_revenues  = system_service_revenues_cluster(c = c, 
                                                             power_plants = power_plants, 
                                                             zone = area)

  #Outputs
  energy_revenues_annual_MW = colSums(energy_revenues) / capacity
  heat_revenues_annual_MW = colSums(heat_revenues) / capacity
  system_service_revenues_MW = system_service_revenues / capacity
  
  return(list(energy_revenues_annual_MW = energy_revenues_annual_MW,
              heat_revenues_annual_MW = heat_revenues_annual_MW,
              system_service_revenues_annual_MW = system_service_revenues_MW,
              production = production))
}


#' Compute energy only revenues on hourly level by cluster
#'
#' @param c (str) cluster name
#' @param marginal_cost (scalar) variable cost (ie the bid on the energy only market)
#' @param production production (hourly level)
#' @param spot spot prices (hourly level)
#'
#' @return energy only revenues (hourly level)
#' @export
energy_revenues_cluster <- function(c,
                                    marginal_cost, 
                                    production, 
                                    spot){
  #Energy only revenues
  if (grepl("chp", c)) {
    energy_revenues = production * pmax(as.matrix(spot - marginal_cost), 0)
  } else {
    energy_revenues = production * (spot - marginal_cost)
  }
  return(energy_revenues)
}

#' Compute heat revenues 
#'
#' @param c (str) cluster name
#' @param heat_production heat production (hourly level)
#' @param power_plants power_plants dataframe hypothesis
#' 
#' @importFrom magrittr set_colnames
#' @return heat revenues 
#' @export
heat_revenues_cluster <- function(c,
                                  heat_production, 
                                  power_plants){
  
  #Heat revenues computation
  heat_revenues = heat_production * power_plants[c, "heat_remuneration_euros_MWh"]
  heat_revenues %<>% set_colnames(mc_list)

  return(heat_revenues)
}

#' Service system revenue computation
#'
#' @param c (str) cluster name
#' @param power_plants (dataframe) with costs hypothesis 
#' @param zone (str) country code
#'
#' @return Revenues, annual granularity from service system 
#' @export
system_service_revenues_cluster <- function(c,
                                            power_plants, 
                                            zone){
  
  ### Service system revenues computation 
  cluster_type = get_cluster_type(c)
  
  #Get the share part of the nominal capacity of the cluster in its group
  mask = grepl(cluster_type, sapply(power_plants$cluster, function(c) get_cluster_type(c)))  
  share_in_group = power_plants[c, 'nominalcapacity'] / (power_plants[mask, 'nominalcapacity'] %>% sum(.)) 
  
  if (cluster_type %in% names(service_system_share)) {
    share_part = service_system_share[[cluster_type]] * share_in_group
    system_service_revenues = share_part * service_system_total[[zone]]
  }
  else{
    system_service_revenues = 0
  }

  return(system_service_revenues)
}

#' Update the capacity at each time knowing capacity modulation factor
#'
#' @param clusters_not_supported (str vector)
#' @param power_plants (datafrmame) with costs data for clusters
#' @param data_clusters_modulation antaresRead data.table for custers modulation
#'
#' @return data_clusters_modulation updated with capacity modulated
#' @export
modulate_capacity <- function(power_plants,
                              clusters_not_supported,
                              data_clusters_modulation) {
  
  data_clusters_modulation$minGen = 0
  data_clusters_modulation$capacity = 0
  
  for (c in clusters_not_supported){
    total_capacity = power_plants[c,"unitcount"] * power_plants[c,"nominalcapacity"] 
    data_clusters_modulation[cluster == c]$minGen = total_capacity * data_clusters_modulation[cluster == c]$minGenModulation
    data_clusters_modulation[cluster == c]$capacity = total_capacity * data_clusters_modulation[cluster == c]$capacityModulation
  }
  
  # Suppression des NA
  data_clusters_modulation$minGen[is.na(data_clusters_modulation$minGen)] = 0
  data_clusters_modulation$capacity[is.na(data_clusters_modulation$capacity)] = 0
  return(data_clusters_modulation)
}
