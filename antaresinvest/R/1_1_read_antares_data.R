#' Read the simulation data from the antares_output_name run
#'
#' @param study_id (str) the name of the antares simulation
#' @param antares_output_name  (str) name of the output antares corresponding to a run.
#' @param zone (str) country code
#' 
#' @importFrom glue glue
#' @import antaresRead
#' @importFrom antaresRead readAntares setSimulationPath setSimulationPathAPI 
#' @importFrom dplyr %>% select
#'
#' @return data_antares
#' @export
read_simulation_data <- function(study_id,
                                 antares_output_name,
                                 zone){
  
  if (!use_api) antares_output_name = check_antares_output(study_id = study_id,
                                                           antares_output_name = antares_output_name)
  
  ###------------------------------ Set simulation path ----------------------------##
  opts = set_simulation_antares(study_id = study_id, 
                                input_mode = FALSE, 
                                antares_output_name = antares_output_name)
  
  ###------------------------------ READ Cluster data ------------------------------###  
  cluster_description = readClusterDesc(opts) %>% filter(area == zone)
  cluster_description_RES = readClusterResDesc(opts) %>% filter(area == zone)
  
  ### Read data clusters ###
  print(glue::glue("\n=> Reading clusters data."))
  data_clusters = readAntares(opts = opts, 
                              mcYears = mc_list, 
                              clusters = zone, 
                              clustersRes = zone,
                              showProgress = FALSE) 
  
  data_clusters_RES = data_clusters %>% .$clustersRes %>% select(-c("day", "month", "hour"))
  data_clusters = data_clusters %>% .$clusters %>% select(-c("day", "month", "hour"))
  
  ### Read clusters modulation ###
  print(glue::glue("\n=> Reading clusters modulation."))
  clusters_modulation = readAntares(opts = opts, 
                                    clusters = zone, 
                                    thermalModulation=TRUE, 
                                    mustRun=TRUE, 
                                    showProgress = FALSE) %>% select(-c("day", "month", "hour"))
  
  ### Read AREAS ###
  print(glue::glue("\n=> Reading areas hourly."))
  variable_production = c('SOLAR PV', 'SOLAR ROOFT', 'SOLAR CONCRT.' ,'WIND ONSHORE', 'WIND OFFSHORE' ,'NUCLEAR', 'COAL','GAS', 
                          'OIL', 'H. STOR', 'LIGNITE', 'H. ROR','MISC. DTG', 'MIX. FUEL', 'MISC. NDG')
  variable_to_get = c(variable_production,'mcYear', 'timeId', 'LOAD', 'MRG. PRICE', 'LOLD', 'UNSP. ENRG', 'SPIL. ENRG', 'BALANCE')
  data_areas_hourly = readAntares(opts = opts, 
                                  mcYears = mc_list,
                                  areas = zone, 
                                  timeStep = "hourly", 
                                  select = variable_to_get, 
                                  showProgress = FALSE)  %>% select(-c("day", "month", "hour"))
  
  
  print(glue::glue("\n=> Reading areas annual."))
  variable_to_get = c('UNSP. ENRG', 'LOLD', 'LOAD', 'MRG. PRICE')
  data_areas_annual = readAntares(opts = opts, 
                                  areas = zone,
                                  select = variable_to_get, 
                                  timeStep = "annual", 
                                  showProgress = FALSE) %>%
    .[, lapply(.SD[, variable_to_get, with = F], mean), by = list(area, timeId, time)]
  
  ### Read Links ###
  print(glue::glue("\n => Reading links."))
  links_neighbours = get_neighbours_links(area = zone, study_id = study_id)
  data_links = readAntares(opts = opts,
                           links = links_neighbours, 
                           timeStep = "hourly", 
                           showProgress = FALSE)
  
  ### Read batteries LINKS ###
  if (grepl('batteries',cluster_description$cluster %>% as.vector(.)) %>% sum(.) >= 1){
    data_batteries = readAntares(opts = opts,
                                 mcYears = mc_list, 
                                 links=as.character(glue("{zone} - {batteries_country}")), 
                                 select=c('mcYear', 'timeId', 'FLOW LIN.'),
                                 showProgress = FALSE) %>% select(-c("day", "month", "hour"))
  } else { data_batteries = data.frame() }
  
  ###------------ Build out list to return all the dataframes --------###
  data_antares = list(cluster_description = cluster_description,
                      cluster_description_RES = cluster_description_RES,
                      clusters = data_clusters,
                      clusters_RES = data_clusters_RES,
                      clusters_modulation = clusters_modulation,
                      areas = data_areas_hourly,
                      links = data_links,
                      areas_annual = data_areas_annual,
                      batteries = data_batteries)
  
  ###------------ Return a list with the data --------###
  if (capacity_mechanism$enabled & zone %in% names(CRM)){
    setRam(80)
    print(glue('\n=> Reading availabilities.'))
    data_antares$thermal_availability = readAntares(opts = opts, 
                                                    mcYears = mc_list,
                                                    clusters = zone, 
                                                    thermalAvailabilities = TRUE,
                                                    showProgress = FALSE)
    
  }
  return(data_antares)
}


#' Build the costs clusters dataframe
#'
#' @param study_id (str) id of the simulation antares
#' @param zone (str) country code
#' @param power_plants (df) list dataframe of the hypothesis of the powerfleet 
#'
#' @import yaml
#' @importFrom glue glue
#' @importFrom magrittr %>%
#'
#' @return updated powerplants hypothesis dataframe
#' @export
get_costs_clusters <- function(zone, 
                               power_plants){
  
  cost_path = paste(hypothesis_dir, zone, 'costs.yaml', sep = '/')
  cfg_file = yaml::read_yaml(cost_path)
  costs = cfg_file$costs
  cluster_list = power_plants$cluster %>% as.vector(.)
  
  for (c in cluster_list) {
    cluster_type = get_cluster_type(c)
    
    if (cluster_type %in% names(costs)) {
      power_plants[c, 'investment_cost_euros_MW_year'] = costs[[cluster_type]]$investment_cost
      power_plants[c, 'operational_cost_euros_MW_year'] = costs[[cluster_type]]$operational_cost
      power_plants[c, 'construction_period_year'] = costs[[cluster_type]]$construction_period
      power_plants[c, 'heat_remuneration_euros_MWh'] = costs[[cluster_type]]$heat_remuneration
      power_plants[c, 'oa_share'] = costs[[cluster_type]]$oa_share
      
      if(grepl('dsr', c)) {
        capacity = power_plants[c, 'nominalcapacity'] * power_plants[c, 'unitcount']
        om_costs = compute_dsr_operation_cost(capacity, cfg_file$cost_dsr)
        power_plants[c,"operational_cost_euros_MW_year"] = om_costs
      }
    }
  }
  power_plants[is.na(power_plants)] = 0
  
  return(power_plants)
}


#' Build the clusters interesting list 
#'
#' @param zone (str) country code
#' @param cluster_list (str vector) dataframe with the cluster of the zone and their name
#' @return clusters_filters for one year simulated (the one of data_antares)
#'
#' @importFrom dplyr %>% filter
#' @import yaml
#' @importFrom magrittr %<>% 
#' @export
get_clusters_filters <- function(zone,
                                 cluster_list){
  path_invest_filters = paste(hypothesis_dir, zone, 'investment_filters.yaml', sep = '/')
  invest_filters = yaml::read_yaml(path_invest_filters)
  
  clusters_filters = list()
  
  clusters_to_avoid = c(invest_filters$specific_clusters_to_avoid, cluster_list[grepl(paste(pattern_to_avoid, collapse = '|'), cluster_list %>% .$cluster),])
  #Completing clusters_filters with not_supported clusters (ie interesting for analysis of investments and revenues), supported (exlcuded from revenues analysis)
  cluster_list$cluster_type = sapply(cluster_list$cluster,get_cluster_type)
  
  clusters_filters$supported = cluster_list %>% filter(cluster_type %in% invest_filters$clusters_supported) %>% .$cluster %>% as.vector(.)
  clusters_filters$not_supported = cluster_list %>% filter(cluster_type %in% invest_filters$clusters_not_supported, !cluster %in% clusters_to_avoid) %>% .$cluster %>% as.vector(.)
  
  if(static_mode$enabled){
    clusters_filters$not_supported = c(clusters_filters$not_supported, cluster_list %>% filter(cluster_type %in% static_mode$cluster_to_consider) %>% .$cluster %>% as.vector(.))
    clusters_filters$not_supported %<>% .[!duplicated(.)]
  }
  
  clusters_filters$candidates_investment = cluster_list %>% filter(cluster_type %in% invest_filters$candidates_investment, !cluster %in% clusters_to_avoid) %>% .$cluster %>% as.vector(.) #add groups
  clusters_filters$candidates_investment %<>% c(.,  invest_filters$candidates_investment %>% .[sapply(.,function(x) {x %>% strsplit(., '_') %>% unlist(.) %>% .[1] %>% {. == zone}})]) #add specific clusters
  clusters_filters$candidates_investment %<>% .[. %in% clusters_filters$not_supported]
  clusters_filters$candidates_investment %<>% .[!duplicated(.)]
  
  clusters_filters$candidates_retirement = cluster_list %>% filter(cluster_type %in% invest_filters$candidates_retirement, !cluster %in% clusters_to_avoid) %>% .$cluster %>% as.vector(.)
  clusters_filters$candidates_retirement %<>% c(.,  invest_filters$candidates_retirement %>% .[sapply(.,function(x) {x %>% strsplit(., '_') %>% unlist(.) %>% .[1] %>% {. == zone}})])
  clusters_filters$candidates_investment %<>% .[. %in% clusters_filters$not_supported]
  clusters_filters$candidates_retirement %<>% .[!duplicated(.)] 
  
  #Add to supported, every clusters we want to avoid. 
  clusters_filters$supported = c(clusters_filters$supported, clusters_to_avoid)  
  
  return(clusters_filters)
}

#' Check if antares_output_name is coherent with launch time of antares run
#'
#' @param study_id (str) 
#' @param antares_output_name (str)
#'
#' @return None
check_antares_output <- function(study_id,
                                 antares_output_name){
  ### Code made for any problem of delay between run and call to run of antares -> 1min gap.
  check = file.exists(paste(antares_simu_dir, study_id, 'output', antares_output_name, sep = '/'))
  if (!check){
    print(glue("\n===> outputs : [{antares_output_name}] doesn't exist."))
    outputs_split = antares_output_name %>% strsplit(., split = '-') %>% .[[1]]
    time =  outputs_split %>% .[grepl(substr(antares_mode, 1,3), .)] %>% strsplit(., split = substr(antares_mode, 1,3)) %>% .[[1]] %>% as.numeric(.)
    hour = time %/% 100
    minute =  time - (time %/% 100) * 100
    if (minute == 59){
      minute = '00'
      hour %<>% {. + 1} %>% as.character(.)
      time = paste(hour,minute, sep = '')
    }
    else{
      time %<>% {. + 1} %>% as.character(.)
    }
    outputs_split[2] = as.character(glue::glue('{time}{substr(antares_mode, 1,3)}'))
    antares_output_name = paste(outputs_split, collapse = '-')
    print(glue("\n===> Trying new output name : [{antares_output_name}]. In case no adequacy due to delay between call and run antares."))
  }
  return(antares_output_name)
}



#' Get the cluster type of any antares cluster based on its name. Mandatory to determine this cluster cost automatically
#'
#' @param cluster (str) name of cluster e.g fr_oil_chp
#' @importFrom magrittr %<>% 
#' @import reshape2
#'
#' @return cluster_type (str): the type of cluster
#' @export
get_cluster_type <- function(cluster){
    
    naming_params = reshape2::melt(naming_params) %>% .[,1:2] %>% setNames(., c('cluster', 'cluster_type'))
    cluster %<>% gsub('_',' ', .)
    mask = sapply(naming_params$cluster, function(c) grepl(glue('\\<{c}\\>'), cluster)) %>% as.vector(.)
    if (sum(mask) == 0) return(NA)
    
    cluster_type = naming_params %>% filter(cluster == naming_params$cluster[mask]) %>% select('cluster_type') %>% as.character(.)
    
    # Add peculiarities to the cluster type.
    mask = sapply(peculiarities, function(c) grepl(c, cluster)) %>% as.vector(.)
    if (sum(mask) == 0) return(cluster_type)
    cluster_type = paste(cluster_type, peculiarities[mask] , sep = '_')
    
    return(cluster_type)
}

#' Compute the om costs of dsrs
#'
#' @param capacity capacity 
#' @param cost_dsr table of dsrs costs. 
#'
#' @return om cost of dsrs
#' @export
compute_dsr_operation_cost <- function(capacity,
                                       cost_dsr){
  
  i=1
  operation_cost = 0
  while(capacity > 0){
    if(capacity>(cost_dsr$power_cumulated)[i]){
      operation_cost = operation_cost + (cost_dsr$power_cumulated)[i]*(cost_dsr$cost)[i]
    }
    else{
      operation_cost = operation_cost + (cost_dsr$cost)[i]*capacity
    }
    capacity = capacity - (cost_dsr$power_cumulated)[i]
    i=i+1
  }
  return(operation_cost/capacity)
}

