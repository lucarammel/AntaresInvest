#' Make power_plants dataframe based on thermal clusters list ini file and returns it.
#'
#' @param area (str) country code
#' @param study_id (str) id of the year simulated
#' @param year_simulated (str) year simulated (for verbose)
#'
#' @importFrom glue glue
#' @importFrom antaresRead setSimulationPath setSimulationPathAPI
#' @importFrom magrittr %>% %<>% set_rownames
#' @importFrom plyr rbind.fill
#' 
#' @return power_plants dataframe
#' @export
make_powerplants_from_antares <- function(study_id,
                                          year_simulated,
                                          zone){
  
  print(glue("=> Making power plants dataframe : {year_simulated} - {toupper(zone)} \n"))
  
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  power_plants = readClusterDesc(opts) %>% filter(area == zone) %>% as.data.frame(.) %>% set_rownames(.$cluster) %>%
    mutate(enabled = NULL, area = NULL, group = sapply(rownames(.), get_cluster_type)) %>% 
    replace(is.na(.), FALSE) %>% setNames(lapply(names(.), gsub, pattern = '.', replacement = '_', fixed = TRUE))
  
  power_plants_RES = readClusterResDesc(opts) %>% filter(area == zone) %>% as.data.frame(.) %>% set_rownames(.$cluster) %>%
    mutate(enabled = NULL, area = NULL, ts.interpretation = NULL, group = sapply(rownames(.), get_cluster_type)) %>% 
    replace(is.na(.), FALSE) %>% setNames(lapply(names(.), gsub, pattern = '.', replacement = '_', fixed = TRUE))
  
  power_plants = rbind.fill(power_plants, power_plants_RES) %>% replace(is.na(.), FALSE) %>% set_rownames(., .$cluster)
  
  power_plants = get_costs_clusters(zone = zone,
                                    power_plants = power_plants)
  
  return(power_plants)
}

#' Remove power plant mother function from power_plants dataframe and from the simulation source files.
#'
#' @param capacity_name (str) name of the capacity to remove
#' @param unit_count  (int) units to remove
#' @param power_plants (dataframe) with the power fleetfrom make_powerplants_from_antares()
#' @param area (str) country code
#' @param year_simulated (int) year of simulation to update
#' @param study_id (str) antares simulation id
#'
#'
#' @return power_plants dataframe updated
#' @export
remove_powerplant <- function(capacity_name,
                              unit_count,
                              power_plants,
                              area,
                              year_simulated,
                              study_id){
  
  ###Compute new unitcount of the cluster for every other clusters. 
  old_unitcount = power_plants[(power_plants$cluster == capacity_name),]$unitcount
  new_unitcount = as.integer(as.integer(old_unitcount) - unit_count)
  
  if (new_unitcount <= 0) {
    ### Remove into the simulation antares ###
    power_plants = retirement_capacity(area = area, 
                                       capacity_name = capacity_name, 
                                       study_id = study_id,
                                       old_unitcount = old_unitcount, 
                                       year_simulated = year_simulated)
  } else {
    ### Remove only few units 
    power_plants = remove_production_unit(area = area, 
                                          capacity_name = capacity_name, 
                                          study_id = study_id, 
                                          unitcount = unit_count, 
                                          new_unitcount = new_unitcount,
                                          year_simulated = year_simulated)
  }
  return(power_plants)
}

#' Retire completely a capacity 
#'
#' @param capacity_name (str) name of the capacity
#' @param area (str) country code
#' @param old_unitcount (int) old unit count before retirement
#' @param study_id (str) antares simulation id
#' @param year_simulated (str) year simulated
#' 
#' @importFrom antaresEditObject editCluster editClusterRES
#'
#' @return powerplants dataframe updated
#' @export
retirement_capacity <- function(capacity_name, 
                                area, 
                                old_unitcount, 
                                study_id, 
                                year_simulated){
  
  print(glue('\n\n=> [{toupper(area)}] - REMOVING cluster ({capacity_name}) in simulation |{year_simulated}|'))
  
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  cluster_type = get_cluster_type(capacity_name)
  if (cluster_type %in% renewables_type){
    editClusterRES(area = area,
                   cluster_name = capacity_name,
                   unitcount = as.integer(0),
                   add_prefix = FALSE,
                   enabled = FALSE,
                   opts = opts)
  }
  else{
    ### Change unitcount
    editCluster(area = area,
                cluster_name = capacity_name,
                unitcount = as.integer(0),
                add_prefix = FALSE,
                enabled = FALSE,
                opts = opts)
    
    ### Change timeseries 
    if(old_unitcount != 0){
      edit_ts_input_cluster(study_id = study_id,
                            area = area,
                            capacity_name = capacity_name,
                            value = round(1/old_unitcount,2))
    }
  }
  ###Remove into the dataframe R
  power_plants[(power_plants$cluster == capacity_name),]$unitcount = as.integer(0)
  
  return(power_plants)
}

#' Decomission n units of a capacity
#'
#' @param unitcount (int) unitcount decomissioned
#' @param capacity_name (str) name of the capacity
#' @param area (str) country code
#' @param new_unitcount (int) new unit count before retirement
#' @param study_id (str) antares simulation id
#' @param year_simulated (str) year simulated
#' 
#' @importFrom antaresEditObject editCluster editClusterRES
#' @importFrom glue glue
#'
#' @return powerplants dataframe updated
#' @export
remove_production_unit <- function(area, 
                                   unitcount, 
                                   new_unitcount,
                                   capacity_name, 
                                   study_id, 
                                   year_simulated){
  
  print(glue('\n\n=> [{toupper(area)}] - DECOMISSIONING {unit_count} units for cluster [{capacity_name}] in simulation |{year_simulated}|'))
  
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  cluster_type = get_cluster_type(capacity_name)
  if (cluster_type %in% renewables_type){
    
    editClusterRES(area = area,
                   cluster_name = capacity_name,
                   unitcount = new_unitcount,
                   add_prefix = FALSE,
                   enabled = TRUE,
                   opts = opts)
  }
  else{
    editCluster(area = area,
                cluster_name = capacity_name,
                unitcount = new_unitcount,
                add_prefix = FALSE,
                enabled = TRUE,
                opts = opts)
    
    ### Editing time series by changing max capacity => new_max_capacity = (new_unitcount/old_unitcount)*max_capacity
    coefficient = round(new_unitcount/old_unitcount,2)
    
    edit_ts_input_cluster(study_id = study_id,
                          area = area,
                          capacity_name = capacity_name,
                          value = coefficient)
  }
  ###Edit the cluster into the dataframe R
  power_plants[(power_plants$cluster == capacity_name),]$unitcount = new_unitcount
  
  return(power_plants)
}

#' Add power plant in power_plants dataframe and in the simulation source files.
#'
#' @param capacity_name (str) name of the new capacity. If already exists, build a new one with an specific id as suffix.
#' @param unit_count (scalar) units to add
#' @param power_plants dataframe with the power fleet from make_powerplants_from_antares()
#' @param power_plants_actual dataframe with the power fleet actual from make_powerplants_from_antares()
#' @param area (str) country code
#' @param study_id (str) antares simulation id
#' @param year_actual (int) current simulated year
#' @param s (int) loop
#'
#' @importFrom glue glue
#' @importFrom magrittr %<>% %>% 
#'
#' @return power_plants updated
#' @export
add_powerplant <- function(capacity_name,
                           unit_count,
                           area,
                           year_simulated,
                           study_id,
                           power_plants){
  
  print(glue('\n\n=> [{toupper(area)}] - INVESTING {unit_count} units for cluster [{capacity_name}] in simulation |{year_simulated}|'))
  
  ### Reformat capacity_name & extract cluster info
  cluster_info = power_plants[(power_plants$cluster == capacity_name),]
  
  ###Compute the new unitcount of the existing cluster
  old_unitcount = cluster_info$unitcount
  new_unitcount = as.integer(as.integer(old_unitcount) + unit_count)
  
  if (new_unitcount > 100) {
    #----------------  Exception added because Antares doesn't support cluster with unit > 100 ------#
    #------------- We have to create a new cluster --------------------------------------------------#
    ## First, generate new capacity name
    new_capacity_name = generate_new_capacity_name(capacity_name = capacity_name, power_plants = power_plants)
    
    ## Compute coefficient which will be used to create the new time series of new cluster
    coefficient  = (new_unitcount - 100)/old_unitcount
    
    ### Extract time series to create the thermal cluster
    cluster_type = get_cluster_type(capacity_name)
    if (!cluster_type %in% renewables_type){
      ts_cluster = extract_ts_clusters(capacity_name = capacity_name,
                                       study_id = study_id, coefficient = coefficient)
    }
    else{
      ts_cluster = extract_ts_clusters(capacity_name = capacity_name,
                                       study_id = study_id, coefficient = 1) #Not MWh but load factor for RES
    }
    
    power_plants = create_new_powerplant(study_id = study_id,
                                         area = area,
                                         capacity_name = capacity_name,
                                         old_unitcount = old_unitcount,
                                         cluster_info = cluster_info,
                                         ts_cluster = ts_cluster,
                                         new_capacity_name = new_capacity_name,
                                         new_unitcount = new_unitcount,
                                         power_plants = power_plants)
  } else if (old_unitcount == 0){
    power_plants = create_cluster_from_existing(area = area,
                                                study_id = study_id,
                                                capacity_name = capacity_name,
                                                new_unitcount = new_unitcount,
                                                power_plants = power_plants,
                                                study_id = study_id)
  }
  else{
    power_plants = add_production_unit(study_id = study_id,
                                       area = area,
                                       capacity_name = capacity_name,
                                       new_unitcount =  new_unitcount,
                                       old_unitcount = old_unitcount,
                                       power_plants = power_plants)
  }
  return(power_plants)
}

#' Add a new power plant to a simulation and update data structures.
#'
#' @param study_id (str) Simulation id
#' @param study_id (str) The ID of the study
#' @param area (str) The country code
#' @param capacity_name (str) The name of the new capacity If the capacity already exists, a new one will be created with a specific ID as a suffix
#' @param old_unitcount (int) The number of units in the old capacity
#' @param cluster_info (data frame) A data frame containing information about the cluster
#' @param ts_cluster (list) Time series data for the cluster
#' @param new_capacity_name (str) The name of the new capacity
#' @param new_unitcount (int) The number of units in the new capacity
#' @param power_plants (data frame) A data frame containing information about the power plants in the simulation
#'
#' @importFrom antaresEditObject createCluster editCluster editClusterRES createClusterRES
#' @importFrom magrittr %<>% %>% set_rownames
#'
#'
#' @return A updated version of the `power_plants` data frame.
create_new_powerplant <- function(study_id,
                                  area,
                                  capacity_name,
                                  old_unitcount,
                                  cluster_info,
                                  ts_cluster,
                                  new_capacity_name,
                                  new_unitcount,
                                  power_plants){
  
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  #--------------------- createCluster with the remaining unitcount --------------------------#
  print(glue('\n ==> Creating new powerplant {new_capacity_name} from existant one :{capacity_name} because unitcount limit'))
  cluster_type = get_cluster_type(new_capacity_name)
  
  if (!cluster_type %in% renewables_type){
    createCluster(area = area,
                  cluster_name = new_capacity_name,
                  unitcount = as.integer(new_unitcount - 100),
                  group = cluster_info$group,
                  nominalcapacity = cluster_info$nominalcapacity,
                  co2 = cluster_info$co2,
                  marginal_cost = cluster_info$marginal_cost,
                  market_bid_cost = cluster_info$market_bid_cost,
                  spread_cost = cluster_info$spread_cost,
                  min_up_time = cluster_info$min_up_time,
                  min_down_time = cluster_info$min_down_time,
                  startup_cost = cluster_info$startup_cost,
                  min_stable_power = cluster_info$min_stable_power,
                  spinning = cluster_info$spinning,
                  add_prefix = FALSE, 
                  enabled = TRUE,
                  time_series = ts_cluster$time_series,
                  prepro_data = ts_cluster$prepro_data, 
                  prepro_modulation = ts_cluster$prepro_modulation,
                  opts = opts)
  } else {
    createClusterRES(area = area, 
                     cluster_name = new_capacity_name,
                     group = cluster_info$group,
                     unitcount = as.integer(new_unitcount - 100), 
                     nominalcapacity = cluster_info$nominalcapacity, 
                     ts_interpretation = 'production-factor', 
                     time_series = ts_cluster, 
                     add_prefix = FALSE)
  }
  
  # Update the power_plants dataframe
  new_power_plant = cluster_info %>% mutate(cluster = new_capacity_name, unitcount = new_unitcount - 100)
  
  power_plants %<>% rbind(., new_power_plant) %>% set_rownames(., .$cluster) 
  power_plants[is.na(power_plants)] = FALSE
  
  cluster_type = get_cluster_type(capacity_name)
  
  if (cluster_type %in% renewables_type){
    editClusterRES(area = area,
                   cluster_name = capacity_name,
                   unitcount = as.integer(100),
                   add_prefix = FALSE,
                   enabled = TRUE,
                   opts = opts)
  } else{
    editCluster(area = area,
                cluster_name = capacity_name,
                unitcount = as.integer(100),
                add_prefix = FALSE,
                enabled = TRUE,
                opts = opts)
    
    ### Update timeseries
    
    coefficient = 100 / old_unitcount
    edit_ts_input_cluster(study_id = study_id,
                          area = area,
                          capacity_name = capacity_name,
                          value = coefficient)
  }
  power_plants[(power_plants$cluster == capacity_name),]$unitcount = 100
  return(power_plants)
}

#' Add a new production unit to a power plant and update data structures
#'
#' This function adds a new production unit to a power plant and updates the corresponding data structures
#'
#' @param study_id (str) The ID of the study
#' @param area (str) The country code
#' @param capacity_name (str) The name of the capacity to which the new unit will be added
#' @param new_unitcount (int) The total number of units in the capacity after adding the new unit
#' @param old_unitcount (int) The number of units in the old capacity
#' @param power_plants (data frame) A data frame containing information about the power plants in the simulation
#'
#' @importFrom antaresEditObject editCluster editClusterRES
#'
#' @return A updated version of the `power_plants` data frame
#' @export
add_production_unit <- function(study_id,
                                area,
                                capacity_name,
                                new_unitcount,
                                old_unitcount,
                                power_plants){
  
  print(glue('\n ==> Adding unitcounts to existing powerplant {capacity_name}'))          
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  cluster_type = get_cluster_type(capacity_name)
  
  if (cluster_type %in% renewables_type){
    editClusterRES(area = area,
                   cluster_name = capacity_name,
                   unitcount = new_unitcount,
                   add_prefix = FALSE,
                   enabled = TRUE,
                   opts = opts)
  }
  else{
    editCluster(area = area,
                cluster_name = capacity_name,
                unitcount = new_unitcount,
                add_prefix = FALSE,
                enabled = TRUE,
                opts = opts)
    
    ### Editing time series by changing max capacity => new_max_capacity = (new_unitcount/old_unitcount)*max_capacity
    coefficient = round(new_unitcount/old_unitcount,2)
    edit_ts_input_cluster(study_id = study_id,
                          area = area,
                          capacity_name = capacity_name,
                          value = coefficient)
  }
  ###Edit the cluster in the R dataframe
  power_plants[(power_plants$cluster == capacity_name),]$unitcount = new_unitcount
  return(power_plants)  
}

#' Add a new power plant to a simulation and update data structures.
#'
#' @param study_id (str) The ID of the study
#' @param area (str) The country code
#' @param capacity_name (str) The name of the new capacity If the capacity already exists, a new one will be created with a specific ID as a suffix
#' @param new_unitcount (int) The number of units in the new capacity
#' @param power_plants (data frame) A data frame containing information about the power plants in the simulation
#'
#' @importFrom antaresEditObject editCluster editClusterRES
#'
#' @return A updated version of the `power_plants` data frame.
create_cluster_from_existing <- function(area,
                                         study_id,
                                         capacity_name,
                                         new_unitcount, 
                                         power_plants){
  
  print(glue('\n ==> Creating a new powerplant from existant {capacity_name}'))
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  cluster_type = get_cluster_type(capacity_name)
  
  if (cluster_type %in% renewables_type){
    editClusterRES(area = area,
                   cluster_name = capacity_name,
                   unitcount = new_unitcount,
                   add_prefix = FALSE,
                   enabled = TRUE,
                   opts = opts)
  }else{
    editCluster(area = area,
                cluster_name = capacity_name,
                unitcount = new_unitcount,
                opts = opts,
                add_prefix = FALSE,
                enabled = TRUE)
    
    ### Editing time series by multiply it by new_unictout / 1 
    edit_ts_input_cluster(study_id = study_id,
                          area = area,
                          capacity_name = capacity_name,
                          value = new_unitcount)
  }
  
  ###Edit the cluster in the R dataframe
  power_plants[(power_plants$cluster == capacity_name),]$unitcount = new_unitcount
  return(power_plants)
}


#' Update the power_plants dataframe using the power_plants_moves dataframe
#'
#' @param power_plants (dataframe) table with the power fleet
#' @param power_plants_actual (dataframe) table with the power fleet actual
#' @param area (str) country code
#' @param year_simulated (int) year simulated
#' @param year_actual (int) current simulated year
#' @param s loop
#' 
#' @importFrom dplyr %>% filter
#'
#' @return power_plants updated with power_plants_moves
#' @export
update_powerplants <- function(power_plants,
                               area,
                               year_simulated,
                               year_actual,
                               study_id,
                               s){
  if(nrow(decisions[[area]]$power_plants_moves) == 0){
    return(power_plants)
  }
  if (year_actual > year_simulated){
    return(power_plants) ## Does not make sense to edit a year that is previous the current year 
  }
  
  id_move = c() #Get the rows used stored. 
  
  #Filter for thermal and retirement
  filter = decisions[[area]]$power_plants_moves %>% filter(movement == 'retirement', year_decision <= year_simulated - retirement_time, done == FALSE)
  if (nrow(filter) > 0){
    for (row in rownames(filter)){
      power_plants = remove_powerplant(power_plants = power_plants,
                                       capacity_name = as.character(filter[row, 'name']),
                                       unit_count = as.numeric(filter[row, 'unitcount']),
                                       area = area,
                                       year_simulated = year_simulated,
                                       study_id = study_id)
      
      id_move = c(id_move, filter[row, 'id_move'])
    }
  }
  #Filter for thermal and investment
  filter = decisions[[area]]$power_plants_moves %>% filter(movement == 'investment', year_start <= year_simulated, done == FALSE)
  if (nrow(filter) > 0){
    for (row in rownames(filter)){
      power_plants = add_powerplant(capacity_name = as.character(filter[row, 'name']),
                                    unit_count = as.integer(filter[row, 'unitcount']),
                                    area = area,
                                    year_simulated = year_simulated,
                                    study_id = study_id,
                                    power_plants = power_plants)
      
      id_move = c(id_move, filter[row, 'id_move'])
    }
  }
  
  if (year_simulated == time_horizon){
    decisions[[area]]$power_plants_moves[decisions[[area]]$power_plants_moves$id_move %in% id_move,'done'] <<- TRUE #Set as done the movement
  }
  
  return(power_plants)
}

