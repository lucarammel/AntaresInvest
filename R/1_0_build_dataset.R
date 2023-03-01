#' Mother function building all data from antares and hypothesis
#'
#' @param year_simulated (str) year simulated to get antares outputs
#' @param antares_output_name (str) name of the output antares corresponding to a run.
#' @param area (str) country code
#' @param study_id (str) simulation name or id API
#'
#' @importFrom glue glue
#' @return margins and data_antares and clusters filters
#' @export
build_dataset <- function(year_simulated,
                          power_plants,
                          antares_output_name,
                          area, 
                          study_id) {
  
  print(glue('\n\nReading antares output of the simulation: {year_simulated} - [{toupper(area)}] - loop: {as.character(s)} '))
  
  ###Build antares data table from output simulation run
  data_antares = read_simulation_data(study_id = study_id,
                                      antares_output_name = antares_output_name,
                                      zone = area)
  
  cluster_list = power_plants %>% filter(unitcount > 0 & nominalcapacity > 0) %>% rownames(.) %>% 
    as.data.frame(.) %>% setNames(., 'cluster')
 
  clusters_filters = get_clusters_filters(zone = area,
                                          cluster_list = cluster_list)
 
  spot = data_antares$areas %>% dcast(.,timeId~mcYear, value.var = "MRG. PRICE") %>% select(-c('timeId'))
  
  load = data_antares$areas %>% dcast(.,timeId~mcYear, value.var = "LOAD") %>% select(-c('timeId'))
  
  if (static_mode$enabled){ 
    energy_not_served = data_antares$areas %>% dcast(.,timeId~mcYear, value.var = "UNSP. ENRG") %>% select(-c('timeId'))
    
    variable_production = c('SOLAR PV', 'SOLAR ROOFT', 'SOLAR CONCRT.' ,'WIND ONSHORE', 'WIND OFFSHORE' ,'NUCLEAR', 'COAL',
                            'GAS', 'OIL', 'H. STOR', 'LIGNITE', 'H. ROR','MISC. DTG', 'MIX. FUEL', 'MISC. NDG')
    
    data_antares$areas %<>% mutate('PROD. TOTAL' = rowSums(.[,..variable_production])) # '..' is to select columns 
    production_total_hourly = data_antares$areas %>% dcast(.,timeId~mcYear, value.var = "PROD. TOTAL") %>% select(-c('timeId'))
    
    balance_hourly = data_antares$areas %>% dcast(.,timeId~mcYear, value.var = "BALANCE") %>% select(-c('timeId'))
    
  }
  else {
    energy_not_served = NULL
    production_total_hourly = NULL
    balance_hourly = NULL
  }
  
  return(list(clusters_filters = clusters_filters,
              data_antares = data_antares,
              load = load, 
              spot = spot,
              energy_not_served = energy_not_served, 
              balance_hourly = balance_hourly, 
              production_total_hourly = production_total_hourly))
  
}

#' Parallelized load of the dataset for a list of years_to_simulate and areas
#'
#' @param years_to_simulate (int vector) Years to simulate
#' @param areas (str vector) Areas
#'
#' @importFrom magrittr %>%
#'
#' @return dataset
#' @export
parallelisation_build_dataset <- function(years_to_simulate,
                                          areas){
  ### Export all the functions and all the variables required for the parallel loop
  varlist = c(ls(envir=.GlobalEnv) %>% .[! . %in% c("dataset","dataset_revenues")], ls("package:antaresinvest"))
  areas_verbose = sapply(areas,function(a) glue::glue("[{a}]")) %>% as.vector(.) %>% paste(.,collapse = '-')
  print(glue::glue("\n\n Loading dataset for {toupper(areas_verbose)} - ({years_to_simulate %>% paste(.,collapse = '-')})"))
  
  ### Create the cluster
  my_cluster = parallel::makeCluster(nb_cores, type = 'PSOCK')
  doParallel::registerDoParallel(my_cluster)
  
  ### Export the var list
  parallel::clusterExport(my_cluster,varlist = varlist ,envir=globalenv())
  
  ### Launch paralell loading
  dataset = foreach::foreach(year_simulated = as.character(years_to_simulate) ,
                             .packages = c("parallel","doParallel"),
                             combine = rbind) %dopar% 
    { Sys.sleep(runif(1,0,2)) ## Random time sleep to avoid simulateneous pings
      foreach::foreach(area = areas) %dopar% 
        { 
          Sys.sleep(runif(1,0,1)) ## Random time sleep to avoid simulateneous pings
          build_dataset(year_simulated = year_simulated,
                        power_plants = power_plants[[year_simulated]][[area]],
                        antares_output_name = outputs[[year_simulated]],
                        area = area, 
                        study_id = study_list[[year_simulated]]$variant_id)  
        }
    } %>% rename_dataset(., years_to_simulate = years_to_simulate, areas = areas)
  parallel::stopCluster(my_cluster)
  print(glue::glue("\n=> Sucessfull parallelized load of dataset\n"))
  return(dataset)
}



#' Build the study list for local and API
#'
#' @param params_study_list 
#' @importFrom antaresRead setSimulationPathAPI
#'
#' @return study_list (list) study ids list
build_study_list <- function(params_study_list){
  
  study_list = list()
  
  if(use_api){
    for (y in names(params_study_list)){
      opts = setSimulationPathAPI(host = host,
                                  study_id = params_study_list[[y]],
                                  token = token, 
                                  simulation = 'input', 
                                  timeout = 400)
      print(glue('\n=> Creating variant of Antares Web simulation {y}'))
      variant_opts = createVariant(name, opts = opts)
      study_list[[y]]$reference_id = opts$study_id
      study_list[[y]]$variant_id = variant_opts$study_id
    }
  }
  else{
    for (y in names(params_study_list)){
      study_list[[y]]$reference_id = y
      study_list[[y]]$variant_id = y
    }
  }
  return(study_list)
}
