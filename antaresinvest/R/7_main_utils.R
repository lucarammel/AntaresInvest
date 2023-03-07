#' Wrapper of the run antares simulation : local and antares_web
#'
#' @param years_to_simulate (int vector) years to simulate
#' @param year_actual (int) year actual of the model
#' @param s (int) loop number
#' 
#' @return outputs of the years simulated by the antares runs
#' 
#' @export
run_antares <- function(years_to_simulate,
                        year_actual,
                        s){
  
  id_run = as.character(glue('{s}-{year_actual}')) #Save number of the powerfleet corresponding to the loop
  if(!use_api){
    # Running the 3 simulation antares at once.
    outputs = parallelisation_antares(nb_cores = nb_cores,
                                      solver_path = solver_path,
                                      years_to_simulate = years_to_simulate,
                                      antares_simu_dir = antares_simu_dir,
                                      sub_mode = sub_mode,
                                      mc_list = mc_list,
                                      run_name = id_run, 
                                      antares_mode = antares_mode,
                                      antares_run_save = antares_run_save) %>% setNames(., years_to_simulate)
  } else {      
    outputs = parallel_run_antares_web(years_to_simulate = years_to_simulate,
                                       sub_mode = sub_mode,
                                       mc_list = mc_list,
                                       run_name = id_run) %>% setNames(.,years_to_simulate)
    Sys.sleep(20) ## Little time sleep before reading data with antaresRead
  }
  
  return(outputs)
}


#' Wrapper of the restore antares function either or not for antares Web 
#'
#' @return None
#' 
#' @export
restore_antares <- function(study_list){
  if(!use_api){
    restore_antares_simulation(save_id = '0_0',
                               saving_mode = FALSE,
                               end_etude = TRUE)
    
    tmp = lapply(names(study_list), function(year_simulated) edit_price_cap(study_id = study_list[[year_simulated]]$variant_id,
                                                                             restore_mode = TRUE))                         
  } else {
    restore_antares_web_simulation()
  }
}

#' Setup of directories and variables relative to the good progress of the model
#'
#' @importFrom glue glue
#' @return list of essential variables
#' @export
setup_antares_invest <- function(){
  
  ### Checks
  check = lapply(areas, function (area_to_check) {if (area_to_check %in% list.files(hypothesis_dir) ) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop(' Missing area in hypothesis folder. Please fill it to match with areas wanted parameters.')
  
  check = lapply(areas, function (area_to_check) {if (area_to_check %in% names(power_increment)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop(' Missing area in power_increment. Please fill settings/invest_params.yaml to match with areas wanted parameters.')
  
  if (CRM_forced){
    check  = lapply(areas, function(area_to_check) {if (area_to_check %in% names(reliability_standard)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
    if (check == 0) stop('Missing LOLE criteria in CRM params otherwise Basic CRM activated in global options. Please fill settings/capacity_market.yaml')  
  }
  
  check = lapply(areas, function(area_to_check) {if (area_to_check %in% names(service_system_total)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop('Missing service system total amount. Please fill settings/investment_params.yaml to match with areas wanted parameters')  
  
  check = lapply(areas, function(area_to_check) {if (area_to_check %in% names(color_palette_countries)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop('Missing colors for countries. Please fill settings/graphics_option.yaml to match with areas wanted parameters')  
  
  check = lapply(areas, function(area_to_check) {if (area_to_check %in% names(limit_investment)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop('Missing limit investment. Please fill settings/investment_params.yaml to match with areas wanted parameters')  
  
  check = lapply(areas, function(area_to_check) {if (area_to_check %in% names(limit_retired)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop('Missing limit retired. Please fill settings/investment_params.yaml to match with areas wanted parameters')  
  
  check = lapply(areas, function(area_to_check) {if (area_to_check %in% list.files(hypothesis_dir)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
  if (check == 0) stop('Missing costs hypothesis for a country. Please fill hypothesis/country/costs.yaml to match with areas wanted parameters') 
  
  if (use_api) {
    check = lapply(antares_to_simulate, function(y) {if (y %in% names(api_study_list[[antares_sub_dir]]) %>% as.character(.)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
    if (check == 0) stop('Missing Antares study in AntaresWeb for your choose of Antares sub-directory. Fill settings/antares_params.yaml or choose the right sub directory')  
  }

  if (reliability_options$enabled) {
    check = lapply(areas, function(area_to_check) {if (area_to_check %in% names(RO_strike_price_country)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
    if (check == 0) stop('Missing RO strike prices. Please fill settings/CRM_params.yaml to match with areas wanted parameters')  
    
    if (capacity_mechanism$enabled == FALSE) stop('Reliability options activated while no CRM option activated. Please activate capacity_mechanism$enabled.')
  }
  ### Creating directories for the study and the user
  build_folders_architecture()
  
  ### Start log file 
  sink(paste(study_dir, 'simulation_log.txt', sep = '/'), split = TRUE)

  message = glue('Launching AntaresInvest model for {user}, study name {study_name}')
  decorate_message(message)
  
  message = glue("AntaresInvest parameters:
                \n- Antares years to be simulated : {antares_to_simulate %>% paste(.,collapse = '-')}
                \n- Countries selected : {toupper(areas) %>% paste(.,collapse = '-')}
                \n- Iteration max: {iteration_max} 
                \n- AntaresWeb : {use_api}
                \n- Mode static : {static_mode$enabled}")
  decorate_message(message)

  start_time <<- Sys.time() #Time tracking for elapsed time
  
  ### Prepare data variables to store/record interesting outputs
  decisions <<- lapply(areas, function(area) {list(power_plants_moves = data.frame(id_move = numeric(),
                                                                                   name = character(),
                                                                                   year_decision = numeric(),
                                                                                   unitcount = numeric(),
                                                                                   nominalcapacity = numeric(),
                                                                                   construction_period = numeric(),
                                                                                   year_start = numeric(),
                                                                                   movement = character(),
                                                                                   loop = numeric(), 
                                                                                   done = logical()), 
                                                   continue = 1)}) %>% setNames(., areas)
  
  data_outputs <<- list()
  max_loop <<- 0 
  
  ### Build study list from parameters
  study_list <<- build_study_list(params_study_list = params_study_list)

  ### Save the initial powerfleet.
  if(!use_api){
    restore_antares_simulation(save_id = '0_0',
                               saving_mode = TRUE,
                               end_etude = FALSE)
  } 
  
  ### Build the dataframe containing initial powerplants fleet from antares study
  power_plants <<- lapply(antares_study_list, function(simulation_name) lapply(areas,function(area) make_powerplants_from_antares(study_id = study_list[[simulation_name]]$variant_id,
                                                                                                                                  zone = area, 
                                                                                                                                  year_simulated = simulation_name))) %>% rename_dataset(., years_to_simulate = antares_study_list, 
                                                                                                                                                                                          areas = areas)
  
}

#' Wrapper of multiple make_outputs functions
#'
#' @param data_outputs (list) Recovered outputs of the model by get_data_outputs()
#' @param decisions (list) investment decisions
#' @param max_loop (int) max loop reached
#'
#' @return None
#' @export
make_outputs_wrapper <- function(data_outputs,
                                 decisions, 
                                 max_loop){
  if (!parallel_loading){
    outputs_model = lapply(areas, function (area) make_outputs(data_outputs = data_outputs, 
                                                               area = area, 
                                                               decisions = decisions, 
                                                               max_loop = max_loop)) %>% setNames(., areas)
  } else {
    outputs_model = parallelisation_make_outputs(areas = areas) %>% setNames(., areas)
  }
}

#' Wrapper for get_data_outputs
#'
#' @param dataset (list) antaresRead data and other processed data after the antares run
#' @param dataset_revenues (list) computed revenues for each cluster for each country
#' @param power_plants (list) power_plants dataframe hypothesis list for each country
#' @param year_actual (int) actual year 
#' @param data_outputs (list) outputs recovered from the model
#' @param s (int) loop
#' @param investment_metrics (list) investments metrics dataframe for each country 
#' @param continue (int) continue variable 
#'
#' @return data_outputs (list) outputs recovered from the model updated
#' @export
get_data_outputs_wrapper <- function(dataset,
                                     dataset_revenues,
                                     power_plants,
                                     year_actual,
                                     data_outputs,
                                     s,
                                     investment_metrics,
                                     continue){
  if (!static_mode$enabled){
    data_outputs %<>% get_data_outputs_model(dataset = dataset,
                                             dataset_revenues = dataset_revenues,
                                             power_plants = power_plants, 
                                             year_actual = year_actual,
                                             data_outputs = .,
                                             s = s, 
                                             investment_metrics = investment_metrics,
                                             continue = continue) %>% setNames(., areas)
  } else {
    for (y in years_to_simulate) data_outputs %<>% get_data_outputs_model(dataset = dataset, dataset_revenues = dataset_revenues, power_plants = power_plants, year_actual = y,
                                                                          data_outputs = ., s = s, investment_metrics = investment_metrics, continue = continue) 
  }
  return(data_outputs)
}

#' Wrapper for buid_dataset function 
#'
#' @param years_to_simulate (int vector) 
#' @param power_plants (list) power_plants dataframe hypothesis for each country
#' @param study_list (list)
#' @param outputs (list) antares outputs
#'
#' @return dataset (list) antaresRead data and other processed data after the antares run
#' @export
build_dataset_wrapper <- function(power_plants,
                                  study_list,
                                  years_to_simulate, 
                                  outputs){
  if(!parallel_loading){
    dataset = lapply(as.character(years_to_simulate), function(year_simulated) lapply(areas, function(area) build_dataset(year_simulated = year_simulated,
                                                                                                                          power_plants = power_plants[[year_simulated]][[area]],  
                                                                                                                          antares_output_name = outputs[[year_simulated]],
                                                                                                                          area = area, 
                                                                                                                          study_id = study_list[[year_simulated]]$variant_id))) %>% rename_dataset(., years_to_simulate = years_to_simulate, 
                                                                                                                                                                                                      areas = areas)
  } else {
    dataset = parallelisation_build_dataset(years_to_simulate, areas)
  }
  return(dataset)
}

#' Make years wrapper 
#'
#' @param antares_actual (str) the year actual
#'
#' @return (list) with year actual and the years to be simulated
#' @export
make_years <- function(antares_actual){
  year_actual <<- as.integer(antares_actual)

  if (static_mode$enabled){
    years_to_simulate <<- c(year_actual, antares_study_list %>% as.integer() %>% .[. > year_actual])
  }
  else {
    if (!is.null(antares_futures_study)){
      if (year_actual %in% as.integer(antares_futures_study)){
        years_to_simulate <<- c(year_actual, antares_study_list %>% as.integer() %>% .[. > year_actual])
      }
      else{ 
        years_to_simulate <<- c(year_actual, antares_futures_study)
      }
    }
    else{
      years_to_simulate <<- c(year_actual, antares_study_list %>% as.integer() %>% .[. > year_actual])
    }
  }
  ### Initialize the counter of loops for the actual year
  s <<- 1 
  continue <<- 1 #If >= 1, continues, if = 0, stops. 
}

#' Remove outputs of antares runs when finished to recover spaces on disk
#'
#' @return None
#' @export
delete_outputs_antares <- function(force = FALSE){
  
  for (study in antares_study_list){
    path_outputs = paste(antares_simu_dir, study, 'output', sep = '/')
    
    for (output in list.files(path_outputs)){
      if (!grepl(antares_run_save$pattern, output) | force){
        path_output = paste(path_outputs, output, sep = '/') 
        print(glue('=> Removing outputs from {study} - {output}'))
        unlink(path_output, recursive = TRUE)
      }
    }
  }
}

#' Creating directories for the study and the user

#' @importFrom glue glue
#' @return None
#' @export
build_folders_architecture <- function(){
  
  dir.create(paste(project_dir, 'results', user, sep = '/'), recursive = TRUE)
  if (!use_api) dir.create(paste(antares_simu_dir, 'saved_clusters', sep = '/'), recursive = TRUE)
  dir.create(paste(study_dir, 'graphes', sep = '/'), recursive = TRUE)
}

#' Rename the second level of dataset with areas names
#'
#' @param dataset the 2 leveled dataset list to rename with first level : years, second level : countries
#' @param years_to_simulate (str vector) the years to simulate
#' @param areas (str vector)
#'
#' @return dataset updated with areas name
#' @export
rename_dataset <- function(dataset, 
                           years_to_simulate, 
                           areas){
  
  names(dataset) = years_to_simulate
  
  for (y in names(dataset)) names(dataset[[y]]) = areas
  
  return(dataset)
}

#' Print decorated message 
#'
#' @param message the message to decorate
#'
#' @export
decorate_message <- function(message){
  length_char = message %>% as.character(.) %>% strsplit('\n') %>% .[[1]] %>% nchar(.) %>% max(.)
  decorator = strrep( "*", length_char)
  print(glue::glue("\n\n{decorator}"))
  print(glue::glue('{message}\n'))
  print(glue::glue("{decorator}\n"))
}
