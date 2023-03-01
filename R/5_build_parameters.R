
#' Build the parameters yaml file in order to record the parameters used
#' 
#' @param toc elapsed time of the study
#'
#' @return write the parameters yaml file in results/study_name folder
#' @import yaml
#' @export
build_parameters_output <- function(toc, start_time){
  
  df = list(username = user,
            study_name = study_name,
            areas = areas,
            years_simulated = antares_to_simulate,
            delay_investment_decision = delay_investment_decision,
            discount_rate = discount_rate,
            mc_list = mc_list,
            capacity_mechanism = capacity_mechanism$enabled,
            allow_invest = allow_invest,
            minimum_increment = minimum_increment,
            minimum_revenue_margin = minimum_revenue_margin,
            LOLE_margin = LOLE_margin,
            CRM_forced = CRM_forced,
            adaptative_power_increment = adaptative_power_increment,
            iteration_max = as.integer(iteration_max),
            elapsed_time_hour = difftime(toc, start_time, units = c('hours')) %>% as.numeric(.), 
            power_increment = power_increment,
            limit_retired = limit_retired, 
            limit_investment = limit_investment,
            service_system_share = service_system_share,
            service_system_total = service_system_total,
            metric = metric) 
  
  if (capacity_mechanism$enabled) df$CRM = CRM
  if (CRM_forced) df$reliability_standard = reliability_standard
  if (reliability_options$enabled){
    df$RO_strike_price = RO_strike_price
    df$reliability_options = reliability_options
  }
  if (moving_price_cap$enabled) df$price_cap = moving_price_cap$price_cap_energy
  
  file_name = paste(project_dir, 'results',user, study_name, 'params.yaml', sep = '/')
  yaml::write_yaml(x = df, file = file_name)
  
  save.image(paste(project_dir, 'results', user, study_name, '.RData', sep = '/'))
}

#' Read yaml config parameters file and set to global environnement
#'
#' @return None
#' @export
build_parameters_input <- function(update_yaml = FALSE){
  
  options(warn=-1) #Get rid of warnings
  build_user_parameter() 
  
  #Load all parameters from config files
  params = list.files(paste(project_dir,'settings' ,sep = '/'), full.names = TRUE)
  params_yaml = sapply(params, yaml::read_yaml)
  tmp = lapply(params_yaml, list2env, envir = .GlobalEnv)
  
  #Complete with other computed global variables
  if (!is.null(mc_list)) {
    mc_number <<- length(mc_list)} 
  else if (is.null(mc_list)) {
      mc_list <<- 1:mc_number
  }
  else if ((mc_list == 'all') %>% prod(.) == 1) {
    mc_list <<- 1:999
  }

  if (areas == 'all') areas <<- all_areas
  
  # Get all the antares studies
  antares_study_list <<- as.character(c(antares_to_simulate, antares_futures_study))
  antares_study_list <<- antares_study_list[!duplicated(antares_study_list)]
  time_horizon <<- as.numeric(antares_study_list[length(antares_study_list)])

  # Filter the antares_to_simulate, if we are in static mode
  if (static_mode$enabled) {
    iteration_max <<- 1
    antares_to_simulate <<- c(antares_to_simulate[1])
  }

  if (use_api){
    params_study_list <<- api_study_list[[antares_sub_dir]][antares_study_list]
  } else {
    local_study_list <<- lapply(1:length(antares_study_list), function(year) antares_study_list[year]) %>% setNames(., antares_study_list)
    params_study_list <<- local_study_list
  }

  data_dir <<- paste(project_dir, "data", sep="/")
  hypothesis_dir <<- paste(data_dir, 'hypothesis', sep="/")
  antares_simu_dir <<- paste(data_dir,'antares', antares_sub_dir, sep = '/')

  if (!update_yaml){
    study_name <<- paste(format(Sys.time(),format = '%Y%m%d-%H%M'), name, sep = '-')
    study_dir <<-  paste(project_dir, "results",user, study_name, sep="/")
  }
}

#' Build user_params file in yaml format
#'
#' @return None. File is written.
#' @export
build_user_parameter <- function(){
  
  path_user = paste(project_dir,'settings','user_params.yaml', sep = '/')
  
  if(file.exists(path_user)){
    return()
  }
  else{
    token = readline(prompt = 'Enter personal token from Antares Web - useful only for antaresWeb runs : ')
    user = readline(prompt = 'Enter username :')
    
    df = list(token = token, 
              user = user)
    
    yaml::write_yaml(x = df, file = path_user)
  }
}
