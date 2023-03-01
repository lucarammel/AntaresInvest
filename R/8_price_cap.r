#' Compute max hourly spot prices in Europe
#'
#' @param study_id (str) the name of the antares simulation
#' @param antares_output_name (str)  name of the output antares corresponding to a run. 
#'
#' @return Hourly max spot prices in Europe
#' 
#' @importFrom antaresRead readAntares setSimulationPath setSimulationPathAPI
#' @import glue 
#' 
#' @export
compute_max_hourly_spot <- function(antares_output_name, 
                                    study_id){
  
  print(glue::glue("\n=> Computing hourly max spot price over Europe"))
  
  if (use_api){
    opts = setSimulationPathAPI(host = host, token = token,
                                study_id = study_id, simulation = antares_output_name,
                                timeout = 400)
  } else {
    opts = setSimulationPath(paste(antares_simu_dir, study_id, sep="/"), antares_output_name)
  }
  
  variable_to_get = c('mcYear', 'timeId', 'MRG. PRICE')
  
  #Get all spot prices over every area
  spot_prices = lapply(all_areas, function(area) {readAntares(opts = opts, mcYears = mc_list, areas = area, timeStep= "hourly", select=variable_to_get, showProgress = FALSE) %>%
      select(-c("day", "month", "hour")) %>% 
      dcast(.,timeId~mcYear, value.var = "MRG. PRICE") %>%
      as.data.table(.) %>% .[, -c('timeId')] %>% as.data.frame(.)}) %>% setNames(all_areas) 
  
  max_spot = spot_prices[[1]] #initialisation 
  for (area in all_areas) max_spot = pmax(spot_prices[[area]], max_spot) #Get the maximum for each hour
  
  return(max_spot)
}

#' Edit price cap in the simulation
#'
#' @param price_cap (scalar) new price cap for energy only market
#' @param study_id (str) simulation name
#' @param restore_mode (bool) if yes or no, restore the initial price cap
#' 
#' @importFrom glue glue
#' @importFrom antaresEditObject writeIniFile
#' @importFrom antaresRead readIniFile
#'
#' @return None or the initial price cap if reading mode = TRUE
#' @export
edit_price_cap <- function(price_cap = NA,
                           study_id, 
                           restore_mode = FALSE,
                           reading_mode = FALSE){
  
  if (!use_api){
    path_ini = paste(antares_simu_dir, study_id, 'input', 'thermal', 'areas.ini', sep = '/')
    areas_ini = readIniFile(path_ini)
  }else{
    opts = setSimulationPathAPI(study_id = study_id, simulation = 'input',timeout=400,
                                host = host, token = token)
    areas_ini = readIniAPI(study_id = study_id, path = 'input/thermal/areas', token = token, host = host)
  }
  
  if(reading_mode) return(areas_ini$unserverdenergycost[[1]]) #return the price cap of the first areas equivalent to all of the areas
  
  if (!restore_mode){                        
    print(glue('\n=> Changing price cap of energy-only market for EU - [{study_id}] - set to {price_cap}'))
    for (area in names(areas_ini$unserverdenergycost)){
      areas_ini$unserverdenergycost[[area]] = price_cap
    }
    
    if (!use_api){
      writeIniFile(listData = areas_ini, pathIni = path_ini, overwrite = TRUE)
    }
    else{writeIni(opts = opts, listData = areas_ini, pathIni = 'input/thermal/areas')}
  }
  else{
    print(glue('\n=> Restoring price cap of energy-only market for EU - [{study_id}] - set to {moving_price_cap$price_cap_energy}'))
    for (area in names(areas_ini$unserverdenergycost)){
      areas_ini$unserverdenergycost[[area]] = moving_price_cap$price_cap_energy
    }
    
    if (!use_api){
      writeIniFile(listData = areas_ini, pathIni = path_ini, overwrite = TRUE)
    }
    else{writeIni(opts = opts, listData = areas_ini, pathIni = 'input/thermal/areas')}
  }
}


#' Compute price cap following ACER decision of 14 November 2017
#'
#' @param antares_output_name (str) output of the antares run
#' @param initial_price_cap (scalar) initial price cap
#' @param year_actual (str) name of the antares simulation
#'
#' @return price cap evolution
#' 
#' @export
make_price_caps_moves <- function(antares_output_name,
                                  year_actual) {
  
  print(glue('\n => Moving price cap option activated. '))
  initial_price_cap = edit_price_cap(study_id = study_list[[year_actual]]$variant_id,
                                     reading_mode = TRUE)                                  
  
  max_spot = compute_max_hourly_spot(antares_output_name = antares_output_name,
                                     study_id = study_list[[year_actual]]$variant_id)
  
  cap_threshold = initial_price_cap * moving_price_cap$cap_threshold_share
  max_spot$hour = 1:nrow(max_spot)
  max_spot$day = lapply(1:364, function(x) {rep(x, 24)}) %>% unlist(.)
  
  # Computing number of times of price cap reached 
  times_cap_reached = list()
  
  for (y in mc_list %>% as.character(.)){
    
    df = max_spot[,c(y, 'hour', 'day')] %>% filter(if_any(!starts_with('hour') & !starts_with('day'),function(x) {x > cap_threshold}))
    if (nrow(df) > 1){
      df = df[,c('hour', 'day')]
      df$diff_hour = c(diff(df$hour),0)
      df$diff_day = c(diff(df$day),0)

      df$is_consecutive = c(sapply(1:(nrow(df)-1), function(x) {df[x, 'diff_day'] == df[x+1, 'diff_day'] & df[x, 'diff_hour'] == df[x+1, 'diff_hour'] }), FALSE)
      
      df$nbr_trigger = sapply(1:nrow(df), function(x){if(x == 1){df[x,'is_consecutive']}
        else{df[x, 'diff_day'] >= 1 | df[x, 'diff_hour'] > 1}})
      
      times_cap_reached[[y]] = df$nbr_trigger %>% sum(.)
    }
    else{times_cap_reached[[y]] = nrow(df)}
  }
  
  times_cap_reached = times_cap_reached %>% unlist(.) %>% mean(.) %>% round(.)
  
  if (times_cap_reached > moving_price_cap$duration_threshold){
    new_price_cap = times_cap_reached * moving_price_cap$cap_raising + initial_price_cap
    print(glue('\n ==> {times_cap_reached} times price cap reached - new price cap {new_price_cap}.'))
    #Finally edit the price cap into the simulation itself
    following_years = antares_study_list %>% as.integer(.) %>% .[. > as.integer(year_actual)] %>% as.character(.)
    tmp = lapply(following_years, function(year_simulated) edit_price_cap(price_cap = new_price_cap,
                                                                          study_id = study_list[[year_simulated]]$variant_id))
  }
  else{
    print(glue('\n ==> Energy-only price cap reached only {times_cap_reached} times. Not enough to raise price cap.'))
  }
}
