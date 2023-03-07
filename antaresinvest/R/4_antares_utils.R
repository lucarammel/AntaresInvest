#' Run an antares simulation and modify automatically settings
#'
#' @param simulation_path path of the simulation run
#' @param simulation_name name of the simulation output antares
#' @param solver_path antares solver path
#' @param sub_mode antares optimisation mode fast or accurate
#' @param antares_mode default : economy
#' @param mc_list montecarlo playlist
#' @param antares_run_save option to make a run antares not allowed for deleting automatically
#'
#' @import glue
#' @import antaresRead
#' @importFrom antaresEditObject updateGeneralSettings setPlaylist updateOptimizationSettings editArea runSimulation setSolverPath
#'
#' @return the output name of the antares run
#' @export
run_simulation_antares <- function(solver_path,
                                   simulation_path,
                                   simulation_name,
                                   antares_mode,
                                   sub_mode,
                                   mc_list, 
                                   antares_run_save) {
  
  print(glue::glue("Setting up the simulation here : {simulation_path},\n in mode {sub_mode}, running simulation ... .\n"))
  
  antaresRead::setSimulationPath(simulation_path, 'input')
  updateGeneralSettings(nbyears = 1000) #To avoid mistakes on the following lines
  setPlaylist(mc_list) #Run only on the representatives playlist of montecarlo years
  
  setSolverPath(solver_path)
  updateOptimizationSettings(unit.commitment.mode = sub_mode)
  
  output_name = paste(format(Sys.time(),format = '%Y%m%d-%H%M'),substring(antares_mode,1,3),sep = '')
  
  if (!antares_run_save$enabled){
    output_name = paste(output_name, simulation_name, sep = '-')
    runSimulation(name = glue('{simulation_name}'), mode = antares_mode)
  } else{
    output_name = paste(output_name, simulation_name, antares_run_save$pattern ,sep = '-')
    runSimulation(name = glue('{simulation_name}-{antares_run_save$pattern}'), mode = antares_mode)
  }
  
  print(glue('Name of the output created : {output_name} \n'))
  
  return (output_name)
}


#' Parallelisation of antares run
#'
#' @param years_to_simulate years to simulate simultaneously
#' @param nb_cores cores to use for the calculation
#' @param solver_path path of antares solver
#' @param antares_simu_dir antares simulation directory
#' @param sub_mode fast or accuratev lancer 'apAPImatin matib
#' @param mc_list montecarlo list
#' @param run_name name of the run_name without the default YYMMDD-HHmm
#' @param years_to_simulate years to simulate
#' @param antares_run_save option to make a run antares not allowed for deleting automatically
#'
#' @return output names of the runs.
#' @import glue
#' @import antaresEditObject
#' @import antaresRead
#' @import tictoc
#' @import doParallel
#' @import parallel
#' @import foreach
#' @importFrom foreach %dopar%
#' @return None
#' @export
parallelisation_antares <- function(nb_cores,
                                    solver_path,
                                    years_to_simulate,
                                    antares_simu_dir,
                                    sub_mode,
                                    mc_list,
                                    run_name, 
                                    antares_mode,
                                    antares_run_save){
  
  #Preparation to parallelisation
  print(glue::glue("\n\n ==> Simulating year [{years_to_simulate %>% paste(.,collapse = '-')}] with Antares Simulator.."))
  my_cluster = parallel::makeCluster(nb_cores ,type = 'PSOCK')
  doParallel::registerDoParallel(my_cluster)
  
  path_to_simulate = unlist(lapply(x = years_to_simulate,
                                   FUN = paste0,
                                   paste0(antares_simu_dir,'/')))
  
  #Parallelsisation itself
  outputs = foreach::foreach(path = path_to_simulate,
                             .packages = c('antaresEditObject',
                                           'antaresRead',
                                           'glue'),
                             combine = rbind,
                             .export = c('run_simulation_antares')) %dopar% run_simulation_antares(solver_path = solver_path,
                                                                                                   simulation_path = path,
                                                                                                   simulation_name = run_name,
                                                                                                   antares_mode = antares_mode,
                                                                                                   sub_mode = sub_mode,
                                                                                                   mc_list = mc_list, 
                                                                                                   antares_run_save = antares_run_save)
  return(outputs)
}

#' Extract time series of a cluster
#'
#' @param capacity_name (str) name of the new capacity. If already exists, build a new one with an specific id as suffix.
#' @param study_id (str) id of study
#' @param coefficient (float) coefficient to extract and scale the timeseries 
#'
#' @importFrom magrittr %<>% %>% multiply_by 
#' @importFrom dplyr mutate_all select
#' @importFrom antaresRead readInputThermal readInputRES
#'
#' @return timeseries (list) of every timeseries of the cluster
#' @export 
extract_ts_clusters <- function(capacity_name,
                                study_id,
                                coefficient){
  
  print(glue('==> Extracting [{capacity_name}].'))
  ### Extract time series to create the cluster
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  cluster_type = get_cluster_type(capacity_name)
  
  if (!cluster_type %in% renewables_type){
    # Loading input timeseries of the capacity 
    ts_cluster = readInputThermal(opts = opts, clusters = capacity_name, 
                                  thermalModulation = TRUE, 
                                  thermalData = TRUE)
    
    # Thermal availabilities (series.txt)
    ts_cluster$thermalAvailabilities %<>% select(., names(.)[grepl('ts', names(.))]) %>%
      multiply_by(., coefficient) %>% mutate_all(., as.integer) %>% as.matrix(.) %>% unname(.) 
    
    variable_to_remove = c('area', 'cluster', 'timeId', 'time', 'day', 'month', 'hour')
    
    # Modulation (modulation.txt)
    ts_cluster$thermalModulation %<>% select(-variable_to_remove) %>% replace(is.na(.), 0) %>% 
      as.matrix(.) %>% unname(.) 
    
    # Data (data.txt)
    ts_cluster$thermalData %<>% select(-variable_to_remove) %>% replace(is.na(.), 0) %>% 
      as.matrix(.) %>% unname(.) 
  }
  else {
    ts_cluster = readInputRES(opts = opts, clusters = capacity_name)
    ts_cluster %<>% select(., names(.)[grepl('ts', names(.))]) %>% multiply_by(., coefficient) %>%
      as.matrix(.) %>% unname(.) 
  }
  return(ts_cluster)
}


#' Edit time series for thermal cluster
#'
#' @param study_id study_id of study you want to edit
#' @param area area containing link or cluster
#' @param cluster_name name of cluster (or link)
#' @param operation which operation to apply to time series => '=' ; '*' ; '+'
#' @param value value to use with operation; e.g. if operation "=" and value "10" will set all time series value equal to 10
#'
#' @importFrom antaresRead readInputThermal readInputRES
#' @importFrom antaresEditObject editCluster editClusterRES
#' @importFrom magrittr %<>% %>% multiply_by 
#' @importFrom dplyr mutate_all select
#' 
#' @return None
#' @export
edit_ts_input_cluster <- function(study_id,
                                  area,
                                  capacity_name,
                                  value){
  
  opts = set_simulation_antares(study_id = study_id, input_mode = TRUE)
  
  cluster_type = get_cluster_type(capacity_name)
  
  if (!cluster_type %in% renewables_type){
    time_series = readInputThermal(opts = opts, clusters = capacity_name)  %>% 
      select(., names(.)[grepl('ts', names(.))]) %>% multiply_by(., value) %>% mutate_all(., as.integer)
    
    editCluster(area = area, 
                cluster_name = capacity_name,
                time_series = time_series,
                add_prefix = FALSE)
  }
  else{
    time_series = readInputRES(opts = opts, clusters = capacity_name)  %>% 
      select(., names(.)[grepl('ts', names(.))]) %>% multiply_by(., value) 
    
    editClusterRES(area = area, 
                   cluster_name = capacity_name,
                   time_series = time_series,
                   add_prefix = FALSE)
  }
  print(glue('\n ==> Successfully edited {cluster_name} timeseries - Multiplied by {round(value,2)}'))
}

#' Generate the new capacity name when adding a cluster with the same property than an existing one
#'
#' @param capacity_name (str) initial capacity name
#' @param power_plants (dataframe) with the power fleetfrom make_powerplants_from_antares()
#'
#' @importFrom magrittr %<>% %>%
#' 
#' @return new_capacity_name
#' @export
generate_new_capacity_name <- function(capacity_name,
                                       power_plants){
  if (capacity_name %>% substring(., nchar(.)) %>% as.integer(.) %>% is.na(.)) {
    #If no id at the end of the cluster name
    id = 1
    new_capacity_name = paste(capacity_name, id, sep = ' ')
    
  } else {
    #If an id exists, increase by 1 the id.
    id = capacity_name %>% substring(.,nchar(.)) %>% as.integer(.)
    id = id + 1
    
    new_capacity_name = capacity_name %>% strsplit(., split = '') %>% .[[1]] %>% .[-c(length(.))] %>% paste(., collapse = '') %>% paste(.,id, sep = '')
    while(grepl(new_capacity_name, power_plants$cluster) %>% sum(.) == 1){
      #Other check, to be sure that new capacity name already exists
      id_existing = new_capacity_name %>% substring(.,nchar(.)) %>% as.integer(.)
      id_existing = id_existing + 1
      new_capacity_name %<>% strsplit(., split = '') %>% .[[1]] %>% .[-c(length(.))] %>% paste(., collapse = '') %>% paste(.,id_existing, sep = '')
    }
  }
  return(new_capacity_name)
}


#' Save and restore the antares power fleet.
#'
#' @param save_id number of the save
#' @param saving_mode mode to save or restore
#' @param end_etude if true, every saves are erased and the power fleet are all restored.
#' @importFrom glue glue
#'
#' @return None
#' @export
restore_antares_simulation <- function(save_id = NA,
                                       saving_mode = FALSE,
                                       end_etude = FALSE){
  saved_clusters_path_initial = paste(antares_simu_dir, 'saved_clusters', 'save_0_0', sep = '/')
  if (saving_mode){
    if (save_id == '0_0' & file.exists(saved_clusters_path_initial)){
      #Check if an initial save exist and if all areas are completed
      check = lapply(all_areas, function (area_to_check) {if (area_to_check %in% list.files(saved_clusters_path_initial)) TRUE else FALSE}) %>% unlist(.) %>% prod(.)
      if (check == 1){ 
        print(glue('\n => Initial save already exists \n')) ## Don"t create a new initial save if already exists
        return()
      }
      else{
        #Complete the initial save with missing areas
        missing_areas = all_areas[!all_areas %in% list.files(saved_clusters_path_initial)]
        print(glue(" => Adding {missing_areas %>% toupper(.) %>% paste(.,collapse = '-')} to save 0"))
        for (area in missing_areas){
          restore_antares_simulation_save_mode(area = area, 
                                               save_id = save_id)
        } 
      }
    }
    else {
      print(glue('=> Saving powerfleet to saved_clusters/save_{save_id}..'))
      for (area in all_areas) restore_antares_simulation_save_mode(area = area, 
                                                                   save_id = save_id)
    }
  }
  if (!saving_mode){
    print(glue('\n => Restoring the save number {save_id}..'))
    for (area in all_areas){
      path_for_saved_ini = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, sep = '/')
      path_for_saved_series = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, 'series', sep = '/')
      path_for_saved_series_enr = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, 'series_renewables', sep = '/')
      path_for_saved_prepro = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, 'prepro', sep = '/')
      
      for (study in list.files(path_for_saved_prepro)){ 
        ### Copying the saved clusters ini files into the simulation back
        print(glue('Restoring powerfleet for {study}, {area} ..'))

        path_to_copy_series = paste(antares_simu_dir, study, 'input', 'thermal', 'series', area, sep = '/')
        path_to_copy_series_enr = paste(antares_simu_dir, study, 'input', 'renewables', 'series', area, sep = '/')
        path_to_copy_ini = paste(antares_simu_dir, study, 'input', 'thermal', 'clusters',area, sep = '/')
        path_to_copy_ini_enr = paste(antares_simu_dir, study, 'input', 'renewables', 'clusters',area, sep = '/')
        path_to_copy_prepro = paste(antares_simu_dir, study, 'input', 'thermal', 'prepro', area, sep = '/')

        ###Revert ini files for thermal & renewables
        file.copy(from = paste0(path_for_saved_ini, '/list_thermal_', study, '.ini'),
                  to = path_to_copy_ini)
        file.remove(paste(path_to_copy_ini, 'list.ini', sep = '/'))
        file.rename(from = paste0(path_to_copy_ini, '/list_thermal_', study, '.ini'),
                    to = paste(path_to_copy_ini, 'list.ini', sep = '/'))
        
        file.copy(from = paste0(path_for_saved_ini, '/list_enr_', study, '.ini'),
                  to = path_to_copy_ini_enr)
        file.remove(paste(path_to_copy_ini_enr, 'list.ini', sep = '/'))
        file.rename(from = paste0(path_to_copy_ini_enr, '/list_enr_', study, '.ini'),
                    to = paste(path_to_copy_ini_enr, 'list.ini', sep = '/'))

        ###Revert series and prepro files thermal & renewables
        for (file in list.files(path_to_copy_prepro)){
          ###Remove existing files
          unlink(paste(path_to_copy_prepro, file, sep = '/'), recursive = TRUE)
          unlink(paste(path_to_copy_series, file, sep = '/'), recursive = TRUE)
        }
        for (file in list.files(path_to_copy_series_enr)){
          ###Remove existing files
          unlink(paste(path_to_copy_series_enr, file, sep = '/'), recursive = TRUE)
        }

        for (file in list.files(paste(path_for_saved_series, study, sep = '/'))) {
          ###Replacing with saved files
          file.copy(from = paste(path_for_saved_series, study, file, sep = '/'),
                    to = path_to_copy_series, recursive = TRUE)
          file.copy(from = paste(path_for_saved_prepro, study, file, sep = '/'),
                    to = path_to_copy_prepro, recursive = TRUE)
        }
        for (file in list.files(paste(path_for_saved_series_enr, study, sep = '/'))){
          ###Replacing with saved files
          file.copy(from = paste(path_for_saved_series_enr, study, file, sep = '/'),
                    to = path_to_copy_series_enr, recursive = TRUE)
        }
      }
    }
  }
  if (end_etude) {
    ###Delete al saves except the 0_0 
    print(glue('=> Removing all saves .. End of the study.'))
    for (file in list.files(paste(antares_simu_dir, 'saved_clusters', sep = '/'))){
      if (!grepl('0_0', file)) unlink(paste(antares_simu_dir, 'saved_clusters', file , sep = '/'), recursive = TRUE)
    }
  }
}

#' Save the antares simulation files used by the model
#'
#' @param area country code 
#' @param save_id id of the save
#'
#' @return None
restore_antares_simulation_save_mode <- function(area, 
                                                 save_id){
  
  path_for_saved_ini = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, sep = '/')
  path_for_saved_series_enr = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, 'series_renewables', sep = '/')
  path_for_saved_series = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, 'series', sep = '/')
  path_for_saved_prepro = paste(antares_simu_dir, 'saved_clusters', glue('save_{save_id}'), area, 'prepro', sep = '/')
  
  ###First folder architecture building
  dir.create(path_for_saved_ini, recursive  = TRUE)
  dir.create(path_for_saved_prepro, recursive  = TRUE)
  dir.create(path_for_saved_series, recursive  = TRUE)
  
  for (study in list.files(antares_simu_dir) %>% .[grepl('20',.)] ){
    ###Folder building
    print(glue('Saving powerfleet for {study}, {area} ..'))

    series_simu_path = paste(path_for_saved_series, study, sep = '/')
    series_enr_simu_path = paste(path_for_saved_series_enr, study, sep = '/')
    prepro_simu_path = paste(path_for_saved_prepro, study, sep = '/')

    dir.create(series_simu_path, recursive = TRUE)
    dir.create(prepro_simu_path, recursive = TRUE)
    dir.create(series_enr_simu_path, recursive = TRUE)

    ###Ini saves
    ini_path = paste(antares_simu_dir, study, 'input', 'thermal', 'clusters',area, 'list.ini', sep = '/')
    ini_path_enr = paste(antares_simu_dir, study, 'input', 'renewables', 'clusters',area, 'list.ini', sep = '/')

    file.copy(from = ini_path,
              to = path_for_saved_ini)
    file.rename(from = paste(path_for_saved_ini, 'list.ini', sep = '/'),
                to = paste0(path_for_saved_ini, '/list_thermal_', study, '.ini'))
    
    file.copy(from = ini_path_enr,
              to = path_for_saved_ini)
    file.rename(from = paste(path_for_saved_ini, 'list.ini', sep = '/'),
                to = paste0(path_for_saved_ini, '/list_enr_', study, '.ini'))
    
    ### Series and prepro saves
    series_path_initial = paste(antares_simu_dir, study, 'input', 'thermal', 'series', area, sep = '/')
    series_enr_path_initial = paste(antares_simu_dir, study, 'input', 'renewables', 'series', area, sep = '/')
    prepro_path_initial = paste(antares_simu_dir, study, 'input', 'thermal', 'prepro', area, sep = '/')
    
    for (file in list.files(series_path_initial)) {
      file.copy(from = paste(series_path_initial, file, sep = '/'),
                to = series_simu_path, recursive = TRUE)
      file.copy(from = paste(prepro_path_initial, file, sep = '/'),
                to = prepro_simu_path, recursive = TRUE)
    }
    for  (file in list.files(series_enr_path_initial)) {
      file.copy(from = paste(series_enr_path_initial, file, sep = '/'),
                to = series_enr_simu_path, recursive = TRUE)
    }
  }
}

#' Make the first check, to assert simulation dataset is suitable for AntaresInvest
#'
#' @param study_id (str) antares simulation id
#' @param power_plants (dataframe) powerplants dataframe with all the hypothesis
#' @param power_plants (dataframe) powerplants actual dataframe with all the hypothesis
#' @param area (str) country code
#'
#' @importFrom antaresEditObject editArea editCluster editClusterRES
#' @importFrom antaresRead setSimulationPath
#' @importFrom glue glue
#' @return None
#' @export 
check_antares_dataset <- function(study_id,
                                  power_plants, 
                                  power_plants_actual,
                                  area, 
                                  lowering = TRUE,
                                  editing_outputs = TRUE,
                                  filling_info_cluster = TRUE){
  
  # Set up the simulation                                  
  simulation_path = paste(antares_simu_dir, study_id, sep = '/')
  setSimulationPath(simulation_path, 'input')
  
  if (lowering){
    print(glue('=> Lowering the case for [{toupper(area)}] - {study_id}'))
    # Lower the case of your antares simulation
    power_plants$cluster = sapply(power_plants$cluster, tolower) %>% as.vector(.)
    
    for (n in power_plants$cluster){
      cluster_type = get_cluster_type(n)
      
      if (!cluster_type %in% renewables_type){
        editCluster(area = area, 
                    cluster_name = n,
                    add_prefix = FALSE)
      } else {
        editClusterRES(area = area, 
                       cluster_name = n,
                       unitcount = as.integer(power_plants[power_plants$cluster == n, 'unitcount'] * 100),
                       nominalcapacity = as.integer(power_plants[power_plants$cluster == n, 'nominalcapacity'] / 100),
                       add_prefix = FALSE)
      }
    }
  }
  
  if (editing_outputs){
    print(glue('=> Editing area & links filtering outputs for [{toupper(area)}]'))
    # Setting the filter in order to get all areas to edit for the computing of the revenues
    editArea(name = area, 
             filtering = filteringOptions(filter_year_by_year = c('hourly', 'annual'),
                                          filter_synthesis = c('hourly', 'annual')))
    editLink(from = area, to = 'z_batteries', filter_year_by_year = c('hourly', 'annual'))
  }
  
  if (filling_info_cluster){
    # Check if information and timeseries exists, to not have trouble when investing.
    clusters = power_plants %>% filter(unitcount == 0) %>% .$cluster
    first_simulation_path = paste(antares_simu_dir, antares_study_list[1], sep = '/')
    
    for (c in clusters){
      ts_path = paste(simulation_path, 'input', 'thermal', 'series', area, c, 'series.txt', sep = '/')
      ts_path_first = paste(first_simulation_path, 'input', 'thermal', 'series', area, c, 'series.txt', sep = '/')
      df = fread(ts_path)
      df_first_simulation = fread(ts_path_first)
      cluster_info = power_plants_actual[(power_plants_actual$cluster == c),]
      
      if (nrow(df) == 0 & nrow(df_first_simulation) != 0){
        print(glue('\n => [{c}] filling information for simulation {study_id}'))
        coefficient = 1 / cluster_info$unitcount 
        df_first_simulation %<>% multiply_by(.,coefficient) %>% .[, names(.) := lapply(.SD, as.integer)]
        fwrite(df_first_simulation, ts_path, col.names = FALSE, sep = '\t')
        
        editCluster(area = area, 
                    cluster_name = c,
                    nominalcapacity = cluster_info$nominalcapacity,
                    startup_cost = cluster_info$startup_cost,
                    min_stable_power = cluster_info$min_stable_power,
                    co2 = cluster_info$co2,
                    enabled = FALSE,
                    add_prefix = FALSE)  
      }
    }
  }
}

#' Set simulation Path wrapper for Antares 
#'
#' @param study_id (str) antares simulation id
#' @param input_mode (str) if TRUE, setsimulation in reading mode
#' @param antares_output_name (str) compulsory if output mode
#' 
#' @return opts (list) antares simulation options
#' @export
set_simulation_antares <- function(study_id, 
                                   input_mode, 
                                   antares_output_name = NA){
  if (input_mode){
    if (use_api){
      opts = setSimulationPathAPI(host = host,
                                  token = token,
                                  study_id = study_id,
                                  simulation = "input",
                                  timeout = 400)
    } else {
      simulation_path = paste(antares_simu_dir, study_id, sep = '/')
      opts = setSimulationPath(simulation_path, 'input')
    }
    opts$timeIdMax = 8760
  } else{
    if (use_api){
      opts = setSimulationPathAPI(host = host,
                                  token = token,
                                  study_id = study_id,
                                  simulation = antares_output_name,
                                  timeout = 400)
    } else {
      simulation_path = paste(antares_simu_dir, study_id, sep = '/')
      opts = setSimulationPath(simulation_path, antares_output_name)
    }
  }
  
  return(opts)
}
