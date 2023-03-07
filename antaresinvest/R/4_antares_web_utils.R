#' Run a simulation on antares web, specifiying the general options
#'
#' @param year year of simulation
#' @param simulation_name name of simulation
#' @param antares_mode mode of simulation (economy)
#' @param sub_mode sub_mode (fast)
#' @param mc_list mc_list to simulate
#' @param verbose verbose 
#'
#' @import glue
#' @import antaresRead
#' @importFrom antaresEditObject createVariant updateGeneralSettings setPlaylist updateOptimizationSettings editArea runSimulation
#' @import tictoc
#' @importFrom dplyr %>%
#'
#' @return job_id : id of the job launched on antares web
#' @export
run_simulation_antares_web <- function(year,
                                       simulation_name,
                                       antares_mode,
                                       sub_mode,
                                       mc_list,
                                       verbose = TRUE){
  if (verbose) {
    print(glue::glue("\nSetting up antaresWeb simulation for {year}, running simulation ... .\n"))
  }
  ### We select the study_if of the study we need to run
  study_id  = study_list[[as.character(year)]]$variant_id
  
  antaresRead::setSimulationPathAPI(host = host,
                                    token = token,
                                    study_id = study_id,
                                    simulation = "input",
                                    timeout = 400)
  updateGeneralSettings(nbyears = 1000) #To avoid mistakes on the following lines
  setPlaylist(mc_list) #Run only on the representatives playlist of montecarlo years
  
  updateOptimizationSettings(unit.commitment.mode = sub_mode)
  
  output_name = paste(format(Sys.time(),format = '%Y%m%d-%H%M'),substring(antares_mode,1,3),sep = '')
  output_name = paste(output_name, simulation_name, sep = '-')
  job_id = runSimulation(name = glue::glue('{simulation_name}'), mode = antares_mode, wait = FALSE, nb_cpu = nb_cpu) # 24 cores out of 24.
  Sys.sleep(2) ## Little time sleep after we launched a simulation
  return (job_id)
}

# }

#' Launch in parralel several simulation on antares Web
#'
#' @param years_to_simulate years 202K to simulate
#' @param sub_mode sub_mode ("fast")
#' @param mc_list monte carlo years list
#' @param run_name name of study
#' @importFrom dplyr %>%
#' @importFrom antaresEditObject getJobs
#' @import glue
#' @return outputs
#' @export
parallel_run_antares_web <- function(years_to_simulate,
                                     sub_mode,
                                     mc_list,
                                     run_name){
  jobs = lapply(years_to_simulate,
                function(year){ run_simulation_antares_web(year = year,
                                                           simulation_name = run_name ,
                                                           antares_mode = antares_mode,
                                                           sub_mode = sub_mode,
                                                           mc_list = mc_list,
                                                           verbose = TRUE)}) %>% setNames(.,years_to_simulate)
  finished = F
  ## This loop enable to track the launched jobs.
  ### We stop while when the number of finished jobs is equal to the number of jobs we have launched
  while(!finished){
    count_finished=0
    Sys.sleep(10) # Refresh every 10 seconds
    
    for (year in years_to_simulate){
      
      year = as.character(year)
      job_id = jobs[[year]]$job_id
      job = getJobs(job_id)
      Sys.sleep(1) ## A little time sleep to avoid a bug
      jobs[[year]]$status = job$status
      jobs[[year]]$output_id = job$output_id
      
      if (job$status == 'success'){ count_finished  = count_finished +1 }
      
      if (job$status == "failed"){
        finished = F
        stop(glue::glue("\nSimulation [{year}] - FAILED"))
      }
      finished = (count_finished==length(jobs)) } 
  }
  if (finished){
    print(glue::glue("\n\n Successful run of loop {s} for  years [{years_to_simulate %>% paste(.,collapse = '-')}]"))
    outputs = lapply(years_to_simulate,function(year){return(jobs[[as.character(year)]]$output_id)}) 
  } else {
    stop(glue::glue("\nError while running loop {s} for  years [{years_to_simulate %>% paste(.,collapse = '-')}]"))
    ouputs = NA
  }
  return(outputs)
}

#' Get all the variants with their level (essential to delete study), recursively
#'
#' @param l list
#' @param v_list variant list 
#' @param level level of the variant (level=0 is the reference study)
#'
#' @return None
#' @export
get_all_variants_of_study = function( l,v_list=list(), level=0){
  key = glue::glue("level_{level}")
  v_list[[key]] = c(v_list[[key]],l$node$id)
  level = level+1
  for ( m in l$children ){
    if (m$children %>% length(.) > 0){
      v_list = get_all_variants_of_study(m,v_list,level=level)
    } else {
      key = glue::glue("level_{level}")
      v_list[[key]] = c(v_list[[key]], m$node$id)
    }
  }
  return(v_list)
}

#' Detele all variants of a given study
#'
#' @param study_id study_id of the original study
#' @param year year of the simulation (for verbose)
#'
#' @return None
#' @export
delete_all_variants_of_a_study = function(study_id,year){
  ### kill all the variants of a given study
  url = glue::glue("{host}/v1/studies/{study_id}/variants")
  p = httr::GET( url, httr::add_headers(Authorization = paste("Bearer", token, sep = " ")) ) %>% 
    httr::content(.) %>% get_all_variants_of_study(.) 
  ### Build the study_list
  p$level_0 = NULL ### We will not delete the reference study
  tmp = lapply(names(p) %>% rev(.),function(level){ lapply( p[[level]], function(id) delete_study_antares_web(study_id = id,variant_name = id,year=year) ) })
}

#' Remove all the variants of the studies stored in study_list
#'
#' @return None
#' @export
restore_antares_web_simulation <- function(){
  cat("\nDeleting all variants used for the study:\n\n")
  tmp = lapply( names(study_list), function(year) delete_all_variants_of_a_study(study_list[[year]]$reference_id, year) )
}


#' Delete an antares web study thanks to antares_web API
#'
#' @param study_id study_id of the simulation we are launching
#' @param variant_name name of the variant we will delete
#' @param year year of simulation
#'
#' @import glue
#' @import httr
#' @return None
#' @export
delete_study_antares_web <- function(study_id,
                                     variant_name,
                                     year){
  if (study_id %in% dont_delete_antares_web){
    return()
  }
  if( !is.null(study_id) ){
    url = glue::glue("{host}/v1/studies/{study_id}")
    del = httr::DELETE(url, httr::add_headers(Authorization = paste("Bearer", token, sep = " ")))
    if(del$status_code==200){
      print(glue::glue("\n=> Successfully deleted variant [{variant_name}] from [{year}]"))
    } else {
      print(glue::glue("\n!! FAILED !! at deleted study [{study_id}] of {year})"))
    }
  }
}
