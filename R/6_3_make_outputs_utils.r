
#' Parallelized outputs creation (graphs & Excel sheets)
#'
#' @param areas Areas
#'
#' @importFrom magrittr %>%
#'
#' @return outputs of the model
#' @export
parallelisation_make_outputs <- function(areas){
  ### Export all the functions and all the variables required for the parallel loop
  varlist = c(ls(envir=.GlobalEnv) %>% .[! . %in% c("dataset","dataset_revenues")], ls("package:antaresinvest"))
  areas_verbose = sapply(areas,function(a) glue::glue("[{toupper(a)}]")) %>% as.vector(.) %>% paste(.,collapse = '-')
  print(glue::glue("\n\n Launching parallelized outputs creation for {areas_verbose}"))
  ### Create the cluster
  my_cluster = parallel::makeCluster(min(length(areas),nb_cores), type = 'PSOCK')
  doParallel::registerDoParallel(my_cluster)
  ### Export the var list
  parallel::clusterExport(my_cluster,varlist = varlist ,envir=globalenv())
  ### Launch paralell loading
  outputs = foreach::foreach(area = areas ,
                             combine = rbind) %dopar% {
                               make_outputs(data_outputs = data_outputs,
                                            area = area,
                                            decisions = decisions, 
                                            max_loop = max_loop)}
  parallel::stopCluster(my_cluster)
  print(glue::glue("\n=> Sucessfull parallelized outputs creation for data_outputs\n"))
  
  return(outputs)
}


#' Remove loops stored uselessly between max_loop and iteration_max
#'
#' @param max_loop number maximum of really run loops over years
#'
#' @importFrom magrittr %>%
#' @export
remove_useless_loops <- function(max_loop,
                                 data_outputs){
  if(!static_mode$enabled){
    for (area in data_outputs %>% names(.)){
      for (y in data_outputs[[area]] %>% names(.)){
        #Get loops copied and useless because no mouvement for every year actual on these ones. 
        loop_to_delete = ((max_loop + 1) : iteration_max) %>% as.character(.)
        if (max_loop == iteration_max) loop_to_delete = NULL
        for (s in loop_to_delete){
          data_outputs[[area]][[y]][[s]] = NULL  #Remove theses.
        }
      }
    }
    return(data_outputs)
  }
  else{return(data_outputs)}
}

#' Function to record during the model runs the investment metrics as excel files.
#' 
#' @import glue
#' @import xlsx
#' 
record_investment_metrics <- function(){
  #Store as excel file the investment metrics
  out_path = paste(study_dir, 'investment_metrics_record.xlsx', sep = '/') 
  
  if (file.exists(out_path)) {
    #An excel file already exists
    existing_movement_reports = out_path %>% excel_sheets() %>% set_names() %>%  map(read_excel, path = out_path)
    unlink(out_path) #delete old excel file
    
    for (zone in areas){
      #Add columns to identify the year_actual and loop 
      if (!is.null(investment_metrics[[zone]]$metrics) & !is.null(investment_metrics[[zone]]$revenues_total_net)){
        investment_metrics[[zone]]$metrics %<>% mutate (year_actual = year_actual, loop = s)
        investment_metrics[[zone]]$revenues_total_net %<>% mutate (year_actual = year_actual, loop = s, clusters = rownames(.)) %>% select('clusters', everything())
        
        existing_sheet_metrics = existing_movement_reports[[glue::glue('{toupper(zone)} - {metric}')]]
        existing_sheet_revenues = existing_movement_reports[[glue::glue('{toupper(zone)} - revenues_net_total')]]
        
        #Complete missing years to get adequacy for the concatenation of investment metrics table
        existing_years = existing_sheet_revenues %>% colnames(.) %>% .[grepl('20', .)] %>% as.integer(.)
        missing_years = investment_metrics[[zone]]$revenues_total_net %>% colnames(.) %>% .[grepl('20',.)] %>% as.integer(.)
        missing_years = existing_years[!existing_years %in% missing_years] %>% as.character(.)
        
        if (length(missing_years) > 0) {
          for (year in missing_years) investment_metrics[[zone]]$revenues_total_net[[year]] = 0 #complete missing year columns
          investment_metrics[[zone]]$revenues_total_net %<>% .[, colnames(existing_sheet_revenues)]
        }
        
        new_sheet_metrics = rbind(existing_sheet_metrics, investment_metrics[[zone]]$metrics) %>% as.data.frame(.)  
        new_sheet_revenues = rbind(existing_sheet_revenues, investment_metrics[[zone]]$revenues_total_net) %>% as.data.frame(.)   
        
        xlsx::write.xlsx(new_sheet_metrics,
                         sheetName = glue::glue('{toupper(zone)} - {metric}'), 
                         file = out_path, 
                         append = TRUE, 
                         row.names = FALSE)
        xlsx::write.xlsx(new_sheet_revenues, 
                         sheetName = glue::glue('{toupper(zone)} - revenues_net_total'), 
                         file = out_path , 
                         append = TRUE, 
                         row.names = FALSE)      
      }   
    }
  }
  else{
    #We create the first excel file
    #Add columns to identify the year_actual and loop
    for (zone in areas){
      investment_metrics[[zone]]$metrics %<>% mutate (year_actual = year_actual, loop = s)
      investment_metrics[[zone]]$revenues_total_net %<>% mutate (year_actual = year_actual, loop = s, clusters = rownames(.)) %>% select('clusters', everything())
      
      xlsx::write.xlsx(investment_metrics[[zone]]$metrics,
                       sheetName = glue::glue('{toupper(zone)} - {metric}'), 
                       file = out_path, 
                       append = TRUE, 
                       row.names = FALSE)
      xlsx::write.xlsx(investment_metrics[[zone]]$revenues_total_net, 
                       sheetName = glue::glue('{toupper(zone)} - revenues_net_total'), 
                       file = out_path, 
                       append = TRUE, 
                       row.names = FALSE)
    }
  }
}