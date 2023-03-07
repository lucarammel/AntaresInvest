### Clear workspace and everything before starting and load magrittr to build project_dir variable.  
rm(list = ls())
library(magrittr)

### Build the project directory variable path
project_dir = rstudioapi::getActiveDocumentContext()$path %>% strsplit(., split = "/", fixed = T)  %>% .[[1]] %>% .[-c(length(.),length(.)-1)] %>%
  paste(.,collapse="/")

### Loading AntaresInvest
antaresinvest_directory = paste(project_dir, 'antaresinvest', sep = '/')
devtools::load_all(antaresinvest_directory)

### Loading settings and config files
build_parameters_input()

### Set up essential variables for the model progress
setup_antares_invest()

for (antares_actual in antares_to_simulate){
  
  make_years(antares_actual)
  decorate_message(glue('Entering [{year_actual}]'))
  
  while (s <= iteration_max & continue >= 1){
    
    decorate_message(glue('Starting loop [{s}] for [{year_actual}]'))
    
    ### Add the changes obtained by the model
    power_plants = lapply(as.integer(antares_study_list), function(year_simulated) lapply(areas, function(area) update_powerplants(power_plants = power_plants[[as.character(year_simulated)]][[area]],
                                                                                                                                   area = area,
                                                                                                                                   year_simulated = year_simulated,
                                                                                                                                   year_actual = year_actual,
                                                                                                                                   study_id = study_list[[as.character(year_simulated)]]$variant_id,
                                                                                                                                   s = s))) %>% rename_dataset(.,years_to_simulate = antares_study_list, areas =  areas)
    
    ### Perform the antares runs and stores the outputs names
    outputs = run_antares(years_to_simulate = years_to_simulate, 
                          year_actual = year_actual,
                          s = s)
    
    ### Read and build dataset from antares simulation
    dataset = build_dataset_wrapper(power_plants = power_plants,
                                    study_list = study_list,
                                    years_to_simulate = years_to_simulate,
                                    outputs = outputs)
    
    ### Make price caps moving if option activated
    if(moving_price_cap$enabled) make_price_caps_moves(antares_output_name = outputs[[as.character(year_actual)]],
                                                       year_actual = as.character(year_actual))
    
    ### Analysis of capacity and energy only markets : loop on years and areas
    dataset_revenues = lapply(as.character(years_to_simulate), function(year_simulated) lapply(areas, function (area) analysis_revenues_total(year_simulated = year_simulated,
                                                                                                                                              area = area,
                                                                                                                                              power_plants =  power_plants[[year_simulated]][[area]],
                                                                                                                                              clusters_not_supported = dataset[[year_simulated]][[area]]$clusters_filters$not_supported,
                                                                                                                                              data_antares = dataset[[year_simulated]][[area]]$data_antares))) %>% rename_dataset(., years_to_simulate = years_to_simulate, 
                                                                                                                                                                                                                                  areas = areas)
    
    ### Investment model
    if (!static_mode$enabled){
      decorate_message(glue("Investment decision based on simulated revenues on years {years_to_simulate %>% paste(.,collapse = '-')}"))
      # Prepare data for investments decisions
      investment_metrics = lapply(areas, function(area) prepare_investment_metrics(dataset_revenues = dataset_revenues,
                                                                                   dataset = dataset,
                                                                                   year_actual = year_actual,
                                                                                   years_to_simulate = years_to_simulate,
                                                                                   power_plants = power_plants[[as.character(year_actual)]][[area]],
                                                                                   clusters_not_supported = dataset[[as.character(year_actual)]][[area]]$clusters_filters$not_supported,
                                                                                   area = area)) %>% setNames(., areas)
      
      # Investment decisions
      decisions = lapply(areas, function(area) investment_model_base(year_actual = year_actual,
                                                                     power_plants = power_plants[[as.character(year_actual)]][[area]],
                                                                     investment_metrics = investment_metrics, 
                                                                     area = area, 
                                                                     decisions = decisions,
                                                                     LOLD = dataset[[as.character(year_actual)]][[area]]$data_antares$areas_annual %>% filter(area == area) %>% select('LOLD') %>% as.numeric(.),
                                                                     s = s)) %>% setNames(., areas)
      
      # Make the product sum of continue variable by area in order to get the overall continue
      continue = sapply(areas, function(area) {decisions[[area]]$continue}) %>% as.vector(.) %>% sum(.) 
    } else {
      continue = 0
    }
    
    ### Keep tracking of outputs after each loop and for each year.
    data_outputs = get_data_outputs_wrapper(dataset = dataset,
                                            dataset_revenues = dataset_revenues,
                                            power_plants = power_plants,
                                            year_actual = year_actual,
                                            data_outputs = data_outputs,
                                            s = s,
                                            investment_metrics = investment_metrics,
                                            continue = continue)
    
    if (s > max_loop) max_loop = s
    decorate_message(glue('End of loop [{s}] for [{year_actual}]'))
    if (!static_mode$enabled) s = s + 1
  }
  decorate_message(glue('All done for [{year_actual}]'))
}

### Build the parameters yaml record file 
build_parameters_output(toc = Sys.time(),
                        start_time = start_time)

### Make graphs
make_outputs_wrapper(data_outputs = data_outputs,
                     max_loop = max_loop,
                     decisions = decisions)

### Restore antares studies power fleet
restore_antares(study_list = study_list)

### End log file
sink()

if (!static_mode$enabled & !use_api) delete_outputs_antares()

