#' Mother function of outputs (ie graphs, and outputs files)
#' 
#' @param data_outputs (list)outputs from the model
#' @param area (str) country code
#' @param decisions (list) investment decisions
#' @param max_loop (int) number maximum of really run loops over years
#'
#' @importFrom  magrittr %>%
#' @importFrom xlsx write.xlsx
#' @return Make automated graphics and charts in the results/study_name/graphiques folder
#' @export
make_outputs <- function(data_outputs, 
                         area,
                         decisions, 
                         max_loop){
  
  graph_dir = paste(study_dir, 'graphes', area, sep ='/')
  dir.create(graph_dir, recursive = TRUE)
  
  out = build_from_data_outputs(data_outputs = data_outputs, 
                                decisions = decisions, 
                                max_loop = max_loop)
  
  spot = out$spot
  revenues = out$revenues
  power_plants = out$power_plants
  LOLD = out$LOLD
  production = out$production
  power_plants_moves = out$power_plants_moves
  energy_costs_consumers = out$energy_costs_consumers
  crm_supply = out$crm_supply
  crm_demand = out$crm_demand
  auction_info = out$auction_info
  production_total_hourly = out$production_total_hourly
  balance_hourly = out$balance_hourly
  spot_hourly = out$spot_hourly
  load_hourly = out$load_hourly
  LOLD_hourly = out$LOLD_hourly
  
  if (capacity_mechanism$enabled & area %in% names(CRM)){     
    
    ro_strike = out$ro_strike
    build_crm_curves_chart(graph_dir = graph_dir,
                           crm_supply = crm_supply,
                           crm_demand = crm_demand,
                           auction_info = auction_info,
                           ro_strike = ro_strike,
                           zone = area)
  }
  #Build graphs for each country 
  build_revenues_chart(graph_dir = graph_dir,
                       revenues = revenues, 
                       zone = area)
  
  build_powerfleet_chart(graph_dir = graph_dir, 
                         power_plants = power_plants, 
                         zone = area)
  
  build_production_chart(graph_dir = graph_dir, 
                         production = production,
                         zone = area)
  
  if (area == areas[length(areas)]){
    #Build graphs aggregated on europe 
    build_spot_chart(spot = spot, 
                     max_loop = max_loop)
    
    build_capacity_shares_chart(power_plants = power_plants)
    
    build_lold_chart(LOLD = LOLD, 
                     max_loop = max_loop)
    
    if (!static_mode$enabled) build_power_plants_moves_chart(power_plants_moves = power_plants_moves)
    
    build_powerfleet_europe_chart(power_plants = power_plants, 
                                  max_loop = max_loop)
    
    crm_file_appendix = glue::glue('{demand$type}')
    if (reliability_options$enabled){
      if (reliability_options$dynamic_strike_price$enabled){
        crm_file_appendix = glue::glue("{demand$type}_RO_and_dynamic_strike_price")
      } else {
        crm_file_appendix = glue::glue("{demand$type}_RO_and_fixed_strike_price")
      }
    }
    
    #Store outputs in excel files
    file_revenues = paste(study_dir, glue::glue('revenues_{crm_file_appendix}.xlsx'), sep = '/')
    write.xlsx(revenues, file_revenues, row.names = FALSE)
    
    file_spot = paste(study_dir, 'spot_prices.xlsx', sep = '/')
    write.xlsx(spot,file_spot, row.names = FALSE)
    
    file_power_fleet = paste(study_dir, 'power_fleet.xlsx', sep = '/')
    write.xlsx(power_plants,file_power_fleet, row.names = FALSE)
    
    file_LOLD = paste(study_dir, 'LOLD.xlsx', sep = '/')
    write.xlsx(LOLD,file_LOLD, row.names = FALSE)
    
    file_production = paste(study_dir, 'production.xlsx', sep = '/')
    write.xlsx(production, file_production, row.names = FALSE)
    
    if(capacity_mechanism$enabled & area %in% names(CRM)){
      file_crm_demand = paste(study_dir, glue::glue('crm_demand_side_{crm_file_appendix}.xlsx'), sep = '/')
      write.xlsx(crm_demand, file_crm_demand, row.names = FALSE)
      
      file_crm_supply = paste(study_dir, glue::glue('crm_supply_side_{crm_file_appendix}.xlsx'), sep = '/')
      write.xlsx(crm_supply %>% filter(cluster_name != ''), file_crm_supply, row.names = FALSE)
      
      file_energy_costs_consumers = paste(study_dir, glue::glue('energy_costs_consumers_{crm_file_appendix}.xlsx'), sep = '/')
      write.xlsx(energy_costs_consumers, file_energy_costs_consumers, row.names = FALSE)
      
      file_auction_info = paste(study_dir, glue::glue('auction_info_{crm_file_appendix}.xlsx'), sep = '/')
      write.xlsx(auction_info, file_auction_info, row.names = FALSE)
      
      if (reliability_options$enabled){
        file_ro = paste(study_dir, glue::glue('ro_strike_price_{crm_file_appendix}.xlsx'), sep = '/')
        write.xlsx(ro_strike, file_ro, row.names = FALSE)
      }
      
      if (static_mode$enabled){
        # Hourly variables are considered only for a static mode due to the size of data
        file_production_total_hourly = paste(study_dir, 'production_total_hourly.csv', sep = '/')
        write.csv(production_total_hourly, file_production_total_hourly, row.names = FALSE)
        
        file_spot_hourly = paste(study_dir, 'spot_hourly.csv', sep = '/')
        write.csv(spot_hourly, file_spot_hourly, row.names = FALSE)
        
        file_load_hourly = paste(study_dir, 'load_hourly.csv', sep = '/')
        write.csv(load_hourly, file_load_hourly, row.names = FALSE)
        
        file_LOLD_hourly = paste(study_dir, 'LOLD_hourly.csv', sep = '/')
        write.csv(LOLD_hourly, file_LOLD_hourly, row.names = FALSE)
        
        file_balance_hourly = paste(study_dir, 'balance_hourly.csv', sep = '/')
        write.csv(balance_hourly, file_balance_hourly, row.names = FALSE)
      }
    }
    if (! static_mode$enabled){ # Investment decisions are only available for non static run
      file_power_plants_moves = paste(study_dir, 'power_plants_moves.xlsx', sep = '/')
      write.xlsx(power_plants_moves %>% as.data.frame(.), file_power_plants_moves, row.names = FALSE, sheetName = 'aggregated', append = TRUE)
      
      tmp = lapply(areas, function(area) if (nrow(decisions[[area]]$power_plants_moves) != 0) write.xlsx(decisions[[area]]$power_plants_moves %>% as.data.frame(.),sheetName = area,
                                                                                                         file = paste(study_dir, 'power_plants_moves.xlsx', sep = '/'),
                                                                                                         append = TRUE, row.names = FALSE) else NULL)
    }
  }
}


#' Get data outputs to store/record data from the model
#'
#' @param dataset dataset from antares runs outputs
#' @param power_plants power_plants dataframe 
#' @param year_actual (int) actual year of the model
#' @param setup (bool) if TRUE, function returns empty dataframe ready to store data
#' @param data_outputs (list of dataframe) where results will be stored
#' @param dataset_revenues (list of dataframe) revenues by cluster and year returned by analysis_revenues_total
#' @param s (int) loop number on the actual year
#' @param investment_metrics (dataframe) investment metrics for the year actual and loop s 
#'
#' @import xlsx
#' @import glue
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr set_names map
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate
#'
#' @return variables to store data from the model : power_plants_moves & data_outputs
#' @export
get_data_outputs_model <- function(dataset = NA,
                                   dataset_revenues = NA,
                                   power_plants = NA,
                                   year_actual = NA, 
                                   data_outputs = NA, 
                                   continue = NA, 
                                   investment_metrics = NA,
                                   s = NA){
  
  s = as.character(s)
  year_actual = as.character(year_actual)
  
  print(glue("\n\n => Storing outputs for loop [{s}] - [{year_actual}] and ({toupper(areas) %>% paste(.,collapse = '-')})"))
  for (zone in areas) {
    #Updating data real with the evolution on the s run of loop
    power_plants[[year_actual]][[zone]] %<>% na.omit(.) %>% set_rownames(NULL) 
    data_outputs[[zone]][[year_actual]][[s]]$power_plants = power_plants[[year_actual]][[zone]][,c('cluster','group','unitcount','nominalcapacity')]
    data_outputs[[zone]][[year_actual]][[s]]$revenues = dataset_revenues[[year_actual]][[zone]]$market_info_supply[,c('cluster_name','group' , 'energy_revenues_MW', 'revenues_crm_MW', 'heat_revenues_MW', 'system_service_revenues_MW', 'rendered_revenues_MW')]
    data_outputs[[zone]][[year_actual]][[s]]$spot = dataset[[year_actual]][[zone]]$data_antares$areas_annual[,c('area', 'MRG. PRICE')][area == zone]$`MRG. PRICE`
    data_outputs[[zone]][[year_actual]][[s]]$LOLD = dataset[[year_actual]][[zone]]$data_antares$areas_annual[,c('area', 'LOLD')][area == zone]$LOLD
    
    # Production averaged on MC years by clusters; little process specific
    cluster_list = names(dataset_revenues[[year_actual]][[zone]]$revenues)
    production = data.frame(cluster = character(),
                            group = character(),
                            production = numeric())
    for (c in cluster_list){
      new_row = list(cluster = c, 
                     group = power_plants[[year_actual]][[zone]] %>% filter(cluster == c) %>% select('group') %>% as.character(.),
                     production = mean(colSums(dataset_revenues[[year_actual]][[zone]]$revenues[[c]]$production)))
      production = rbind(production, new_row)
    }
    
    data_outputs[[zone]][[year_actual]][[s]]$production = production
    
    # Clearing price of capacity market & supply_curve
    if (capacity_mechanism$enabled & zone %in% names(CRM)){
      df_revenues = dataset_revenues[[year_actual]][[zone]]
      df = dataset[[year_actual]][[zone]]
      data_outputs[[zone]][[year_actual]][[s]]$crm_supply_curve = df_revenues$crm_supply_curve
      data_outputs[[zone]][[year_actual]][[s]]$crm_demand_curve = df_revenues$crm_demand_curve
      data_outputs[[zone]][[year_actual]][[s]]$energy_costs_consumers = (df$spot * (df$load - df$energy_not_served)) %>% colSums(.) %>% mean(.)
      data_outputs[[zone]][[year_actual]][[s]]$auction_info = df_revenues$auction_info
      
      if (static_mode$enabled){ # Hourly variables are considered only for a static mode due to the size of data
        data_outputs[[zone]][[year_actual]][[s]]$spot_hourly = df$spot
        data_outputs[[zone]][[year_actual]][[s]]$load_hourly = df$load
        data_outputs[[zone]][[year_actual]][[s]]$LOLD_hourly = df$energy_not_served
        data_outputs[[zone]][[year_actual]][[s]]$balance_hourly = df$balance_hourly 
        data_outputs[[zone]][[year_actual]][[s]]$production_total_hourly = df$production_total_hourly
      }
      
      # RO option with or without strike price by capacity
      if (reliability_options$enabled & reliability_options$by_capacity) {
        data_outputs[[zone]][[year_actual]][[s]]$ro_strike_price = df_revenues$ro_strike_price %>% 
          as.data.frame(.) %>% setNames(lapply(names(.), gsub, pattern = '.', replacement = '_', fixed = TRUE))
      }
      else {data_outputs[[zone]][[year_actual]][[s]]$ro_strike_price = df_revenues$ro_strike_price}
    }
  }
  
  #Complete loops not filled because no decisions made on loop s
  if (continue == 0 & as.integer(s) < iteration_max &!static_mode$enabled){
    loop_to_complete = (as.integer(s) + 1) :iteration_max
    print(glue("\n=> Completing data_outputs for missing loops for loops ({loop_to_complete %>% paste(.,collapse = '-')})"))
    for (zone in areas){
      for (l in loop_to_complete){
        data_outputs[[zone]][[year_actual]][[as.character(l)]] = data_outputs[[zone]][[year_actual]][[s]]
      }                                                                   
    }
  }
  
  if (!static_mode$enabled){ # Investment only available for non static run
    record_investment_metrics()
  }
  return(data_outputs)
}

#' Reformat data from data_outputs in order to seperate interesting variables
#'
#' @param data_outputs outputs recorded from the model
#' @param decisions investments/decomissioning decisions
#' @param max_loop number maximum of really run loops over years
#' 
#' @importFrom reshape2 melt dcast
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr bind_rows
#' @return list of lists with interesting variables  
#' @export
build_from_data_outputs<- function(data_outputs, 
                                   decisions, 
                                   max_loop){
  
  #Reformatting data from dictionnary of metrics to metrics' dictionnary
  LOLD = list()
  revenues = list()
  power_plants = list()
  spot = list()
  production = list()
  crm_supply = list()
  crm_demand = list()
  ro_strike = list()
  energy_costs_consumers = list()
  balance_hourly = list()
  production_total_hourly = list()
  spot_hourly = list()
  load_hourly = list()
  LOLD_hourly = list()
  auction_info = list()
  
  # Remove useless loops first 
  data_outputs = remove_useless_loops(data_outputs = data_outputs, 
                                      max_loop = max_loop)
  
  for (area in areas) {
    for (year in names(data_outputs[[area]])){
      for (s in names(data_outputs[[area]][[year]])) {
        out = data_outputs[[area]][[year]][[s]]
        
        LOLD[[area]][[year]] = c(LOLD[[area]][[year]], out$LOLD)
        revenues[[area]][[year]][[s]] = out$revenues
        power_plants[[area]][[year]][[s]] = out$power_plants
        power_plants[[area]][[year]][[s]]$total_capacity = power_plants[[area]][[year]][[s]]$unitcount * power_plants[[area]][[year]][[s]]$nominalcapacity
        spot[[area]][[year]] = c(spot[[area]][[year]], out$spot)
        production[[area]][[year]][[s]] = out$production
        
        if (capacity_mechanism$enabled & area %in% names(CRM)){ # CRM variable
          crm_supply[[area]][[year]][[s]] = out$crm_supply_curve
          crm_demand[[area]][[year]][[s]] = out$crm_demand_curve
          energy_costs_consumers[[area]][[year]] = c(energy_costs_consumers[[area]][[year]],out$energy_costs_consumers)
          auction_info[[area]][[year]][[s]] = out$auction_info
          
          if(static_mode$enabled){ # Hourly variables are considered only for a static mode due to the size of data
            balance_hourly[[area]][[year]][[s]] = out$balance_hourly 
            production_total_hourly[[area]][[year]][[s]] = out$production_total_hourly
            spot_hourly[[area]][[year]][[s]] = out$spot_hourly
            LOLD_hourly[[area]][[year]][[s]] = out$LOLD_hourly
            load_hourly[[area]][[year]][[s]] = out$load_hourly
          }
          
          if (reliability_options$enabled){
            if (!reliability_options$by_capacity){
              ro_strike[[area]][[year]] = c(ro_strike[[area]][[year]],out$ro_strike_price)
            }
            else{
              ro_strike[[area]][[year]][[s]] = out$ro_strike_price 
            }
          }
        }
      }
    }
    LOLD[[area]] %<>% as.data.frame(.) %>% setNames(names(data_outputs[[area]]))
    spot[[area]] %<>% as.data.frame(.) %>% setNames(names(data_outputs[[area]]))
  }
  # Reshaping for numerical capacity market outputs
  if (capacity_mechanism$enabled & area %in% names(CRM)){
    energy_costs_consumers[[area]] %<>% as.data.frame(.) %>% setNames(names(data_outputs[[area]]))
    if(reliability_options$enabled & !reliability_options$by_capacity){
      ro_strike[[area]] %<>% as.data.frame(.) %>% setNames(names(data_outputs[[area]]))
    }
  }
  
  ### -------- Melting data to get them prepared for graphics -------- ###
  LOLD %<>% melt(., id.vars = NULL)  %>%  mutate(loop = rep(rownames(LOLD[[1]]), length(colnames(LOLD[[1]])) * length(areas))) %>%
    setNames(c('year', 'LOLD', 'area', 'loop'))
  
  revenues %<>% melt(., id.vars = c('cluster_name', 'group')) %>% 
    setNames(c('cluster_name', 'group', 'type_revenues', 'revenues', 'loop', 'year', 'area'))
  
  power_plants %<>% melt(., id.vars = c('cluster', 'group')) %>% 
    setNames(c('cluster_name','group' ,'variable', 'value', 'loop', 'year', 'area'))
  
  spot %<>% melt(., id.vars = NULL) %>% mutate(loop = rep(rownames(spot[[1]]), length(colnames(spot[[1]])) * length(areas))) %>% 
    setNames(c('year', 'spot', 'area','loop'))
  
  production %<>% melt(., id.vars = c('cluster', 'group')) %>% 
    setNames(c('cluster_name','group', 'variable','production', 'loop', 'year', 'area'))
  
  
  if (capacity_mechanism$enabled & area %in% names(CRM)){
    
    # Melting doesn't work as expected with id variables for auction info, special process here
    tmp_auction_info = list()
    for (area in areas) {
      tmp_auction_info[[area]] = lapply(auction_info[[area]],function(x) bind_rows(x,.id = 'loop')) %>% bind_rows(., .id = 'year')
    }
    auction_info = tmp_auction_info %>% bind_rows(., .id = 'area')
    
    energy_costs_consumers %<>% melt(., id.vars = NULL)  %>%  mutate(loop = rep(rownames(energy_costs_consumers[[1]]), length(colnames(energy_costs_consumers[[1]])) * length(areas))) %>%
      setNames(c('year', 'energy_costs_consumers_euros', 'area', 'loop'))
    
    crm_supply %<>% melt(.,id.vars = c("cluster_name","capacity_MW","capacity_cumulative","bid")) %>%
      setNames(., c("cluster_name","capacity_MW","capacity_cumulative","bid","loop","year","area"))
    
    crm_demand %<>% melt(.,id.vars = c("capacity_cumulative","price")) %>%
      setNames(., c("capacity_cumulative","price","loop","year","area"))
    
    if (reliability_options$enabled & reliability_options$by_capacity) {
      ro_strike %<>% melt(., id.vars = NULL) %>% setNames(c("cluster_name","strike_price","loop", "year","area"))
    }
    if (reliability_options$enabled & !reliability_options$by_capacity) {
      ro_strike %<>% melt(., id.vars = NULL) %>% mutate(loop = rep(rownames(ro_strike[[1]]), length(colnames(ro_strike[[1]])) * length(areas))) %>%
        setNames(c("year","strike_price","area", "loop"))
    }
    
    # Specific variable manipulation because hourly variable      
    if (static_mode$enabled){
      
      tmp_balance_hourly = list()
      tmp_production = list()
      tmp_spot_hourly = list()
      tmp_balance_hourly = list()
      tmp_LOLD_hourly = list()
      tmp_load_hourly = list()
      
      for (area in areas) {
        tmp_balance_hourly[[area]] = lapply(balance_hourly[[area]],function(x) bind_rows(x,.id = 'loop')) %>% bind_rows(., .id = 'year') 
        tmp_production[[area]] = lapply(production_total_hourly[[area]],function(x) bind_rows(x,.id = 'loop')) %>% bind_rows(., .id = 'year') 
        tmp_spot_hourly[[area]] = lapply(spot_hourly[[area]],function(x) bind_rows(x,.id = 'loop')) %>% bind_rows(., .id = 'year')
        tmp_LOLD_hourly[[area]] = lapply(LOLD_hourly[[area]],function(x) bind_rows(x,.id = 'loop')) %>% bind_rows(., .id = 'year')
        tmp_load_hourly[[area]] = lapply(load_hourly[[area]],function(x) bind_rows(x,.id = 'loop')) %>% bind_rows(., .id = 'year')
      }
      balance_hourly = tmp_balance_hourly %>% bind_rows(., .id = 'area')
      production_total_hourly = tmp_production %>% bind_rows(., .id = 'area')
      spot_hourly = tmp_spot_hourly %>% bind_rows(., .id = 'area')
      load_hourly = tmp_load_hourly %>% bind_rows(., .id = 'area')
      LOLD_hourly = tmp_LOLD_hourly %>% bind_rows(., .id = 'area')
    }
  }
  #Reformatting power_plants moves
  if(!static_mode$enabled){
    power_plants_moves = lapply(areas,function(area) {decisions[[area]][-1]}) %>% setNames(., areas) %>% 
      melt(., id.vars = c('name', 'year_decision', 'year_start','unitcount', 'nominalcapacity', 'loop', 'movement')) %>% 
      .[,-which(names(.) %in% c('variable', 'value', 'L2'))] %>%
      setNames(c('name', 'year_decision', 'year_start', 'unitcount', 'nominalcapacity', 'loop', 'movement', 'area'))
    
    power_plants_moves %<>% mutate(year_start = as.factor(.$year_start),
                                   year_decision = as.factor(.$year_decision)) 
    
    power_plants_moves$nominalcapacity = sapply(rownames(power_plants_moves), function(row) {if(power_plants_moves[row, 'movement'] == 'retirement'){
      power_plants_moves[row, 'nominalcapacity'] = - power_plants_moves[row, 'nominalcapacity']
    }else {
      power_plants_moves[row, 'nominalcapacity'] = power_plants_moves[row, 'nominalcapacity']}})
    
    power_plants_moves %<>% .[!duplicated(.),] %>%
      group_by(year_start, area) %>% 
      summarise(.,capacity = sum(nominalcapacity)) %>%
      dcast(., formula = year_start ~ area, value.var = c('capacity'))
    
    power_plants_moves[is.na(power_plants_moves)] = 0
    power_plants_moves %<>% melt(., id.vars = c('year_start')) %>% setNames(., c('year_start', 'area', 'capacity'))
  } else {
    power_plants_moves = list()
  }
  return(list(spot = spot,
              power_plants = power_plants,
              LOLD = LOLD,
              revenues = revenues, 
              production = production,
              power_plants_moves = power_plants_moves,
              crm_demand = crm_demand,
              crm_supply = crm_supply,
              ro_strike = ro_strike, 
              auction_info = auction_info,
              energy_costs_consumers = energy_costs_consumers, 
              balance_hourly = balance_hourly, 
              production_total_hourly = production_total_hourly,
              spot_hourly = spot_hourly,
              LOLD_hourly = LOLD_hourly,
              load_hourly = load_hourly))
}
