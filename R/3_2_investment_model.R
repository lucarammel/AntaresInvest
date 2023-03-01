#' Investment basic : take the best/the worst capacity and invest/decommision
#'
#' @param year_actual (int) the actual year of the model
#' @param power_plants (dataframe) with costs hypothesis
#' @param investment_metrics (dataframe) with the investment metrics (ie revenues and metrics such as NPV or IRR here)
#' @param area (str) country code
#' @param decisions (list) investments/ decommissionning decisions 
#' @param LOLD (scalar) Loss of load duration
#' @param s (int) loop step
#'
#' @return power_plants_moves : the record of all movements of capacities.
#' @export
investment_model_base <- function(year_actual,
                                  power_plants,
                                  investment_metrics, 
                                  area, 
                                  decisions,
                                  LOLD = NA,
                                  s){
  
  print(glue('\n\n => Making decisions for country [{toupper(area)}] :'))
  
  #Get all filters and tables needed for investment decisions.
  power_plants_moves = decisions[[area]]$power_plants_moves
  metrics = investment_metrics[[area]]$metrics
  revenues_total_net = investment_metrics[[area]]$revenues_total_net
  policy_retired = investment_metrics[[area]]$policy_retired
  candidates_retirement = investment_metrics[[area]]$candidates_retirement
  candidates_investment = investment_metrics[[area]]$candidates_investment

  if (is.null(metrics) & is.null(revenues_total_net)){
    print(glue('=> Not enough candidates for [{toupper(area)}]. End of EVA.'))
    return(list(continue = 0, 
                power_plants_moves = data.frame(power_plants_moves)))}

  if (adaptative_power_increment) {
    power_increment[[area]] = round(abs((LOLD - reliability_standard[[area]]) / reliability_standard[[area]]) * power_increment[[area]])
    if (s >= round(iteration_max * 0.75)){
      #Last loops, increment are fixed to miminum increment
      power_increment[[area]] = min(power_increment[[area]],minimum_increment)
    }
  } 
  print(glue('\n=> Power increment for [{toupper(area)}] : {power_increment[[area]]} MW'))
  ###Select worst based on merit order on revenues
  worst_capacity = select_worst_capacity(revenues_total_net = revenues_total_net,
                                         year_actual = year_actual, 
                                         policy_retired = policy_retired, 
                                         candidates_retirement = candidates_retirement, 
                                         area = area, 
                                         LOLD = LOLD)
    
  if (worst_capacity != 'end'){
    #If worst capacity has be found add it to powerplants moves only if limit retirement not reached
    id_move = nrow(power_plants_moves) + 1 
    unit_increment = min(power_plants[worst_capacity, 'unitcount'], ceiling(power_increment[[area]] / power_plants[worst_capacity, 'nominalcapacity']))
    nominalcapacity = power_plants[worst_capacity, 'nominalcapacity'] * unit_increment
    
    #Check if limits of decommissioning are reached
    grouped_retired = power_plants_moves %>% filter(movement == 'retirement') %>% group_by(year_start, movement) %>% summarise(.,nominalcapacity = sum(nominalcapacity))
    old_retired = grouped_retired[grouped_retired$year_start == (year_actual + retirement_time),'nominalcapacity'] %>% as.numeric(.)
    new_retired = old_retired + nominalcapacity
    if (is.na(new_retired)) new_retired = 0
    
    if (new_retired > limit_retired[[area]] | ((nominalcapacity < minimum_increment) & (unit_increment != power_plants[worst_capacity, 'unitcount']))){
      continue_worst_capacity = FALSE
      if (new_retired > limit_retired[[area]]) print(glue('\n => [{toupper(area)}] - Retirement limit reached'))
      if (nominalcapacity < minimum_increment) print(glue('\n => [{toupper(area)}] - Nominal capacity not high enough : [{nominalcapacity}] MW'))
    }
    else if (new_retired <= limit_retired[[area]]){
      power_plants_moves = rbind(power_plants_moves,
                                 list(id_move = id_move,
                                      name = worst_capacity,
                                      year_decision = year_actual,
                                      unitcount = unit_increment,
                                      nominalcapacity = nominalcapacity,
                                      construction_period = retirement_time, #retirement is effective on the next year. 
                                      year_start = year_actual + retirement_time,
                                      movement = 'retirement',
                                      loop = s, 
                                      done = FALSE))
      
      continue_worst_capacity = TRUE
    }
  }
  else if(worst_capacity == 'end'){
    continue_worst_capacity = FALSE
  }
  
  if(allow_invest){
    #Get the best NPV according to merit order
    out = select_best_capacity(metrics = metrics,
                               year_actual = year_actual,
                               candidates_investment = candidates_investment,
                               LOLD = LOLD, 
                               area = area, 
                               revenues_total_net = revenues_total_net)
    best_capacity = out$best_capacity
    delay_investment_need = out$delay_investment_need                           
    
    if (best_capacity != 'end') {
      #If best capacity has be found add it to powerplants moves only if limit investment not reached
      id_move = nrow(power_plants_moves) + 1 
      
      unit_increment = min(power_plants[best_capacity, 'unitcount'], ceiling(power_increment[[area]] / power_plants[best_capacity, 'nominalcapacity']))
      nominalcapacity = power_plants[best_capacity, 'nominalcapacity'] * unit_increment
      
      #Check if limits of investment are reached
      grouped_invest = power_plants_moves %>% filter(movement == 'investment') %>% group_by(year_start, movement) %>% summarise(.,nominalcapacity = sum(nominalcapacity))
      old_invest = grouped_invest[grouped_invest$year_start == year_actual + power_plants[best_capacity,'construction_period_year'],'nominalcapacity'] %>% as.numeric(.)
      new_invest = old_invest + nominalcapacity
      
      if (is.na(new_invest)) new_invest = 0
      if (delay_investment_need) year_decision = year_actual + time_horizon_investor else year_decision = year_actual
      
      if (new_invest > limit_investment[[area]] | ((nominalcapacity < minimum_increment) & (unit_increment != power_plants[best_capacity, 'unitcount']) )){
        continue_best_capacity = FALSE
        if (new_invest > limit_investment[[area]]) print(glue('\n => [{toupper(area)}] - Investment limit reached'))
        if (nominalcapacity < minimum_increment) print(glue('\n => [{toupper(area)}] - Nominal capacity not high enough : [{nominalcapacity}] MW'))
      }
      else if (new_invest <= limit_investment[[area]]){  
        power_plants_moves = rbind(power_plants_moves,
                                   list(id_move = id_move,
                                        name = best_capacity,
                                        year_decision = year_decision,
                                        unitcount = unit_increment,
                                        nominalcapacity = nominalcapacity,
                                        construction_period = power_plants[best_capacity,'construction_period_year'],
                                        year_start = year_decision + power_plants[best_capacity,'construction_period_year'],
                                        movement = 'investment',
                                        loop = s, 
                                        done = FALSE))
        
        continue_best_capacity = TRUE
      }
    }
    else if(best_capacity == 'end'){
      #Set the continue variable for while loop over the same actual year to FALSE
      continue_best_capacity = FALSE
    }
  } else if (!allow_invest) {
    #Set the continue variable for while loop over the same actual year to FALSE because no investment allowed
    continue_best_capacity = FALSE
  }
  
  #Continue on the actual year if one decommissioning or one investment at least
  continue = continue_best_capacity | continue_worst_capacity
    
  return(list(continue = continue, 
              power_plants_moves = data.frame(power_plants_moves)))
}
#' Compute the worst capacity based on revenues of the year and the expected net profit
#'
#' @param revenues_total_net (dataframe) of net revenues for each clusters by year
#' @param year_actual (int) year actual
#' @param years_to_simulate (int vector)years_to_simulate 
#' @param policy_retired (str vector) candidates to retire already set in antares studies
#' @param candidates_retirement (str vector) clusters not allowed for decomissionning. Find in parameters.
#' @param LOLD (scalar) Loss of load duration
#' @param area (str) country code 
#'
#' @importFrom glue glue
#' @importFrom dplyr %>% select filter arrange sym
#' @importFrom magrittr %<>% 
#'
#' @return worst cluster's name or string 'end'
#' @export
select_worst_capacity <- function(revenues_total_net,
                                  year_actual, 
                                  policy_retired, 
                                  candidates_retirement, 
                                  LOLD, 
                                  area){
  
  #Filtering with candidates to decommissioning, first with those decommissioned by the antares studies, then with the political one defined in params
  revenues_total_net %<>% filter(!rownames(.) %in% policy_retired & rownames(.) %in% candidates_retirement)
  
  #Ordering revenues
  ordered_revenues = revenues_total_net %>% arrange(!!sym(glue({year_actual})))
  worst_revenue = ordered_revenues[1,glue({year_actual})] #Revenues of the first year are firstly taking into account
  if (is.na(worst_revenue)) worst_revenue = 0
  
  if (!CRM_forced){
    return(select_worst_capacity_base(worst_revenue = worst_revenue,
                                      ordered_revenues = ordered_revenues,
                                      revenues_total_net = revenues_total_net,
                                      year_actual = year_actual))
  }
  else if (CRM_forced) {
    if (LOLD > reliability_standard[[area]]){
      print(glue('==> Criteria NOT reached: [{LOLD}h] - target : [{reliability_standard[[area]]}h}] - no retirement.'))
      return('end')}
    else if (LOLD < reliability_standard[[area]] & abs(reliability_standard[[area]] - LOLD) < LOLE_margin) {
      print(glue('==> Criteria reached and close to LOLE target : [{LOLD}h] - target : [{reliability_standard[[area]]}h}] - no retirement.'))
      return('end')}
    else {
      return(select_worst_capacity_base(worst_revenue = worst_revenue,
                                        ordered_revenues = ordered_revenues,
                                        revenues_total_net = revenues_total_net,
                                        year_actual = year_actual))}
  }
}

#' Compute the best candidates based on the NPV projected.
#'
#' @param metrics (dataframe) of metrics for each clusters
#' @param year_actual (int) year actual 
#' @param candidates_investment (dataframe) candidates to investments
#' @param LOLD (scalar) Loss of Load duration 
#' @param area (str) country code 
#' @param revenues_total_net (dataframe) of net revenues for each clusters by year
#'
#' @importFrom glue glue
#' @importFrom dplyr %>% select filter arrange sym
#'
#' @return best cluster's name or string 'end'
#' @export
select_best_capacity <- function(metrics,
                                 year_actual, 
                                 candidates_investment,
                                 LOLD, 
                                 area, 
                                 revenues_total_net = NA){
  #Filtering with candidates to investments
  ordered_metric = metrics %>% filter(clusters %in% candidates_investment) %>% arrange(desc(!!sym(metric)))
  best_metric = ordered_metric[1,metric]
  if (is.na(best_metric)) {
    print(glue('\n=> No more candidates. End of EVA for [{area}]'))
    
    return(list(best_capacity = 'end', 
                delay_investment_need = FALSE))}
  
  if (!CRM_forced){ 
    return(select_best_capacity_base(ordered_metric = ordered_metric, 
                                     best_metric = best_metric, 
                                     revenues_total_net = revenues_total_net, 
                                     year_actual = year_actual))
  }
  else if (CRM_forced){   
    if (LOLD > reliability_standard[[area]]){
      best_capacity = ordered_metric[1, 'clusters']
      print(glue('\n==> Criteria NOT reached : [{LOLD}h] - target : [{reliability_standard[[area]]}h] - best candidate : [{best_capacity}] \n'))
      
      return(list(best_capacity = best_capacity, 
                  delay_investment_need = FALSE))}
    
    else {
      return(select_best_capacity_base(ordered_metric = ordered_metric, 
                                       best_metric = best_metric, 
                                       revenues_total_net = revenues_total_net,
                                       year_actual = year_actual))
    }
  }
}

#' Base part of select_worst_capacity function
#'
#' @param worst_revenue table of metrics ordered
#' @param ordered_revenues value of the best metric
#' @param revenues_total_net dataframe of total net revenues by year and cluster
#' 
#' @importFrom magrittr set_rownames
#'
#' @return best_capacity name
select_worst_capacity_base <- function(worst_revenue,
                                       ordered_revenues, 
                                       revenues_total_net, 
                                       year_actual){
  if (worst_revenue >= minimum_revenue_margin ){
    print(glue('==> All clusters have positives revenues. No retirement for [{year_actual}]'))
    return('end')
  }
  else {
    worst_cluster = rownames(ordered_revenues)[1]
    #Compute the expected net profit and order them 
    if (revenues_total_net %>% ncol(.) >= time_horizon_ENP){
      ENPs = do.call(rbind, lapply(rownames(revenues_total_net), function(x) ENP(year_end = year_actual + time_horizon_ENP, 
                                                                                 year_start = year_actual,
                                                                                 revenues_energy = revenues_total_net[x,1:time_horizon_ENP], 
                                                                                 revenues_crm = 0, 
                                                                                 operational_cost = 0))) %>% set_rownames(rownames(revenues_total_net)) %>%setNames('ENP')
    
      if(ENPs[worst_cluster,] > 0) {
        #If revenues are negatives on year N, checking if revenues are positive on mid term simulation.
        print(glue('==> [{worst_cluster}] is non profitable for [{year_actual}] but expected net profit for next {time_horizon_ENP} years is positive - no retirement'))
        return('end')
      }
      else{
        print(glue('==> The less profitable cluster for [{year_actual}] is [{worst_cluster}]'))
        return(worst_cluster)
      }  
    }
    else {
      print(glue('==> The less profitable cluster for [{year_actual}] is [{worst_cluster}]. No projection assumed. Retire on current-year base. '))
      return(worst_cluster)
      }
  }
}


#' Base part of select_best_capacity function
#'
#' @param ordered_metric table of metrics ordered
#' @param best_metric value of the best metric
#' @param revenues_total_net dataframe of net revenues for each clusters by year
#'
#' @return best_capacity name
select_best_capacity_base <- function(best_metric, 
                                      ordered_metric, 
                                      year_actual,
                                      revenues_total_net = NA){
  if (best_metric <= 0){
    print(glue('\n==> All clusters have negatives {metric}, no investment decision. \n'))
    return(list(best_capacity = 'end', 
                delay_investment_need = FALSE))
  }
  else {
    best_capacity = ordered_metric[1, 'clusters']
    if (delay_investment_decision){
      if (revenues_total_net[best_capacity, 1:time_horizon_investor] %>% {. > 0} %>% prod(.) == 0) {
        print(glue('\n==> Cluster\'s best candidate is [{best_capacity}] for [{year_actual}]. Delay investment.\n'))
        return(list(best_capacity = best_capacity,
                    delay_investment_need = TRUE))
      }
      else{
        print(glue('\n==> Cluster\'s best candidate is [{best_capacity}] for [{year_actual}]. No delay. \n'))
        return(list(best_capacity = best_capacity,
                    delay_investment_need = FALSE))
      }
    }
    else {print(glue('\n==> Cluster\'s best candidate is [{best_capacity}] for [{year_actual}] \n'))
        return(list(best_capacity = best_capacity,
                    delay_investment_need = FALSE))
    }
  }
}
