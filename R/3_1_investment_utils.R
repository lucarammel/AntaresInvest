
#' Compute NPV (unit /MW installed -> all costs and revenues are per MW installed)
#'
#' @param revenues_energy energy only revenues (euros/MW installed)
#' @param revenues_crm capacity market revenues (euros/MW installed)
#' @param investment_cost investment costs (euros/MW.year installed)
#' @param operational_cost operational costs (euros/MW.year installed)
#' @param year_start the starting year of the computation
#' @param discount_rate discount rate 
#'
#' @return NPV
#' @export
NPV <- function(revenues_energy,
                revenues_crm,
                investment_cost,
                operational_cost,
                year_start, 
                years_to_simulate, 
                discount_rate){
  
  nbr_year = length(years_to_simulate)
  computed_year = time_horizon - year_start + 1
  discount_rate = 1 / (1+discount_rate) ^ (1: computed_year)
  result = sum((revenues_energy + revenues_crm - operational_cost) * discount_rate ) - nbr_year * investment_cost
  
  return(result)
}

#' Profitability index define as NPV / IC
#'
#' @param NPV Net present value 
#' @param investment_cost investment costs (euros/MW installed)
#'
#' @return Profitability index
#' @export
PI <- function(NPV,
               investment_cost){
  
  result = NPV / investment_cost
  return(result)
}


#' Internal rate of return
#'
#' @param revenues_energy energy only revenues (euros/MW installed)
#' @param revenues_crm capacity market revenues (euros/MW installed)
#' @param investment_cost investment costs (euros/MW installed)
#' @param operational_cost operational costs (euros/MW installed)
#' @param current_year the starting year of the computation
#'
#' @return Internal rate of return
#' @export
IRR <- function(revenues_energy,
                revenues_crm,
                investment_cost,
                operational_cost,
                year_start, 
                years_to_simulate){
  
  root = uniroot(function(r) NPV(revenues_energy, revenues_crm,investment_cost,
                                 operational_cost, year_start, years_to_simulate, r), interval = c(0,1))
  
  return(root[[1]])
}


#' Expected net profit
#'
#' @param year_end the ending year of the expected net profit computation (different from time horizon)
#' @param revenues_energy energy only revenues (euros/MW installed)
#' @param revenues_crm capacity market revenues (euros/MW installed)
#' @param operational_cost operational costs (euros/MW installed)
#' @param year_start the starting year of the computation
#'
#' @return expected net profit of the cluster 
#' @export
ENP <- function(year_end,
                year_start,
                revenues_energy,
                revenues_crm,
                operational_cost){
  
  computed_year = year_end - year_start + 1
  discount_rate = 1 / (1+discount_rate) ^ (1: computed_year)
  result =  sum((revenues_energy + revenues_crm - operational_cost) * discount_rate)
  
  return(result)
}


#' Prepare investment metrics for the investment model
#'
#' @param year_actual the actual year of the model
#' @param years_to_simulate (int vector) years to simulate
#' @param clusters_not_supported (str vector) interesting clusters for revenues analysis and investments decisions
#' @param dataset_revenues (list of datatable) revenues and production list by clusters and years 
#' @param dataset (list of datatable) dataset of outputs runs from antares from build dataset
#' @param area (str) country code
#' @param power_plants (dataframe) powerfleet hypothesis dataframe
#' 
#' @return metrics table, dataframe of revenues, candidates decommissioning
#' @export
prepare_investment_metrics <- function(year_actual,
                                       years_to_simulate,
                                       clusters_not_supported,
                                       dataset_revenues, 
                                       dataset, 
                                       power_plants,
                                       area){
  
  #Updating clusters_filters with the one already decomissioned by the antares studies dataset
  dataset = update_clusters_filters(dataset, 
                                    area)
  
  policy_retired = dataset[[1]][[area]]$clusters_filters$policy_retired
  candidates_investment = dataset[[1]][[area]]$clusters_filters$candidates_investment
  candidates_retirement = dataset[[1]][[area]]$clusters_filters$candidates_retirement
  
  #Compute the myopic factors
  myopic_factors = exp(-c(as.integer(years_to_simulate) - year_actual) / (as.integer(years_to_simulate[length(years_to_simulate)]) - year_actual + 1))
  myopic_factors = myopic_factors / sum(myopic_factors)
  
  #Choosing the first year because used for costs that are not changing with times
  metrics = list(NPV = c(),
                 PI = c())
  revenues_total_net = list()

  if (length(clusters_not_supported) == 0) {
    # Avoid crash if no more powerplants to be considered
    return(list(metrics = NULL, 
                revenues_total_net = NULL,
                policy_retired = NULL,
                candidates_investment = NULL,
                candidates_retirement = NULL))}

  for (cluster in clusters_not_supported) {
    #Extract data for the calculation of NPV, ENP etc..
    data = get_data_for_metric(cluster = cluster,
                               years_to_simulate = years_to_simulate,
                               power_plants = power_plants,
                               dataset_revenues = dataset_revenues, 
                               area = area)
    
    #Put the the revenues in the good format for the calculation of NPV (ie from a 1x3years vector to a 1xNyears vector)
    data_reshaped = lapply(data, function(x) build_vector_npv(revenues_vector = x, 
                                                              years_to_simulate = years_to_simulate,
                                                              year_start = year_actual,
                                                              replace = TRUE))
    
    #Compute the net revenues on the years to simulate
    if ( interpolate_revenues & !(cluster %in% policy_retired) ){
      ### Interpolate revenues with approx function
      data_reshaped$om_costs %<>% rep(max(.),length(.))
      data_reshaped$revenues_MW %<>% replace(.,.==0,NA) %>% approx(x=1:length(.), y=., n = length(.)) %>% .$y
      
    }
    revenues_total_net[[cluster]] = data_reshaped$revenues_MW + data_reshaped$revenues_crm_MW - data_reshaped$om_costs
    
    #Compute NPV on years_to_simulate
    NPV_simulated = NPV(revenues_energy = data_reshaped$revenues_MW,
                        revenues_crm = data_reshaped$revenues_crm_MW,
                        investment_cost = data$investment_costs,
                        operational_cost = data_reshaped$om_costs,
                        year_start = year_actual, 
                        years_to_simulate = years_to_simulate,
                        discount_rate = discount_rate)
    
    metrics[['NPV']] = c(metrics[['NPV']], NPV_simulated)
    
    metrics[['PI']] = c(metrics[['PI']], PI(NPV = NPV_simulated,
                                            investment_cost = data$investment_costs))
    
    # metrics[['IRR']] = c(metrics[['IRR']], IRR(revenues_energy = revenues_MW,
    #                                           revenues_crm = revenues_crm_MW,
    #                                           investment_cost = investment_costs,
    #                                           operational_cost = om_costs,
    #                                           year_start = year_actual, 
    #                                           years_to_simulate = years_to_simulate))
  }
  
  #Reformat dataframe of net revenues (from a list of years to a dataframe with years as columns) 
  revenues_total_net = reformat_df_revenues_total(revenues_total_net = revenues_total_net,
                                                     years_to_simulate = years_to_simulate)
  #Prepare metrics data for outputs 
  metrics = data.frame(clusters = clusters_not_supported,
                       metric = metrics[[metric]])
  names(metrics)[names(metrics) == 'metric'] = metric 
  
  return(list(metrics = metrics,
              revenues_total_net = revenues_total_net,
              policy_retired = policy_retired, 
              candidates_retirement = candidates_retirement, 
              candidates_investment = candidates_investment))
}

#'Update the clusters interesting with the candidates to decommissioning already mentioned in antares studies
#'
#' @param dataset the dataset coming out from the build dataset function
#' @param area (str) country code
#'
#' @return updated clusters interesting for every simulated years
#' @export
update_clusters_filters <- function(dataset, 
                                    area){
  
  df = dataset[[1]][[area]]
  for (y in names(dataset)){
    for (c in c(df$clusters_filters$supported, df$clusters_filters$not_supported)){
      
      #Going across every clusters of the first year simulated
      if (is.na(prod(unlist(lapply(1:length(dataset), function(i) df$data_antares$cluster_description[cluster ==c]$unitcount)))) == TRUE){
        
        if (!is.na(df$data_antares$cluster_description[cluster ==c]$unitcount)){
          #If products = 0 & the first year unitcount != 0, the cluster is already decommissioned by the antares studies
          dataset[[as.character(y)]][[area]]$clusters_filters$policy_retired =
            c(c, dataset[[as.character(y)]][[area]]$clusters_filters$policy_retired) 
        }
      }
    }
  }
  return(dataset)
}
