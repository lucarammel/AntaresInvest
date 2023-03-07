
#' Compute availability on PP1 days
#'
#' @param power_plants dataframe with costs data for clusters
#' @param clusters_not_supported (str vector) clusters names candidates to revenues analysis and investments
#' @param data_antares (list of antare data table) all useful extracted data from the antares run
#' @param study_id (str) antares simulation id
#'
#' @importFrom reshape2
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr sym group_by summarise select filter
#' 
#' @return Thermal availability for every clusters (ie capacity certifiee) 
#' @export
compute_availability_total = function(power_plants,
                                      clusters_not_supported,
                                      data_antares){
  
  #Increase max memory in case of needed supplementary ram
  setRam(80)
  
  #Loading the load 
  load = data_antares$areas %>% filter(area == 'fr') %>% select(c('mcYear', 'timeId', 'LOAD')) %>% 
    group_by(timeId) %>% summarise(load = mean(LOAD)) %>% select(-c('timeId'))
  
  clusters_thermal = clusters_not_supported %>% .[!sapply(., get_cluster_type) %in% renewables_type]
  clusters_enr = clusters_not_supported %>% .[sapply(., get_cluster_type) %in% renewables_type]
  
  availability_list = lapply(clusters_thermal, function(c){data_antares$thermal_availability %>% 
      filter(cluster == c) %>% 
      select(c("cluster", 'mcYear', "timeId", "thermalAvailability"))  %>%
      reshape2::dcast(.,mcYear+timeId~cluster, value.var = "thermalAvailability") %>% 
      group_by(timeId) %>% summarise(c = mean(!!sym(c))) %>% select(-timeId)})
  
  if (length(availability_list) > 0) {
    availability_thermal = do.call(cbind,availability_list) %>% setNames(clusters_thermal)
  }else{
    availability_thermal = NULL
  }
  
  availability_list = lapply(clusters_enr, function(c) {data_antares$clusters_RES %>% 
      filter(cluster == c) %>%
      select(c("cluster","mcYear" ,"timeId", "production")) %>%
      dcast(.,mcYear+timeId~cluster, value.var = "production") %>% 
      group_by(timeId) %>% summarise(c = mean(!!sym(c))) %>% select(-timeId)})
  
  if (length(availability_list) > 0) {
    availability_renewables = do.call(cbind, availability_list) %>% setNames(clusters_enr)
  } else {
    availability_renewables = NULL
  }
  links = data_antares$links %<>% select(c('link', 'timeId' ,'FLOW LIN.'))
  
  LOLD = data_antares$areas %>% select('LOLD')
  
  # Compute the certified capacity for thermal groups not supported and dsrs
  capacity_certified_thermal = compute_availability_thermal(load = load, 
                                                            clusters_thermal = clusters_thermal,
                                                            power_plants = power_plants, 
                                                            availability_thermal = availability_thermal)
  
  # Compute the availability of the renewables 
  capacity_certified_enr = compute_availability_enr(area = area, 
                                                    load = load, 
                                                    clusters_enr = clusters_enr,
                                                    availability_renewables = availability_renewables)
  
  # Compute the availability of the interconnections 
  capacity_certified_interconnections = compute_availability_interconnections(area = area, 
                                                                              LOLD = LOLD, 
                                                                              links = links)
  capacity_certified = c(capacity_certified_thermal, 
                         capacity_certified_enr,
                         capacity_certified_interconnections) 
  
  return(capacity_certified)
}

#' Compute availability on PP1 days of not supported clusters
#'
#' @param load (scalar) demand (hourly level)
#' @param clusters_thermal (str vector) thermal clusters names candidates to revenues analysis and investments
#' @param availability_thermal (data.table) availability data for thermal clusters from readAntares
#' @param power_plants (dataframe) with costs data for clusters
#'
#' @importFrom reshape2 dcast
#' @importFrom magrittr %>% %<>% multiply_by
#' @import data.table
#' @importFrom dplyr select
#' 
#' @return certified capacity by cluster
#' @export
compute_availability_thermal <- function(load, 
                                         clusters_thermal, 
                                         power_plants, 
                                         availability_thermal){
  
  if (is.null(availability_thermal)) return(rep(0, length(clusters_thermal)))
  
  cast_cluster = function(c){
    
    if (!grepl('dsr',power_plants[c, 'group'])) {
      oa_share = power_plants[c,"oa_share"]
      availability_cluster = availability_thermal[, c] %>% multiply_by(1-oa_share) %>% as.data.table(.) %>% setNames(.,c)      
      return(availability_cluster)
    }
  }
  availability_thermal = do.call(cbind, lapply(clusters_thermal, cast_cluster))
  
  #Aggregating all availabilities by clusters
  availability =  cbind(load, availability_thermal)
  
  capacity_certified = matrix(nrow=nrow(availability), ncol=ncol(availability))
  
  #Moving average on days
  for(i in 1:364){
    capacity_certified[i,] = as.vector(apply(as.matrix(availability[(24*(i-1)+1):(24*i),]),2,mean))
  }
  
  ##Selecting the days with highest loads as PP1
  capacity_certified  %<>% data.frame(.) %>% setNames(.,colnames(availability)) %>% arrange(desc(load)) %>%
    head(CRM[["fr"]]$nbr_PP1) %>% colMeans(.) %>% .[!grepl('load', names(.))]
  
  return(capacity_certified)
}


#' Compute certified capacity for renewables energy sources
#'
#' @param area (str) country code
#' @param study_id (str) id of the antares simulation
#' @param clusters_enr (str vector) RES clusters names candidates to revenues analysis and investments
#' @param load demand (hourly level)
#'
#' @return certified capacity for ENR (wind and solar) on PP1 days
#' @export
compute_availability_enr <- function(area,
                                     load, 
                                     clusters_enr,
                                     availability_renewables){
  
  if (is.null(availability_renewables)) return(rep(0, length(clusters_enr)))
  
  #Loading data
  availability_enr = cbind(load, availability_renewables)
  
  #Mean on days
  capacity_certified_enr = matrix(nrow=nrow(availability_enr), ncol=ncol(availability_enr))
  
  for(i in 1:364){
    capacity_certified_enr[i,] = as.vector(apply(as.matrix(availability_enr[(24*(i-1)+1):(24*i),]),2,mean))
  }
  
  ##Selecting the days with highest loads as PP1
  capacity_certified_enr %<>% data.frame(.) %>% setNames(colnames(availability_enr)) %>%
    arrange(desc(load)) %>% head(CRM[["fr"]]$nbr_PP1) %>% colMeans(.) %>%  .[!grepl('load', names(.))] 
  
  return(capacity_certified_enr)  
}

#' Compute the availability of the interconnections
#'
#' @param area (str) country code
#' @param LOLD (antares datatable) Loss of load duration
#' @param links (antares datatable) links flow for neighbours
#' 
#' @importFrom dplyr %>% filter 
#' @importFrom magrittr %<>%
#' 
#' @return capacity_certified for every interconections of a country
#' @export
compute_availability_interconnections <- function(area, 
                                                  links,
                                                  LOLD){
  
  certified_capacity_link = list()
  
  for (itc in links$link %>% unique(.)){
    df = cbind(links %>% filter(link == itc), LOLD) %>% filter(LOLD > 0)
    if (nrow(df) == 0){
      certified_capacity_link[[itc]] = 0
    }
    else{
      certified_capacity_link[[itc]] = mean(df$`FLOW LIN.`)
    }
  }  
  
  certified_capacity_link %<>% unlist(.) %>% abs(.) %>%
    sum(.) %>% setNames('interconnections')
  
  return(certified_capacity_link)                                    
}


