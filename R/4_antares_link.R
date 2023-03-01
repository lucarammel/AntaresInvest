#' Get the links of the neighbours area of the area considered
#'
#' @param area (str) country code
#' @param study_id (str) study id of the simulation antares (if API mode)
#' @importFrom antaresRead setSimulationPathAPI setSimulationPath
#' 
#' @return links_neighbours (str vector)
#' @export
get_neighbours_links <- function(area,  
                                 study_id){

  if (use_api){
    setSimulationPathAPI(host = host, 
                         token = token, 
                         simulation = 'input',
                         study_id = study_id)
  }
  else{                                
    setSimulationPath(paste(antares_simu_dir, study_id, sep = '/'), 'input')
  }
  links = getLinks(area)
  
  areas_candidates = all_areas[all_areas != area]
  neighbours = c()
  
  # Get neighbours only
  for (area in areas_candidates){
    mask = sapply(links, function(link) grepl(glue('\\<{area}\\>'), link)) %>% as.vector(.)
    if (sum(mask) >= 1) neighbours = c(neighbours,area)
  }
  
  # Get the links 
  links_neighbours = c()
  for (neighbour in neighbours){
    for (link in links){
      if (grepl(glue('\\<{neighbour}\\>'), link)) links_neighbours = c(links_neighbours, link)
    }
  }

  return(links_neighbours)
}


