#' Make the graph of the spot evolution 
#'
#' @param spot dataframe of spot prices
#' @param max_loop number maximum of really run loops over years
#'
#' @importFrom glue glue
#' @importFrom dplyr %>% filter
#' @import ggplot2
#' 
#' @return None
#' @export 
build_spot_chart <- function(spot, 
                             max_loop){
  print(glue('\n => Making spot graphs'))
  spot %<>% mutate(year = as.factor(year), loop = as.factor(loop))
  spot$date = glue('{spot$year}-{spot$loop}')  %>% factor(., levels = unique(.))
  
  #Background rectangles coordinates
  years = spot$year %>% unique(.) %>% as.vector(.) %>% as.numeric(.)
  mask = !as.numeric(years) %% 2 
  xmin = max_loop * (mask * 1: (years %>% length(.))) - max_loop + 1
  xmax = xmin + (max_loop * mask)
  xmin = xmin[xmin >= 0]
  xmax = xmax[xmax >= 0]
  
  fig = spot %>% ggplot(data = ., aes(x = date, y = spot, group = area)) +
    geom_line(aes(color = area), size = 1.5) +
    annotate('rect',xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
             fill = grey_grid, alpha = 0.3)+
    labs(title = glue('Spot prices'), 
         subtitle = 'For every country in Europe & on the time horizon',
         x = 'Iterations', 
         y = 'Spot prices (euros/MWh)') + theme_light() +
    scale_colour_manual(labels =  toupper(names(color_palette_countries[areas]))
                        ,values = unlist(color_palette_countries[areas]), name = 'Country') +
    theme(plot.title = element_text(face = 'bold'),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(face = 'italic'),
          legend.title = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          plot.subtitle = element_text(face = 'italic'),
          axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
          panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
          legend.background = element_rect(fill = grey_grid))
  
  ggsave(paste(study_dir,'graphes', 'spot_prices.png', sep = '/'), width = 10, height = 10, dpi = 300)
}

#' Make the graph of revenues evolution by cluster 
#'
#' @param graph_dir graphic directory
#' @param revenues revenues list of dataframe
#' @param zone country code
#' 
#' @importFrom dplyr %>% filter group_by
#' @importFrom glue glue
#' @import ggplot2
#' 
#' @return None
#' @export 
build_revenues_chart <- function(graph_dir, 
                                 revenues, 
                                 zone){
  print(glue('\n => Making revenues graphs for {toupper(zone)}'))
  revenues_dir = paste(graph_dir, 'revenues', sep = '/')
  dir.create(revenues_dir)
  
  df_revenues = revenues
  df_revenues %<>% mutate(year = as.factor(year), loop = as.factor(loop))
  
  for (y in df_revenues$year %>% unique(.)){
    for (s in df_revenues$loop %>% unique(.)){
      revenues_empty = df_revenues %>% group_by(type_revenues) %>% summarise(revenues = sum(revenues)) %>% 
        filter(revenues == 0) %>% .$type_revenues %>% as.vector(.) %>% unique(.)
      #Filter and reshape revenues
      revenues = df_revenues %>% filter(area == zone, year == y, loop == s, !grepl('ve', cluster_name), !type_revenues %in% c('rendered_revenues_MW',revenues_empty)) %>% 
        group_by(cluster_name,type_revenues) %>% summarise(., revenues = sum(revenues)) %>% mutate(revenues = revenues /1000)
      
      #Revenue absolute value
      fig = revenues %>% ggplot(data = ., aes(x = cluster_name, y = revenues,  fill = type_revenues, label = format(revenues,
                                                                                                                    scientific = TRUE, 
                                                                                                                    digits = 2))) + 
        geom_bar(stat = 'identity', position = 'stack', colour = 'black') +
        labs(subtitle = glue(' {toupper(zone)} - {y} - {s} : Revenues '), 
             x = 'Capacity types', 
             y = 'Revenues (keuros/MW)',
             title = 'Revenues by capacity and sources types',
             caption = 'AntaresInvest - Rte ') + theme_light() +
        scale_fill_manual(values = as.vector(unlist(color_palette_revenues)), name = 'Revenue type', 
                          label = names(color_palette_revenues)) +
        geom_text(position = position_stack(vjust = 0.5), size = 3.5, colour = 'black') + 
        theme(plot.title = element_text(face = 'bold'),
              axis.title.x = element_text(face = 'bold'), 
              axis.title.y = element_text(face = 'bold'),
              axis.line = element_line(colour = "black"),
              legend.text = element_text(face = 'italic'),
              legend.title = element_text(face = 'bold'),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.subtitle = element_text(face = 'italic'),
              panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
              panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
              axis.text.x = element_text(face = 'bold',angle = 60, vjust = 1, hjust = 1),
              legend.background = element_rect(fill = grey_grid)) 
      
      ggsave(paste(revenues_dir, as.character(glue('revenues_absolute_{zone}_{y}_{s}.png')), sep = '/'), width = 10, height = 10, dpi = 300)
      
      #Revenues shares
      fig = revenues %>% ggplot(data = ., aes(x = cluster_name, y = revenues,  fill = type_revenues)) + 
        geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
        labs(subtitle = glue(' {toupper(zone)} - {y} - {s} : Revenues share'), 
             x = 'Capacity types', 
             title = 'Share part of sources in revenues for capacities',
             y = 'Revenues (%)',
             caption = 'AntaresInvest - Rte') + theme_light() +
        scale_fill_manual(values = as.vector(unlist(color_palette_revenues)), name = 'Revenue type', 
                          label = names(color_palette_revenues)) +
        theme(plot.title = element_text(face = 'bold'),
              axis.title.x = element_text(face = 'bold'), 
              axis.title.y = element_text(face = 'bold'),
              axis.line = element_line(colour = "black"),
              legend.text = element_text(face = 'italic'),
              legend.title = element_text(face = 'bold'),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.subtitle = element_text(face = 'italic'),
              axis.text.x = element_text(face = 'bold'),
              panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
              panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
              legend.background = element_rect(fill = grey_grid)) 
      
      ggsave(paste(revenues_dir, as.character(glue('revenues_share_{zone}_{y}_{s}.png')), sep = '/'), width = 10, height = 10, dpi = 300)
    }
  } 
}


#' Build capacity shares 
#'
#' @param power_plants power plants dataframe
#'
#' @importFrom dplyr %>% filter summarise
#' @importFrom glue glue
#' @import ggplot2
#'  
#' @return None
#' @export
build_capacity_shares_chart <-function(power_plants) {
  print(glue('\n => Making capacity shares graphs'))
  powerfleet_dir = paste(study_dir, 'graphes','capacity_shares' ,sep ='/') 
  dir.create(powerfleet_dir)
  
  df_power_plants = power_plants 
  df_power_plants %<>% mutate(year = as.factor(year), loop = as.factor(loop))
  
  for (y in df_power_plants$year %>% unique(.)){
    for (s in df_power_plants$loop %>% unique(.)){
      
      power_plants = df_power_plants %>% filter(year == y, loop == s, variable == 'total_capacity', !grepl('ve', cluster_name))
      power_plants = power_plants %>% group_by(group, year, area, loop) %>% summarise(., capacity = sum(value)) %>% mutate(capacity = capacity /1000)
      
      groups_unique = unique(power_plants$group)[order(power_plants$group %>% unique(.))]
      new_legend = color_palette_power_groups[groups_unique]
      
      #Capacity shares
      fig = power_plants %>% ggplot(data = ., aes(x = area, y = capacity, fill = group, label = capacity)) +
        geom_bar(stat = 'identity', position =  'fill', colour = 'black') + 
        labs(subtitle = glue('{y} - {s} : Capacity share'), 
             title = 'Share part of capacity type in the powerfleet country',
             x = 'Country', 
             y = 'Capacity share (%)',
             caption = 'AntaresInvest - Rte ') + theme_light() +
        scale_fill_manual(labels = names(new_legend),
                          values = as.vector(unlist(new_legend)), name = 'Capacity type') + #Change the colors
        theme(plot.title = element_text(face = 'bold'),
              axis.title.x = element_text(face = 'bold'),
              axis.title.y = element_text(face = 'bold'),
              axis.line = element_line(colour = "black"),
              legend.text = element_text(face = 'italic'),
              legend.title = element_text(face = 'bold'),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.subtitle = element_text(face = 'italic'),
              axis.text.x = element_text(face = 'bold'),
              panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
              panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
              legend.background = element_rect(fill = grey_grid))
      
      ggsave(paste(powerfleet_dir, as.character(glue('power_fleet_shares_{y}_{s}.png')), sep = '/'), width = 10, height = 10, dpi = 300)
    }
  }
}

#' Make the graph of the powerfleet evolution by cluster
#'
#' @param graph_dir graphic directory
#' @param power_plants power fleet list of dataframe 
#' @param zone country code
#' 
#' @importFrom dplyr %>% filter 
#' @importFrom glue glue
#' @import ggplot2
#'
#' @return None
#' @export
build_powerfleet_chart <- function(graph_dir, 
                                   power_plants, 
                                   zone){
  print(glue('\n => Making powerfleet graphs for {toupper(zone)}'))
  powerfleet_dir = paste(graph_dir, 'powerfleet', sep ='/') 
  dir.create(powerfleet_dir)
  
  df_power_plants = power_plants
  df_power_plants %<>% mutate(year = as.factor(year), loop = as.factor(loop))
  
  for (y in df_power_plants$year %>% unique(.)){
    for (s in df_power_plants$loop %>% unique(.)){
      
      power_plants = df_power_plants %>% filter(area == zone, variable == 'total_capacity', group != 've', year == y, loop == s) %>% 
        group_by(group) %>% summarise(., capacity = sum(value)) %>% mutate(capacity = capacity/1000)
      
      groups_unique = unique(power_plants$group)[order(power_plants$group %>% unique(.))]
      new_legend = color_palette_power_groups[groups_unique]
      
      #Capacity absolute values
      fig = power_plants %>% ggplot(data = ., aes(x = group, y = capacity, fill = group, label = capacity)) +
        geom_bar(stat = 'identity', position =  'dodge', colour = 'black') +
        labs(subtitle = glue('{toupper(zone)} - {y} - {s}  : Capacity'), 
             title = 'Powerfleet by capacity type',
             x = 'Capacity type', 
             y = 'Capacity (GW)',
             caption = 'AntaresInvest - Rte') + theme_light() +
        scale_fill_manual(labels = names(new_legend),
                          values = as.vector(unlist(new_legend)), name = 'Capacity type') + #Change the colors
        geom_text(color = 'black', position = position_dodge(0.9), vjust = -0.25 ,size = 3.5) +
        theme(plot.title = element_text(face = 'bold'),
              axis.title.x = element_text(face = 'bold'), 
              axis.title.y = element_text(face = 'bold'),
              axis.line = element_line(colour = "black"),
              legend.text = element_text(face = 'italic'),
              legend.title = element_text(face = 'bold'),
              plot.subtitle = element_text(face = 'italic'),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(), 
              legend.position = 'none',
              axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
              panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
              panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
              legend.background = element_rect(fill = grey_grid))
      
      ggsave(paste(powerfleet_dir, as.character(glue('power_fleet_absolute_{zone}_{y}_{s}.png')), sep = '/'), width = 10, height = 10, dpi = 300)
    }
  } 
}

#' Make the graph of the Loss of Load duration evolution 
#'
#' @param LOLD loss of load duration dataframe
#' @param max_loop number maximum of really run loops over years
#' 
#' @importFrom dplyr %>% filter 
#' @importFrom glue glue
#' @import ggplot2
#' 
#' @return None
#' @export
build_lold_chart <- function(LOLD, 
                             max_loop){
  print(glue('\n => Making LOLD graphs'))
  LOLD %<>% mutate(year = as.factor(year), loop = as.factor(loop))
  LOLD$date = glue('{LOLD$year}-{LOLD$loop}') %>% factor(., levels = unique(.))
  
  #Background rectangles coordinates
  years = LOLD$year %>% unique(.) %>% as.vector(.) %>% as.numeric(.)
  mask = !as.numeric(years) %% 2
  xmin = max_loop * (mask * 1: (years %>% length(.))) - max_loop + 1
  xmax = xmin + (max_loop * mask)
  xmin = xmin[xmin >= 0]
  xmax = xmax[xmax >= 0]
  
  fig = LOLD %>% ggplot(data = ., aes(x = date, y = LOLD, group = area)) +
    geom_line(aes(color = area), size = 1.5)  +
    annotate('rect',xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
             fill = grey_grid, alpha = 0.3) +
    labs(title = glue('Loss of Load Duration'),
         subtitle = 'For every country in Europe & on the time horizon',
         x = 'Iterations', 
         y = 'LOLD (hour)',
         caption = 'AntaresInvest - Rte') + theme_light() +
    scale_colour_manual(labels = toupper(names(color_palette_countries[areas]))
                        ,values = unlist(color_palette_countries[areas]), name = 'Country') +
    theme(plot.title = element_text(face = 'bold'),
          axis.title.x = element_text(face = 'bold'), 
          axis.title.y = element_text(face = 'bold'),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(face = 'italic'),
          legend.title = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.subtitle = element_text(face = 'italic'),
          axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
          panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
          panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
          legend.background = element_rect(fill = grey_grid))
  
  ggsave(paste(study_dir,'graphes', 'LOLD.png', sep = '/'), width = 10, height = 10, dpi = 300)
}

#' Build production graphs
#'
#' @param graph_dir graphic directory
#' @param production production dataframe
#' @param zone country code
#'
#' @importFrom dplyr %>% filter 
#' @import ggplot2
#' @importFrom glue glue
#' 
#' @return None
#' @export
build_production_chart<- function(graph_dir, 
                                  production, 
                                  zone){
  print(glue('\n => Making production graphs for {toupper(zone)}'))
  production_dir = paste(graph_dir, 'production', sep = '/') 
  dir.create(production_dir)
  df_production  = production %>% mutate(production = production / 1000)
  
  for (y in df_production$year %>% unique(.)){
    for (s in df_production$loop %>% unique(.)){
      
      production = df_production %>% filter(area == zone, year == y, loop  == s) #%>%  group_by(cluster_name,group) %>% summarise(., production = sum(production))
      
      groups_unique = unique(production$group)[order(production$group %>% unique(.))]
      new_legend = color_palette_power_groups[groups_unique]
      
      fig = production %>% ggplot(data = ., aes(x = cluster_name, y = production, fill = group, label = format(production, 
                                                                                                               scientific = TRUE, 
                                                                                                               digits = 2))) +
        geom_bar(stat = 'identity', position = position_dodge(width = 1), colour = 'black') +
        labs(subtitle = glue('{toupper(zone)} - {y} - {s} : Production'),
             title = 'Power production by cluster type',
             x = 'Clusters', 
             y = 'Production (GWh)',
             caption = 'AntaresInvest - Rte') + theme_light() +
        scale_fill_manual(labels = names(new_legend), 
                          values = unlist(new_legend), name = 'Capacity type') +#Change the colors
        geom_text(color = 'black', position = position_stack(), vjust = -0.25 ,size = 3.5) +
        theme(plot.title = element_text(face = 'bold'),
              axis.title.x = element_text(face = 'bold'), 
              axis.title.y = element_text(face = 'bold'),
              axis.line = element_line(colour = "black"),
              legend.text = element_text(face = 'italic'),
              legend.title = element_text(face = 'bold'),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.subtitle = element_text(face = 'italic'),
              axis.text.x = element_text(face = 'bold', angle = 60, vjust = 0.5, hjust=1),
              panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
              panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
              legend.background = element_rect(fill = grey_grid))
      
      ggsave(paste(production_dir, as.character(glue('production_{zone}_{y}_{s}.png')), sep = '/'), width = 10, height = 10, dpi = 300)
    }
  }
}

#' Build power_plants moves graphs
#'
#' @param power_plants_moves List of investments & decommissioning decisions
#'
#' @importFrom dplyr %>% filter 
#' @importFrom glue glue
#' @import ggplot2
#' 
#' @return power_plants_moves reshaped
#' @export
build_power_plants_moves_chart <- function(power_plants_moves){
  print(glue('\n => Making power_plants moves graphs'))
  power_plants_moves %<>% mutate(capacity = capacity / 1000) 
  areas_ordered = power_plants_moves[order(power_plants_moves$area),]$area %>% unique(.) %>% as.vector(.) #To get the legend
  
  fig = power_plants_moves %>% ggplot(data = ., aes(x = year_start, y = capacity,  fill = area, label = format(capacity, 
                                                                                                               digits = 2))) + 
    geom_bar(stat = 'identity', position = position_dodge(width = 1), colour = 'black') + 
    labs(title = 'Investment & retirement decisions',
         subtitle = 'For every country in Europe & on the time horizon',
         x = 'Year', 
         y = 'Capacity moved (GW)', 
         caption = 'AntaresInvest - Rte ') + theme_light() +
    scale_fill_manual(labels = toupper(names(color_palette_countries[areas_ordered])),
                      values = as.vector(unlist(color_palette_countries[areas_ordered])), name = 'Country') + 
    theme(plot.title = element_text(face = 'bold'),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(face = 'italic'),
          legend.title = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          plot.subtitle = element_text(face = 'italic'),
          panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
          panel.grid.major.y = element_line(size = 0.8, color = grey_grid), 
          legend.background = element_rect(fill = grey_grid))
  
  ggsave(paste(study_dir,'graphes', 'power_plants_moves.png', sep = '/'), width = 10, height = 10, dpi = 300)
}

#' Build power plants graphs aggregated at european scale / country scale 
#'
#' @param power_plants power_plants dataframe
#' @param max_loop number maximum of really run loops over years
#'
#' @return None
#' @export
build_powerfleet_europe_chart <- function(power_plants, 
                                          max_loop){
  print(glue('\n => Making powerfleet on Europe by capacity types graphs'))
  df_power_plants = power_plants 
  
  df_power_plants %<>% mutate(year = as.factor(year), loop = as.factor(loop))
  df_power_plants$date = glue('{power_plants$year}-{power_plants$loop}')  %>% factor(., levels = unique(.))
  
  power_plants = df_power_plants %>% filter(variable == 'total_capacity', group != 've') %>% group_by(group, date) %>% summarise(., capacity = sum(value)) %>%
    mutate(capacity = capacity /1000)
  
  groups_unique = unique(power_plants$group)[order(power_plants$group %>% unique(.))]
  new_legend = color_palette_power_groups[groups_unique]
  
  #Background rectangles coordinates
  years = df_power_plants$year %>% unique(.) %>% as.vector(.) %>% as.numeric(.)
  mask = !as.numeric(years) %% 2
  xmin = max_loop * (mask * 1: (years %>% length(.))) - max_loop + 1
  xmax = xmin + (max_loop * mask)
  xmin = xmin[xmin >= 0]
  xmax = xmax[xmax >= 0]
  
  fig = power_plants %>% ggplot(data = ., aes(x = date, y = capacity, group = group)) +
    geom_line(aes(color = group), size = 1.5) +
    annotate('rect',xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
             fill = grey_grid, alpha = 0.3)+
    labs(title = glue('Powerfleet evolution by capacity type'), 
         subtitle = 'For Europe & on the time horizon splitted by iteration',
         x = 'Iterations', 
         y = 'Capacity (GW)') + theme_light() +
    scale_colour_manual(labels = names(new_legend),
                        values = unlist(new_legend), name = 'Capacity type') +
    theme(plot.title = element_text(face = 'bold'),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(face = 'italic'),
          legend.title = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          plot.subtitle = element_text(face = 'italic'),
          axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
          panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
          legend.background = element_rect(fill = grey_grid))
  
  ggsave(paste(study_dir,'graphes', 'power_plants_capacity_technologie_europe.png', sep = '/'), width = 10, height = 10, dpi = 300)
  
  print(glue('\n => Making powerfleet total capacity by country graphs'))
  power_plants = df_power_plants %>% filter(variable == 'total_capacity', group != 've') %>% group_by(area, date) %>% summarise(., capacity = sum(value)) %>%
    mutate(capacity = capacity / 1000)
  
  fig = power_plants %>% ggplot(data = ., aes(x = date, y = capacity, group = area)) +
    geom_line(aes(color = area), size = 1.5) +
    annotate('rect',xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
             fill = grey_grid, alpha = 0.3)+
    labs(title = glue('Powerfleet evolution by country'), 
         subtitle = 'For every country & on the time horizon splitted by iteration',
         x = 'Iterations', 
         y = 'Capacity (GW)') + theme_light() +
    scale_colour_manual(labels =  toupper(names(color_palette_countries[areas]))
                        ,values = unlist(color_palette_countries[areas]), name = 'Country') +
    theme(plot.title = element_text(face = 'bold'),
          axis.title.x = element_text(face = 'bold'),
          axis.title.y = element_text(face = 'bold'),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(face = 'italic'),
          legend.title = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          plot.subtitle = element_text(face = 'italic'),
          axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 1),
          panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
          panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
          legend.background = element_rect(fill = grey_grid))
  
  ggsave(paste(study_dir,'graphes', 'power_plants_capacity_country.png', sep = '/'), width = 10, height = 10, dpi = 300)
}


#' Build CRM curves (demand & supply)
#'
#' @param graph_dir (str) graphic directory
#' @param crm_supply_curve (data.frame) supply_curve (bid, cumulated certified capacity)
#' @param crm_demand (float) demand in MW
#' @param year_simulated (int) year simulation
#' @param auction_info (list) information of the auction
#' @param s s
#' @param ro_strike ro strike dataframe
#' @param zone zone 
#' 
#' @import ggplot2
#' @importFrom dplyr %>% filter select
#'
#' @return None
#' @export
build_crm_curves_chart <- function(graph_dir,
                                   crm_supply,
                                   crm_demand,
                                   auction_info,
                                   ro_strike,
                                   zone){
  print(glue('\n => Making CRM curves graphs for {toupper(zone)}'))
  crm_dir = paste(graph_dir, 'crm_curves',sep ='/') 
  dir.create(crm_dir)
  
  for (y in crm_supply$year %>% unique(.)){
    for (s in crm_supply$loop %>% unique(.)){
      crm_supply_curve = crm_supply %>% filter(area == zone, year == y, loop  == s)
      crm_demand_curve = crm_demand %>% filter(area == zone, year == y, loop  == s)
      auction = auction_info %>% filter(area == zone, year == y, loop  == s)
      
      # RO Option customization
      if (reliability_options$enabled & !reliability_options$by_capacity){
        ro_strike_price = ro_strike %>% filter(area == zone,year == y,loop == s) %>% .$ro_strike_price %>% as.numeric(.)
        title = glue::glue('CRM - {y} - {s} with Reliability Options')
        if (reliability_options$dynamic_strike_price$enabled){
          subtitle = glue::glue('CRM curves for [{toupper(zone)}] - RO strike price = {ro_strike_price}euro/MWh (dynamic strike price)')
          filename_appendix = glue::glue("{demand$type}_RO_and_dynamic_strike_price")
        } else {
          subtitle = glue::glue('CRM curves for [{toupper(zone)}] - RO strike price = {ro_strike_price}euro/MWh (fixed strike price)')
          filename_appendix = glue::glue("{demand$type}_RO_and_fixed_strike_price")
        }
      }
      else if (reliability_options$enabled & reliability_options$by_capacity) {
        title = glue::glue('CRM - {y} - {s} with Reliability Options')
        subtitle = subtitle = glue::glue('CRM curves for [{toupper(zone)}] - RO strike price by capacity')
        filename_appendix = glue::glue("{demand$type}_RO_strike_price_by_capacity")
      }
      else {
        title = glue::glue('CRM - {y} - {s}')
        subtitle = glue::glue('CRM curves for [{toupper(zone)}]')
        filename_appendix = glue::glue("{demand$type}")
      }
      
      # Demand type option for plotting graphs
      if (demand$type == 'inelastic'){
        fig_demand = geom_step(data = crm_demand_curve,aes(x = capacity_cumulative,
                                                           y = price,
                                                           color = "Demand curve"),size=1)
      }
      else {
        fig_demand  = geom_line(data = crm_demand_curve,aes(x = capacity_cumulative,
                                                            y = price,
                                                            color = "Demand curve"),size=1)
      }
      
      fig = crm_supply_curve %>% ggplot(data = ., aes(x = capacity_cumulative, y = bid)) +
        geom_step(size=1,aes(color = "Supply curve"),direction="vh") +
        fig_demand +
        annotate(geom = "text",
                 x = auction$capacity_retained*0.9,
                 y = auction$clearing_price*1.5,
                 fontface ="bold",
                 label = as.character(glue('Marginal bidder : {auction$marginal_bidder}\nClearing price : {auction$clearing_price}')),
                 size = 4.5) +
        scale_color_manual("CRM - Curves", breaks = c("Demand curve","Supply curve"), values = c("red","blue")) +
        labs(title = title,
             subtitle = subtitle,
             x = 'Certified capacity (MW)',
             y = 'Price (euro/MW)',
             caption = 'AntaresInvest - Rte ')+
        theme(plot.title = element_text(face = 'bold'),
              axis.title.x = element_text(face = 'bold'),
              axis.title.y = element_text(face = 'bold'),
              axis.line = element_line(colour = "black"),
              legend.text = element_text(face = 'italic'),
              legend.title = element_text(face = 'bold'),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(), 
              plot.subtitle = element_text(face = 'italic'),
              panel.grid.major.x = element_line(size = 0.8, color = grey_grid),
              panel.grid.major.y = element_line(size = 0.8, color = grey_grid),
              legend.background = element_rect(fill = grey_grid))
      
      ggsave(paste(crm_dir,glue::glue('CRM_{zone}_{y}_{s}_{filename_appendix}.png'), sep = '/'), width = 10, height = 10, dpi = 300) 
    }
  }
}

