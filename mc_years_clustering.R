#---------------------------------------------------- Load packages & antares invest --------------------------------------------------#
rm(list = ls())

library(tidyr)
library(magrittr)

project_dir = rstudioapi::getActiveDocumentContext()$path %>% strsplit(., split = "/", fixed = T)  %>% .[[1]] %>% .[-c(length(.),length(.)-1)] %>%
  paste(.,collapse="/")

antaresinvest_directory = paste(project_dir, 'antaresinvest', sep = '/')
devtools::load_all(antaresinvest_directory)

build_parameters_input(update_yaml = TRUE)

#---------------------------------------------------- Prepare study list to refer to the antares simulation --------------------------------------------------#

initial_mc_list = mc_list ### The initial monte carlo years that were selected by DSVD

years_to_simulate = antares_study_list %>% as.integer(.)
s = 1
mc_list = 1:999 ### all monte carlos years

study_list = list('2031' = list(reference_id = 'ed5427e0-2d7e-460b-a45b-e4359bd0b75b',
                                variant_id = '36bd3a4e-0a55-4fc1-8a48-37b6a33116f9'),
                  '2041' = list(reference_id = '4565e40d-c8b3-4fcf-a28e-2bd0c3cd1d85',
                                variant_id = 'f719d549-2a9a-4bb4-b72c-327649d2a566'))

outputs = list('2031' = '20230227-1001eco-1-2031',
               '2041' = '20230227-1002eco-1-2031')

#----------------------------------- Load antares data & compute revenues with Antares Invest   ---------------------------------------------#
### Build the dataframe containing initial powerplants fleet from antares study
power_plants = lapply(antares_study_list, function(simulation_name) lapply(areas,function(area) make_powerplants_from_antares(study_id = study_list[[simulation_name]]$variant_id,
                                                                                                                                zone = area, 
                                                                                                                                year_simulated = simulation_name))) %>% rename_dataset(., years_to_simulate = antares_study_list, 
                                                                                                                                                                                       areas = areas)

dataset = build_dataset_wrapper(power_plants = power_plants,
                                study_list = study_list,
                                years_to_simulate = years_to_simulate,
                                outputs = outputs)

###Analysis of capacity and energy only markets : loop on years and areas
dataset_revenues = lapply(as.character(years_to_simulate), function(year_simulated) lapply(areas, function (area) analysis_revenues_total(year_simulated = year_simulated,
                                                                                                                                          area = area,
                                                                                                                                          power_plants =  power_plants[[year_simulated]][[area]],
                                                                                                                                          clusters_not_supported = dataset[[year_simulated]][[area]]$clusters_filters$not_supported,
                                                                                                                                          data_antares = dataset[[year_simulated]][[area]]$data_antares))) %>% rename_dataset(., years_to_simulate = years_to_simulate, 
                                                                                                                                                                                                                              areas = areas)

#---------------------------------------------------- LOAD  --------------------------------------------------#
load = lapply(names(dataset),
              function(x) dataset[[x]]$fr$data_antares$areas %>% select(c("mcYear","timeId","LOAD")) %>% tidyr::pivot_wider(names_from = mcYear,values_from = LOAD)) %>% setNames(.,names(dataset))

### Check if years are the same across
#---------------------------------------------------- Extract revenues & unsupplied energy  --------------------------------------------------#
years = as.character(years_to_simulate)
base_revenu = lapply(years,
                     function(year) dataset_revenues[[year]]$fr$revenues %>% names(.) %>% 
                       sapply(.,function(c) return(dataset_revenues[[year]]$fr$revenues[[c]]$energy_revenues_annual_MW))) %>% setNames(.,years)

revenu_median_simu = base_revenu %>% lapply(.,apply,1,median)
revenu_moyen_simu  = base_revenu %>% lapply(.,rowMeans)
sd_revenu = base_revenu %>% lapply(.,apply,1,sd) 

### Extract unsupplied energy ###
table_delestage = lapply(years,
                         function (year) dataset[[year]]$fr$data_antares$areas %>% 
                           dcast(., mcYear+timeId~area, value.var = "UNSP. ENRG") %>% 
                           dcast(., timeId~mcYear, value.var = "fr") %>% 
                           select(-c(timeId)) %>% as.matrix(.)) %>% setNames(.,years)

defaillance_heure_simu = table_delestage %>% lapply(.,function(x) (x>0) %>% colSums(.) %>% as.vector(.))
defaillance_energie_simu = table_delestage %>% lapply(.,function(x) x %>% colSums(.) %>% as.vector(.))

#---------------------------------------------------- Extract price time series and build dataframe ----------------------------------------------#
base_prix =  lapply(years,
                    function(year) dataset[[year]]$fr$data_antares$areas %>% 
                      dcast(., mcYear+timeId~area, value.var = "MRG. PRICE") %>% 
                      dcast(., timeId~mcYear, value.var = "fr") %>%
                      select(-c(timeId)) %>% mutate_all(function(x) ifelse(x<0,0,x))) %>% setNames(.,years)

prix_moyen_simu  = base_prix %>% lapply(., function(x) x %>% colMeans(.) %>% as.vector(.))
prix_median_simu = base_prix %>% lapply(., function(x) x %>% apply(.,2,median) %>% as.vector(.))
sd_prix = base_prix %>% lapply(.,apply,2,sd)

dfs_outputs = lapply(years,
                     function(year) data.table(mcYear = 1:1000,
                                               defaillance = defaillance_heure_simu[[year]],
                                               defaillance_energie = defaillance_energie_simu[[year]],
                                               prix_moyen = prix_moyen_simu[[year]],
                                               prix_median = prix_median_simu[[year]],
                                               sd_prix = sd_prix[[year]],
                                               revenu_moyen = revenu_median_simu[[year]],
                                               revenu_median = revenu_moyen_simu[[year]],
                                               sd_revenu = sd_revenu[[year]])) %>%
  setNames(.,years) %>% purrr::list_modify(., all_years = bind_rows(.,.id="year") )

#################### Extract the table to use DSVD years selection method ##############################
data = lapply(years,
              function(year) data.table(mcYear = 1:1000,
                                        defaillance = defaillance_heure_simu[[year]],
                                        defaillance_energie = defaillance_energie_simu[[year]],
                                        prix_moyen = prix_moyen_simu[[year]],
                                        revenu_moyen = revenu_median_simu[[year]])) %>%
  setNames(.,years)

## Prepare data selection for the year selections code ##
df_2022 = data$`2022` %>% select(-c("mcYear"))
colnames(df_2022) = paste(colnames(df_2022),"2022",sep="_")
df_2026 = data$`2026` %>% select(-c("mcYear"))
colnames(df_2026) = paste(colnames(df_2026),"2026",sep="_")


df_selection = bind_cols(c(data$`2022` %>% select(c("mcYear")),df_2022,df_2026)) %>% as.data.table()
df_selection = readRDS("antares_invest_selection_data.rds")
df_selection
#---------------------------------------------------- Clustering utils --------------------------------------------------#
### Load utils ####
clust.medoid = function(i, distmat, clusters) {
  ind = (clusters == i)
  if(sum(ind)>1){ # If individual is not alone in its cluster
    names(which.min(rowSums(distmat[ind, ind])))
  } else{
    return(which(clusters==i))
  }}

#' Compute statistics regarding the clustering 
stat.comp <- function(x,y){ 
  K <- length(unique(y)) # Number of clusters
  n <- length(x) # Number of observations
  m <- mean(x) # Global average
  TSS <- sum((x-m)^2) # Total variance 
  nk <- table(y) # Conditional groups 
  mk <- tapply(x,y,mean) # moyennes conditionnelles    
  BSS <- sum(nk * (mk - m)^2) # Explained variance    
  result <- c(mk,100.0*BSS/TSS) # averages & proportion of explained variance
  names(result) <- c(paste("G",1:K),"% epl.")    # Rename vector's elements
  return(result) }

#---------------------------------------------------- Clustering --------------------------------------------------#
make_clustering <- function(df,use_median = FALSE, use_quantile = FALSE, use_sd = FALSE){
  cols = c("defaillance","defaillance_energie")
  ifelse(use_median, cols %<>% c(.,"prix_median","revenu_median"), cols %<>% c(.,"prix_moyen","revenu_moyen"))
  
  if(use_sd) cols %<>% c(.,'sd_prix','sd_revenu')
  df1 = df %>% select(all_of(cols))
  
  # center & scale data to avoid high variance variable to impact the result 
  df.cr <- scale(df1 ,center=T,scale=T)
  # distance matrix between individuals
  d.df <- dist(df.cr)
  # CAH - ward criteria #method =  ward.D2 correspond au vrai critere de Ward #utilisant le carre de la distance 
  cah.ward <- hclust(d.df,method="ward.D2")
  
  # plot(cah.ward) # display dendogram
  # rect.hclust(cah.ward,k=10) # display the clusters on dendogram  
  groupes.cah <- cutree(cah.ward,k=10) # split in 10 clusters to get 10 representative monte carlo years
  mat.dist = as.matrix(d.df) # get a full matrix
  
  stat = sapply(df1,stat.comp,y=groupes.cah) %>% data.frame(.) %>% mutate(group = row.names(.))
  row.names(stat) = NULL
  medoids = sapply(unique(groupes.cah), clust.medoid, mat.dist, groupes.cah) %>% as.vector(.) %>% as.integer(.)
  medoids_weights = groupes.cah %>% table(.) %>% as.vector() %>% divide_by(.,sum(.))
  df_cluster = df.cr %>% data.frame(.) %>% mutate(cluster = groupes.cah %>% as.factor(.), mcYear = df$mcYear)
  out = list(groupes = groupes.cah,
             mat.dist = mat.dist,
             medoids = df_cluster[medoids,"mcYear"],
             weights = medoids_weights,
             df_cluster = df_cluster,
             stat = stat)
}

cluster_graph  = function(df,medoids,year="2022", x = "defaillance", y = "revenu_moyen",xlab="Defaillance",ylab = "Revenu Moyen"){
  fig = ggplot() + 
    geom_point(data = df,aes(x = !!sym(x), y = !!sym(y), colour = cluster)) +
    geom_point(data = df[medoids,],aes(x = !!sym(x), y = !!sym(y)),shape ="x",color="black",size=8) +
    
    labs(x = xlab,
         y = ylab,
         title = glue::glue("{ylab} ~ {xlab} - [{year}]"),
         subtitle = glue::glue("Affichage avec indentification des clusters\nAnnees: [{medoids %>% sort(.) %>% paste(.,collapse = '-')}]"),
         caption = "AntaresInvest - Rte") +
    ylim(0,max(df[,y])*1.05)+
    
    
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
}

plot_initial_vs_new_cluster = function(df,initial,year="2022",medoids,x = "defaillance", y = "revenu_moyen",xlab="Defaillance",ylab = "Revenu Moyen"){
  df_temp = rbind(df[medoids,] %>% mutate(cluster_centers = "New cluster centers"), df[initial,] %>% mutate(cluster_centers = "Initial cluster centers"))
  fig = ggplot(data = df_temp,aes(x = !!sym(x), y = !!sym(y),colour = cluster_centers,fill=cluster_centers)) + 
    geom_point(shape = 23,size=8,alpha = 0.8) +
    labs(x = xlab,
         y = ylab,
         title = glue::glue("{ylab} ~ {xlab} - [{year}]"),
         subtitle = "Affichage avec indentification des clusters",
         caption = "AntaresInvest - Rte") +
    ylim(0,max(df[,y])*1.05)+
    
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
  return(fig)
}

#---------------------------------------------------- Clustering with mean values only--------------------------------------------------#
clust_mean_only = lapply(names(dfs_outputs),function(year) make_clustering(dfs_outputs[[year]])) %>% setNames(.,names(dfs_outputs))

fig_mean_only = clust_mean_only %>% {. ->> temp_list } %>% list(l = ., cols = names(.)) %$% ### %$% enable to pass two arguments within pipe (list & names(list))
  lapply(cols, function(y) cluster_graph(df = l[[y]]$df_cluster,
                                         medoids = l[[y]]$medoids,
                                         year = y)) %>% setNames(., names(temp_list)) ### We use the name of temp_list (our first list) to set the names of the new list

fig_init_mean_only = clust_mean_only %>% {. ->> temp_list } %>% list(l=. , cols = names(.)) %$% 
  lapply(cols,function (x) plot_initial_vs_new_cluster(df = l[[x]]$df_cluster,
                                                       initial = initial_mc_list,
                                                       medoids = l[[x]]$medoids,
                                                       year = x))  %>% setNames(.,names(temp_list))

#####################################################################################################################################
#---------------------------------------------------- Assess clustering quality ----------------------------------------------------#
#####################################################################################################################################
#---------------------------------------------------- 1. Revenues side ----------------------------------------------------#

## We keep only the clust_mean_only // We want to compare the revenues for all years and the one that is estimated with mc list & clustering
annual_revenues = lapply(years, ### Loop across all years 
                         #### Compute mean revenues on a set of MC years with weigths
                         function(y) list(all_years = list(centers = 1:1000, weights = 1/1000), ### All MC years
                                          mc_list = list(centers  = initial_mc_list, weights = 1/length(initial_mc_list)), ### The 10 initial MC years, equally weighted
                                          clustering_per_year = list(centers  = clust_mean_only[[y]]$medoids, weights = clust_mean_only[[y]]$weights), ### The years picked when doing the clustering for this specific year, weighted
                                          clustering_all_years= list(centers  = clust_mean_only$all_years$medoids, weights = clust_mean_only$all_years$weights)) %>% ### The years picked when doing the combined clustering (across all years) for this specific year, weighted
                           
                           lapply(.,function(l) base_revenu[[y]][l$centers, ] %>% multiply_by(., l$weights) %>% 
                                    apply(.,2,sum) %>% data.frame(.) %>% t(.) %>% set_rownames(.,"mean_annual_revenue"))) %>% setNames(.,years) %>%
  ### Build a list with for each year the mean revenue of each cluster for various MC configuration (ally years, clustering years only (weighted), initial_mc_list)
  
  melt(.) %>% setNames(.,c("row","cluster_name","mean_annual_revenue","mc_choice",'year')) %>% select(-c("row")) %>%  ### Aggregate the list within a dataframe to ease computation
  
  tidyr::pivot_wider(.,names_from = mc_choice, values_from = mean_annual_revenue) ### Pivot table => dataframe (year, cluster_name, all_years (revenues), mc_list (revenues) ....)

#### Now we can compute error metrics on the revenues side => both MAE (Mean Absolute Error) & MAPE (Mean Absolute Percentage Error)
#### We compute these metrics for each configuration (MAE/MAPE):
####    => When we use the clustering built for the given year
#####   => When we use the years extracted from the combined clustering
#####   => When we use the initial mc list built by DSVD

error_revenues_df = annual_revenues %>% 
  ### Add AE (absolute error) metric
  mutate(ae_clustering_per_year = abs(all_years - clustering_per_year),
         ae_clustering_all_years = abs(all_years - clustering_all_years),
         ae_mc_list = abs(all_years - mc_list)) %>%
  ### Add APE (absolute percentage error) metric (AE/reference value)
  mutate(ape_clustering_per_year = ae_clustering_per_year/all_years,
         ape_clustering_all_years = ae_clustering_all_years/all_years,
         ape_mc_list = ae_mc_list/all_years) %>%
  ### Groupby year to get the results
  group_by(year) %>% 
  ### Compute Mean Absolute Error & Mean Absolute Percentage Error
  summarise(MAE_clustering_per_year = sum(ae_clustering_per_year)/n(),
            MAE_mc_list = sum(ae_mc_list)/n(),
            MAE_clustering_all_years = sum(ae_clustering_all_years)/n(),
            MAPE_clustering_per_year = sum(ape_clustering_per_year)/n(),
            MAPE_clustering_all_years = sum(ape_clustering_all_years)/n(),
            MAPE_mc_list = sum(ape_mc_list)/n())

### We can see that : Initial mc list is not representative and a weighted clustering for each simulation is the right pick. 
### Besides, a combine clustering makes no sense, as for a given antares study some MC years are used
### to represent outliers for an another antares study. This would require to get the weight of each cluster for each antares study and we rather have a specific MC list rather than a list of weights
error_revenues_df ### See the error df

#---------------------------------------------------- 2. Unsupplied energy ----------------------------------------------------#
# We take the same approach on the unsupplied energy side ("Defaillance")
annual_unsp = lapply(years, ### Loop across all years 
                     #### Compute mean revenues on a set of MC years with weigths
                     function(y) list(unsp_all_years = list(centers = 1:1000, weights = 1/1000), ### All MC years
                                      unsp_mc_list = list(centers  = initial_mc_list, weights = 1/length(initial_mc_list)), ### The 10 initial MC years, equally weighted
                                      unsp_clustering_per_year = list(centers  = clust_mean_only[[y]]$medoids, weights = clust_mean_only[[y]]$weights), ### The years picked when doing the clustering for this specific year, weighted
                                      unsp_clustering_all_years= list(centers  = clust_mean_only$all_years$medoids, weights = clust_mean_only$all_years$weights)) %>% ### The years picked when doing the combined clustering (across all years) for this specific year, weighted
                       
                       lapply(.,function(l) defaillance_simu[[y]][l$centers] %>% multiply_by(., l$weights) %>% 
                                sum(.))) %>% setNames(.,years) %>% 
  melt(.) %>% setNames(.,c("annual_mean_unsp","mc_choice","year")) %>% 
  tidyr::pivot_wider(.,names_from= mc_choice, values_from = annual_mean_unsp)

error_usnp_df = annual_unsp %>% 
  ### Add AE (absolute error) metric
  mutate(ae_clustering_per_year = abs(unsp_all_years - unsp_clustering_per_year),
         ae_clustering_all_years = abs(unsp_all_years - unsp_clustering_all_years),
         ae_mc_list = abs(unsp_all_years - unsp_mc_list)) %>%
  ### Add APE (absolute percentage error) metric (AE/reference value)
  mutate(ape_clustering_per_year = ae_clustering_per_year/unsp_all_years,
         ape_clustering_all_years = ae_clustering_all_years/unsp_all_years,
         ape_mc_list = ae_mc_list/unsp_all_years) %>%
  ### Groupby year to get the results
  group_by(year) %>% 
  ### Compute Mean Absolute Error & Mean Absolute Percentage Error
  summarise(MAE_clustering_per_year = sum(ae_clustering_per_year)/n(),
            MAE_mc_list = sum(ae_mc_list)/n(),
            MAE_clustering_all_years = sum(ae_clustering_all_years)/n(),
            MAPE_clustering_per_year = sum(ape_clustering_per_year)/n(),
            MAPE_clustering_all_years = sum(ape_clustering_all_years)/n(),
            MAPE_mc_list = sum(ape_mc_list)/n())

error_usnp_df %>% select(c("year",starts_with("MAPE")))

#### comparer avec les 1000



###########################################################################################################################################
############## Exploring other clustering possibilites including median and sd                                               ##############
############## Kept the code but mean based clustering is the best option                                                    ##############
###########################################################################################################################################
#---------------------------------------------------- Clustering with median values only--------------------------------------------------#
clust_median_only = lapply(names(dfs_outputs),function(year) make_clustering(dfs_outputs[[year]],use_median = TRUE)) %>% setNames(.,names(dfs_outputs))

fig_median_only = clust_median_only %>% {. ->> temp_list } %>% list(l = ., cols = names(.)) %$% 
  lapply(cols, function(y) cluster_graph(df = l[[y]]$df_cluster,
                                         medoids = l[[y]]$medoids,
                                         year = y,
                                         y = "revenu_median",
                                         ylab = "Revenu Median")) %>% setNames(., names(temp_list))

fig_init_median_only = clust_median_only %>% {. ->> temp_list } %>% list(l=. , cols = names(.)) %$% 
  lapply(cols,function (x) plot_initial_vs_new_cluster(df = l[[x]]$df_cluster,
                                                       initial = initial_mc_list,
                                                       medoids = l[[x]]$medoids,
                                                       year = x,
                                                       y = "revenu_median",
                                                       ylab = "Revenu Median"))  %>% setNames(.,names(temp_list)) 

#---------------------------------------------------- Clustering with mean & sd --------------------------------------------------#
clust_mean_sd = lapply(names(dfs_outputs),function(year) make_clustering(dfs_outputs[[year]],use_sd = TRUE)) %>% setNames(.,names(dfs_outputs))

fig_mean_sd = clust_mean_sd %>% {. ->> temp_list } %>% list(l = ., cols = names(.)) %$% 
  lapply(cols, function(y) cluster_graph(df = l[[y]]$df_cluster,
                                         medoids = l[[y]]$medoids,
                                         year = y,
                                         ylab = "Revenu moyen (clustering avec sd)")) %>% setNames(., names(temp_list))

fig_init_mean_sd = clust_mean_sd %>% {. ->> temp_list } %>% list(l=. , cols = names(.)) %$% 
  lapply(cols,function (x) plot_initial_vs_new_cluster(df = l[[x]]$df_cluster,
                                                       initial = initial_mc_list,
                                                       medoids = l[[x]]$medoids,
                                                       year = x,
                                                       ylab = "Revenu moyen (clustering avec sd)"))  %>% setNames(.,names(temp_list))

#---------------------------------------------------- Clustering with median & sd --------------------------------------------------#
clust_median_sd = lapply(names(dfs_outputs),function(year) make_clustering(dfs_outputs[[year]],use_median = TRUE, use_sd = TRUE)) %>% setNames(.,names(dfs_outputs))

fig_median_sd = clust_median_sd %>% {. ->> temp_list } %>% list(l = ., cols = names(.)) %$% 
  lapply(cols, function(y) cluster_graph(df = l[[y]]$df_cluster,
                                         medoids = l[[y]]$medoids,
                                         year = y,
                                         y = "revenu_median",
                                         ylab = "Revenu Median (clustering avec sd)")) %>% setNames(., names(temp_list))

fig_init_median_sd = clust_median_sd %>% {. ->> temp_list } %>% list(l=. , cols = names(.)) %$% 
  lapply(cols,function (x) plot_initial_vs_new_cluster(df = l[[x]]$df_cluster,
                                                       initial = initial_mc_list,
                                                       medoids = l[[x]]$medoids,
                                                       year = x,
                                                       y = "revenu_median",
                                                       ylab = "Revenu Median (clustering avec sd)"))  %>% setNames(.,names(temp_list))
########################################################################################################################################
########################################################################################################################################
### Prepare a list of set of years and compute revenues and compare with the reference of 1k MC years
### 
df_selection = readRDS()

### 1. Use a random list of set 
nb_sample = 1e6
Ntry = 10
nb_year = 1000
nb_year_select = 10
l_try_random <- map(seq(Ntry), ~ data.table(year_sel = sample.int(nb_year, nb_year_select, replace = FALSE)))
names(l_try_random) <- paste0("random_", seq(length(l_try_random)))
### 2. Create a set a list based on the 50 years selection made by DSVD
explicit_grid = expand.grid(5 + seq(0, 800, by = 200) , 6 + seq(0, 800, by = 200),
                            19 + seq(0, 800, by = 200),49 + seq(0, 800, by = 200),
                            67 + seq(0, 800, by = 200),79 + seq(0, 800, by = 200),
                            83 + seq(0, 800, by = 200),130 + seq(0, 800, by = 200),
                            139 + seq(0, 800, by = 200),153 + seq(0, 800, by = 200)) %>% as.data.table() %>% data.table::setnames(.,1:dim(.)[2] %>% as.character(.))

explicit_df = explicit_grid %>% t(.)
l_explicit = list()
for (i in 1:dim(explicit_df)[2]){
  name_col = paste0("explicit_",i)
  l_explicit[[name_col]] = explicit_df[,i] %>% data.table(.) %>% setNames(.,"year_sel")
}
# TODO : check selection & implement antares invest
# TODO : check an approach with a matrix