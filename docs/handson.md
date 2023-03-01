
## R set up

To set up Rstudio on RTE computers and bypass the proxy issues :

* [Download](https://cran.r-project.org/bin/windows/Rtools/) Rtools and install it.

```R
#Run this
file.edit("~/.Rprofile") 

#Copy and paste to the opened file
local({r <- getOption("repos")
r["CRAN"] <- "http://poker-lib.rte-france.com/poker-all/latest"
options(repos=r)})

#Check if it works with this command. If no, restart
getOption("repos")

#This should be the output  
"http://cran.rte-france.com/" 
attr(,"RStudio")
[1] TRUE
```

## Update antaresRead & antaresEditObject with latest versions

To update directly from the github of rte-antares packages and not from the CRAN, follow the instructions below :

* Download the [**archives**](https://github.com/rte-antares-rpackage/) and extract them.
* Run the command and install `antaresRead` **before** `antaresEditObject`:

```R
install.packages(path_to_archive, repos = NULL ,type = "source") # Try this 

devtools::install(path_to_archive) # Or this one

packageVersion(package_name) # To check if package Version is the one you want (the github one)

```

## Hands-on

To get into the model, open the *main.R* script into RStudio. `settings/` folder are all the parameter needed to feed AntaresInvest.

Before running anything in **local** : **MAKE SURE YOU SAVE PROPERLY THE ANTARES DATASET** because the algorithm changes the dataset at the roots. To make a proper save please follow the guidelines :

* Run the code below. Make sure you run this here : `/antaresinvest`

```R
library(magrittr) # Load magrittr to import the pipe operator: %>% 

### Build the project directory variable path
project_dir = rstudioapi::getActiveDocumentContext()$path %>% strsplit(., split = "/", fixed = T)  %>% .[[1]] %>% .[-c(length(.),length(.)-1)] %>%
  paste(.,collapse="/")

### Loading AntaresInvest
antaresinvest_directory = paste(project_dir, 'antaresinvest', sep = '/')
devtools::load_all(antaresinvest_directory)

### Loading settings and config files
build_parameters_input()

### Build study list from parameters
study_list = build_study_list(study_list = study_list)

### Load the powerfleet for each country and simulation year
power_plants = lapply(antares_study_list, function(simulation_name) lapply(areas,function(area) make_powerplants_from_antares(study_id = study_list[[simulation_name]]$variant_id,
                                                                                                                                  zone = area, 
                                                                                                                                  year_simulated = simulation_name))) %>% rename_dataset(., years_to_simulate = antares_study_list,areas = all_areas)
  

### Check and apply correction 
tmp = lapply(as.integer(antares_study_list), function(year) lapply(all_areas, function(area) check_antares_dataset(study_id = year,
                                                                                                                   power_plants = power_plants[[as.character(year)]][[area]],
                                                                                                                   power_plants_actual = power_plants[[antares_study_list[1]]][[area]],
                                                                                                                   area = area,
                                                                                                                   lowering = TRUE,
                                                                                                                   filling_info_cluster = TRUE,
                                                                                                                   editing_outputs = TRUE)))

restore_antares_simulation(save_id = '0_0',
                           saving_mode = TRUE,
                           end_etude = FALSE)
```
