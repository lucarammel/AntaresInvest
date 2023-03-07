# Data format description

* `study_list`
* `dataset`
* `dataset_revenues`
* `investment_metrics`
* `data_outputs`
* `decisions`

Let's have a look across all the different data structure, the tree structure is easily accessible with a "$" symbol:

* **`study_list`** is obtained by the the mother function *build_study_list()* which creates the list to easily store each study_id, output of each simulation launched by the investment model. This automatically generated based either on api_study_list or local_study_list depending on your *use_api* parameter. The list is organised as follows :
  * *`year_simulated`*
    * *`reference_id`* study_id of the reference simulation. This is crucial for the investment to work properly with antares web. Locally study_id = year_simulated
    * *`variant_id`* study_id of the variant created. This is the simulation which will be modified.

* **`power_plants`** is obtained by the function *make_powerplants_from_antares()* which creates the list of the powerfleet and all its description and the hypothesis costs by clusters. The list is organised as follows :
  * *`year_simulated`*
    * *`area`*

* **`dataset`** is obtained by the the mother function *build_dataset()* which reads and format data after the Antares runs on the years simulated. The list is organised as follows :
  * *`year_simulated`*
    * *`area`*
      * *`clusters_filters`*
        * *`not_supported`* vector of the clusters not supported, those who get their revenues only from market mecanisms
        * *`supported`* vector of the cluster supported, and get their revenues from other mecanisms
        * *`candidates_investments`* vector of the clusters candidates to the investment decision
        * *`candidates_retirement`* vector of the clusters candidates to the retirement decision
      * *`data_antares`* a list of the outputs obtained from the runs of antares simulation, for example you could find :
        * `thermal_availability`
        * `areas`
        * `cluster_description`
        * `links`
        * etc ...

* **`dataset_revenues`** is obtained after the computation of the revenues from the energy only market and capacity market simulation. It is organised as follows. **When it is not precised, the revenues are expressed in euros/MW** :

  * *`year_simulated`*
    * *`area`*
      * *`market_info_supply`* a dataframe with all the market information of the supply side (revenues, bids on capacity market, capacity ..)
      * *`auction_info`* a dataframe with the capacity market auction information (clearing price, marginal bid ..) 
      * *`revenues`* a list of dataframe, keys are the clusters.
        * *`revenues_annual`* available for heat, energy and system service revenues
        * *`revenues_hourly`* available for heat and energy revenues
        * *`production`* a dataframe with the hourly production as of the different montecarlo years

* **`investment_metrics`** is obtained after reformatting and extracting data from the dataset_revenues variable.
  * *`area`*
    * *`metrics`* the dataframe with the investment metric computed on simulated years for each cluster
    * *`revenues_total_net`* dataframe with the total net revenues for each cluster on each years (energy only + capacity market  operational costs). Used for decommissioning
    * *`policy_retired`* vector of clusters names that are already decommissionned by the antares simulation dataset.

* **`data_outputs`** data outputs from the model. This a record of outputs variable after each loop, for each year.
  * *`area`*
    * *`year_simulated`*
      * *`loop number`* the loop number on the year_simulated
        * *`spot`* the averaged annual spot price
        * *`power_plants`* the fleet of power_plants
        * *`LOLD`* loss of load duration
        * *`revenues`* the annual revenues for each cluster.
        * ...

* **`decisions`** retirement/investment decisions taken by the model. If `continue = 1`, the model continues to loop on the same actual year.*`power_plants_moves`* is the record of every moves of the model. With useful information related to it.
