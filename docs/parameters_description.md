# Configuration parameters

## Global parameters

In [`/settings/global_params.yaml`](/settings/global_params.yaml) :

* **`use_api`** : If yes or no, runs are performed using antaresWeb
* **`allow_invest`** : If yes or no, investments are allowed.
* **`CRM_forced`** : If yes or no, a CRM based on the convergence to criteria
* **`antares_sub_dir`** : subdirectoty name of the antares simulation data. Name here : `/data/antares/<antares_sub_dir>`
* **`static_mode`** : if yes or no, a basic run just to compute revenues for 1 loop on one year
* **`parallel_loading`** : if yes or no, processes are paralleled in the code.
* **`adaptative_power_increment`** : If yes or no, the power increment has to be adaptative and change with the margin.
* **`areas`** : areas of interest. If set to `all`, `areas = all_areas` parameter in `/settings/antares_params.yaml`
* **`antares_to_simulate`** : names of the years to simulate
* **`antares_futures_study`** : names of the futures studies (not compulsory)
* **`delay_investment_decision`** : if investment decision has to be taken with delay
* **`capacity_mechanism`** :
  * *`enabled`* : If yes or no, CRM are activated
  * *`price_cap_by_technology`* : If yes or no the price cap is fixed by country, independently for clusters. In case, refer to 'all'
* **`moving_price_cap`** :
  * *`enabled`* : If yes or no, the moving price cap option is activated
  * *`cap_threshold_share`* : share part of the price cap to reach to make a increase
  * *`cap_raising`* : amount in euros/MWh to add in case an increase is decided
  * *`duration_threshold`* : minimum duration reaching the `cap_threshold_share` x `initial_price_cap` to trigger an increase. 
  * *`price_cap_energy`* : default price cap of the energy only market 
* **`reliability_options`** :
  * *`enabled`* : if yes or no this option is activated
  * *`dynamic_strike_price`* :
    * *`enabled`*: if yes or no the strike price is based on the marginal bidder
    * *`surplus`* : The quantity to add the marginal bidder in euros/MWh
  * *`by_capacity`* : if no, RO strike price is fixed and set by country. If yes it is fixed and by technology type.
* **`antares_run_save`** : option to save the antares run, and the corresponding ID

## Antares

In [`/settings/antares_params.yaml`](/settings/antares_params.yaml) :

* **`mc_list`** : montecarlo vectors of the selected clustered years. Set to `all` to get all 1000 Monte carlo scenarios.
* **`mc_number`** : (not compulsory). Use it if mc_list is empty and you want to use `1; 2; 3... mc_number` as `mc_list`
* **`iteration_max`** : Loop number to perform on each actual year to simulate
* **`antares_mode`** : default *economy*
* **`sub_mode`** : default *fast*. *accurate* to increase accuracy
* **`batteries_country`** : antares country for batteries
* **`nb_cores`** : core number to run the parallelised process (antares runs, and others functions) in local mode.
* **`nb_cpu`** : nb of cpu unit used in AntaresWeb (out of 24)
* **`antares_hour`** : default 8736 hours
* **`all_areas`** : the whole set of countries in the Antares study
* **`api_study_list`** : list with the ID's of the antares Web studies
  
## Capacity market

In [`/settings/capacity_market.yaml`](/settings/capacity_market.yaml) :

* **`CRM`** :
  * *`area`* :
    * all arguments relative to the capacity remuneration mechanism of the area concerned ..
* **`demand`** : the demand points or parameters used to build the demand curve
  * *`type`* : `inelastic` or  `slope_break`
* **`LOLE_margin_equation`** : parameters of the equation *margin = a ln(LOLE) + b* which binds the margin to the LOLE
* **`reliability_standard`** : Loss of load expectation criteria for each country
* **`LOLE_margin`** : threshold in hour between the LOLE target and the LOLD to not take any decisions
* **`RO_strike_price_country`** : Strike price for each country if no dynamic strike price
* **`RO_strike_price_technology`** : Strike price for each kind of technology if no dynamic strike price

## Graphic

In [`/settings/graphic_option.yaml`](/settings/graphic_option.yaml) :

Find every color code in HTML encoding.

## Investment

In [`/settings/investment_params.yaml`](/settings/investment_params.yaml) :

* **`metric`** : used metric to compute and asses economic viability
* **`minimum_increment`** : minimum increment threshold to add or remove a capacity
* **`minimum_revenue_margin`** : threshold to consider capacity profitable
* **`power_increment`** : arithmetic increment of power to remove/add capacities at each round for each country
* **`link_increment`** : geometric increment to remove/add dsr link capacity
* **`discount_rate`**
* **`service_system_total`** : service system amount in euros to be shared among capacities for each country
* **`service_system_share`** : service system amount shares by capacity type
* **`retirement_time`** : time (in year) to close capacities. The closing happens on year_actual + closing_time
* **`time_horizon_ENP`** : time (in year) to compute and assess the expected net profit of a capacity.
* **`limit_investment`** : limit of investment capacity by year by country
* **`limit_retired`**: limit of retirement in capacity by year by country
* **`time_horizon_investor`** : delay set up for investment decision.

## User

In `/settings/user_params.yaml` :

* **`token`** : personal key to access the AntaresWeb API. You can edit only the study you have imported.
* **`user`** : username

## Naming

In [`/settings/naming_params.yaml`](/settings/naming_params.yaml) :

Mainly correspondancy between names of capacity and their capacity group/type

* **`peculiarities`** : naming distinction which leads to a sub group
* **`pattern_to_avoid`** : naming pattern to add to avoid in all the study (exemple : 'mr' for must run)
* **`renewables_type`** : naming pattern of the renewables sources

## Hypothesis

In [`/hypothesis/<area>`](/data/hypothesis/):

Hypothesis costs and others filters for retirement, investment, policy candidates are all in `data/hypothesis/<area>` stored as yaml file.

**`costs.yaml`** : all costs hypothesis by technology type.

**`investment_filters.yaml`**:
  * *`clusters_supported`* : capacities supported by other remuneration mechanisms than energy only. Not considered into revenues computation/investment decisions
  * *`clusters_not_supported`* : capacities not supported by other remuneration mechanisms than energy only. Considered into revenues computation/investment decisions
  * *`candidates_investment`* : candidates to investments capacities.
  * *`candidates_retirement`* : capacities candidates to retirement.
  * *`specific_cluster_to_avoid`* : specify the name of cluster to avoid in the analysis (ie revenues + investments - clusters will be considered as supported).