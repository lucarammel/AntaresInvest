Project Organisation
--------

    ├── LICENSE
    ├── DESCRIPTION
    ├── README.md        <- The top-level README
    ├── data
    │   ├── hypothesis   <- Hypothesis from third-party sources. Here costs for technology by country.
    │   └── antares      <- Antares simulation dataset with all years.
    │
    ├── settings         <- parameters to run the model stored as yaml configuration files.
    │
    ├── results          <- results obtained by AntaresInvest. Graphs, excel files, log file, parameters used automatically generated
    │
    ├── docs             <- documentations on the project, README parts.
    │
    ├── man              <- All the docstrings of the AntaresInvest functions.
    │
    ├── NAMESPACE        <- Exported functions of AntaresInvest and imported packages
    │
    ├── mc_years_clustering.R <- main script to run the selection/clustering on MC years and get a subset of mc_list for AntaresInvest   
    ├── main.R           <- main script to run AntaresInvest.
    │
    ├── R                <- Source code for use in this project.
    │   ├── 1_..         <- Scripts relative to the data processing directly from Antares and hypothesis.
    │   │
    │   ├── 2_..         <- Scripts to compute revenues from energy-only and CRM.
    │   │
    │   ├── 3_..         <- Scripts to prepare data for investments decisions and perform the decision.
    │   │
    │   ├── 4_..         <- Antares functions, to run, restore, parallelised, add, remove powerplants..
    │   │
    │   ├── 5_..         <- Script to build the config files, load them, and store them.
    │   │
    │   ├── 6_..         <- Script to store outputs, reformat, and plot graphs.
    │   │
    │   ├── 7_..         <- Utilities functions for the main script.
    │   │
    │   ├── 8_..         <- Script for price cap moving features.
      
--------

<p>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</p>
