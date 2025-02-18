# foodborne-module

This repository provides the implementation of the QMRA model corresponding to the foodborne module, 
intended to quantify ESBL producing E. coli in farm-to-fork broiler production chain.

## Directory layout

There are several subdirectors in this repository:

* [`data-input`](./data-input/) contains excel file with input parameters metadata. 
* [`data-output`](./data-output/) contains excel file with outputs.
* [`docs`](./docs/) provides documentaion and  Rmds for the model outputs. 
* [`utilities`](./docs/) provides scripts containing utility functions and visualization. 

The roles of the scripts in this repository:

* [`Module_homepreparation.R`](./Module_homepreparation.R): implements home-preparation submodule.
* [`Module_postprocessing.R`](./Module_postprocessing.R): implements post-processing submodule.
* [`Module_processing.R`](./Module_processing.R): implements processing submodule.
* [`Module_production.R`](./Module_production.R): implements production a.k.a logistics submodule.
* [`load_libraries.R`](./load_libraries.R): load necessary R packages.
* [`run_foodborne_module.R`](./run_foodborne_module.R): simulates user defined batches of production.

## Related resources

Bibliographic resources for this work can be found in this 
[repository](https://github.com/ENVIRE-JPIAMR/bibliography/tree/main/foodborne-module).