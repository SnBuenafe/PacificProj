# PacificProj Repo

### Creating conservation-sensitive, climate-smart fisheries closures for the ABNJ of the Pacific Ocean

Scripts that have been adapted are written/modified by multiple authors, all are part of the Mathematical Marine Ecology Laboratory of the University of Queensland, Australia:

* Kristine ("Tin") Buenafe (tinbuenafe@gmail.com)
* Isaac Brito-Morales
* Jason Everett
* Anthony J. Richardson

The sequencing of the `scripts` reflect the sequence of the workflow. Therefore, it is recommended to go through the scripts in sequential order to fully understand the workflow.

* `01`: creating the Robinson projected study area
* `02`: organizing raw AquaMaps data
* `03`: setting up bycatch data layer (using AquaMaps distributions) by intersecting the data with the study area
* `04`: setting up bycatch data layer (using IUCN distributions) by intersecting the data with the study area
* `05`: rerunning Mercer's GAMs and setting up the commercial layer by intersecting the data with the study area
* `06`: setting up cost layer
* `07`: organizing climate-smart features (RCE & climate velocity)
* `08`: retains PUs that belong to < 25th percentile of RCE and/or climate velocity
* `09`: assigning representation targets per feature
* `10`: running *prioritizr*
* `11`: creating no-regret plans
* `12`: creating summary statistics

The following folders are not pushed to this repository:

* inputs
* outputs
* pdfs
* excel
* statistics