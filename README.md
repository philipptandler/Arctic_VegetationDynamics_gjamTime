# Arctic_VegetationDynamics_gjamTime

This repository contains the scripts used for my master thesis.

## Direcory Stucture
- `data/`: Contains all data files.
- `Scripts/`: Contains all R scripts.
- `rootdir.Rproj`: R project file.
- The `data/`directory can be downloaded under #TODO
- The `Scripts/Processing/Create_....R` scripts require their respective rawdata in `data/`.
The `Scripts/gjamTime/gjamTime.R`script requires the processed inputdata in `data/gjamTime_data/`, as well as a directory `data/gjamTime_outputs/`.

## How to Run the Scripts
1. Open `rootdir.Rproj` in RStudio.
2. Run the scripts from the `Scripts/` directory. Paths to data files are managed using the `here` package.

or
1. Run Scripts from the command line in the rootdirectory

## Dependencies
- R version 4.3.3
- R packages: `here`, `dplyr`, `devtools`, `terra`, `gjam`, `mvtnorm`, `matlib`

## Contact
For any questions, please contact tandlerp@ethz.ch.

