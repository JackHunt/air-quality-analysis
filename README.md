# air-quality-analysis
Air quality analysis.

## Setup
Then install packages; in an R session run
```R
source("install_packages.R")
```

## Example Run
```shell
rm -rf out
Rscript src/single_sensor_analysis.R -i data/wcan -s 270280 --prediction_proportion 0.4
Rscript src/single_sensor_vis.R -i out/270280
```
