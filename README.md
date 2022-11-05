# EMu-Registry-Groups
Heatmaps & Summaries of eregistry permission settings for EMu User Groups

## In-house demo-setup:
http://10.10.10.247:3838/EMuRegistryGroups 

## How to setup and run this locally
1. Clone the repo.
2. In the repo, add a text file named `.Renviron`, following the [`.Renviron.example`](https://github.com/magpiedin/EMu-Registry-Groups/blob/master/.Renviron.example) file. Adjust values for variables if needed.
3. Install [R and RStudio Desktop](https://posit.co/download/rstudio-desktop/)
4. Open an R console -- e.g. in terminal/shell type `R`, or open RStudio and go to the Console pane.

...In the R console: 

5. Install [required R packages](https://github.com/magpiedin/EMu-Registry-Groups/blob/master/requirements.txt), e.g.: `install.packages(c('heatmaply')`
6. Run the app by entering: `shiny::runApp()`


## How to update the input-data:

1. In your copy of this repo, check and add if needed:
    - a subdirectory named: `real_data_in/table_security/`
    - a text file named `.Renviron`, following the [`.Renviron.example`](https://github.com/magpiedin/EMu-Registry-Groups/blob/master/.Renviron.example) file. Adjust values if needed.
2. In EMu, open the Registry module, and make a CSV report that includes Keys1-10 and the Value columns. 
3. Report out all records. 
4. Copy the report output `eregistr.csv` file to `real_data_in/table_security/` in this repo.
5. Re-run `app.R` -- e.g. by entering this at the R console: `shiny::runApp()`


For general shiny app help, see https://shiny.rstudio.com/articles/running.html



