# install.packages('fs')
library(fs)

make_project_dir <- function(){
    dir_names <- c(
        '00_Data',
        '01_Scripts',
        '02_Plots'
    )
    
    dir_create(dir_names)
    
    dir_ls()
}