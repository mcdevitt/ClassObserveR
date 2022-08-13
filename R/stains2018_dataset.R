#' COPUS data from: Anatomy of STEM teaching in North American higher-ed
#'
#' Data set containing COPUS (Classroom Observation for Undergraduate STEM) coding of 2008 STEM classroom observations collected from 548 faculty across 25 institutions in North America.
#' This function will check to see if the dataset is in your working directory. If not, it will download the file from the repository and then load the dataset into your R environment.
#'
#' Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/48MMF9
#'
#' Citation: Stains, Marilyne, 2018, "Replication Data for: Anatomy of STEM teaching in North American higher-ed", https://doi.org/10.7910/DVN/48MMF9, Harvard Dataverse, V1
stains2018_dataset <- function() {
  if (!file.exists(paste0(getwd(), '/stains2018_dataset.xlsx'))){
    download.file(url = 'https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/48MMF9/GYFSND',
                  destfile = 'stains2018_dataset.xlsx',
                  mode = 'wb')
  }
  stains2018 <-
    read_excel(paste0(getwd(), '/stains2018_dataset.xlsx'), sheet = 1)
}
