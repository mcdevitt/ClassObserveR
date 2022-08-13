#' Import multiple classroom observations
#'
#' Select a folder on your machine and all files matching a specified file type (e.g., .csv or .xls/.xlsx) will be imported.
#' Additional filtering can be made based on simple text patterns (e.g., all files that contain an instructor's name).
#' The filepath will be appended to the dataframe and can be used with add_metadata() to add additional metadata about the classroom observation.
#' Depending on the template used, column names may not import in a manner that is easily compatible with R (e.g., R does not like spaces and leading numbers within column names).
#' The function clean_codes() and add_coding_scheme() can be used to reformat column names for optimal use in other ClassObserveR functions.
#' @export
#' @param wd Select a working directory. By default, getwd() will be used to select your current working directory. A direct path to a folder can be specified
#' @param file.type Enter "xls" or "csv". This argument will be used to search for files in the directory ending with these file types. Note: "xls" will select both .xls and .xlsx file types.
#' @param  pattern.1 Enter a character string that will be used to filter files for upload.  By default, "" will be used so that only files matching your file type will be imported.
#' @param  pattern.2 Enter a character string that will be used to further filter files for upload.  By default, "" will be used by so that only files matching your "file type" and "pattern.1" will be imported.
#' @param  pattern.3 Enter a character string that will be used to further filter files for upload.  By default, "" will be used so that only files matching your "file type",  "pattern.1", and "pattern.2" will be imported.
#' @param template.type Optional. We are working on adding common classroom observation templates. Selecting a template type other than default will override "row.skip".
#' @param excel.sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). This argument will be ignored if file.type != "xls".
#' @param row.skip Enter the number of rows to skip during import. The following row (i.e., "row.skip" + 1) will used to specify column headers. By default, 0 is used so that row 1 is used to specify column headers.
#' @param ... Additional arguments from \link[?readxl:read_excel()]{read_excel} or \link[?base:read_csv()]{read_csv} can be passed through this function. However, this is not recommended when specifying a template.type other than "default".
batch_import <-
  function(wd = getwd(),
           file.type = "xls",
           pattern.1 = '', #default returns all files of the specified file type
           pattern.2 = '', #default returns all files of the specified file type
           pattern.3 = '', #default returns all files of the specified file type
           template.type = "default",
           excel.sheet = 1,
           row.skip = 0
           ) {
    #list all relevant files that match the specified file type
    #
    obs.file.list <- sapply(list.files(
      path = wd,
      pattern =  paste0('.', file.type),
      recursive = T,
      include.dirs = T
    ),
    function(x) {
      paste(wd, x, sep = '/')
    })
    #Filter files based on pattern 1
    obs.file.list <-
      obs.file.list[grep(pattern = pattern.1, x = obs.file.list)]
    #Filter files based on pattern 1
    obs.file.list <-
      obs.file.list[grep(pattern = pattern.2, x = obs.file.list)]
    #Filter files based on pattern 1
    obs.file.list <-
      obs.file.list[grep(pattern = pattern.3, x = obs.file.list)]
#load default excel templates
    if (template.type == "default" & file.type == "xls") {
      obs.l <- lapply(1:length(obs.file.list), function(x) {
        print(paste0('Importing file ', x, '/', length(obs.file.list)))
        obx <- read_excel(obs.file.list[x],
                          sheet = excel.sheet,
                          skip = row.skip)
        obx.df <- as.data.frame(obx)
        filename <- obs.file.list[x]
        comb <- cbind(filename, obx.df)
        comb
      })
    }
#load default csv templates
    if (template.type == "default" & file.type == "csv") {
      obs.l <- lapply(1:length(obs.file.list), function(x) {
        print(paste0('Importing file ', x, '/', length(obs.file.list)))
        obx <- read.csv(obs.file.list[x],  skip = row.skip)
        obx.df <- as.data.frame(obx)
        filename <- obs.file.list[x]
        comb <- cbind(filename, obx.df)
        comb
      })
    }
    obs.df <- do.call(rbind.fill, obs.l)
    row.rm <- which(rowSums(!is.na(obs.df[-1])) == 0)
    obs.rm <- obs.df[-c(row.rm), ]
  }

#test <- batch_import(wd = 'D:/IRB_secure/COPUS/02-Originals-UCD-Fall2016', pattern.1 = 'Kim', excel.sheet = 1, row.skip = 4)
