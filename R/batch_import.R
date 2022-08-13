#' Import multiple classroom observations
#' @export
batch_import <-
  function(wd = wd(),
           file.type = "xls",
           pattern.1 = '', #default returns all files of the specified file type
           pattern.2 = '', #default returns all files of the specified file type
           pattern.3 = '', #default returns all files of the specified file type
           template.type = "default",
           excel.sheet = 1,
           row.skip = 0,
           ) {
    #list all relevant files that match the specified file type
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

    obs.df <- do.call(rbind.fill, obs.l)
    row.rm <- which(rowSums(!is.na(obs.df[-1])) == 0)
    obs.rm <- obs.df[-c(row.rm), ]
  }

test <- batch_import(wd = 'D:/IRB_secure/COPUS/02-Originals-UCD-Fall2016', pattern.1 = 'Kim', excel.sheet = 1, row.skip = 4)
