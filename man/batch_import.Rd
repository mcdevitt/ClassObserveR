% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch_import.R
\name{batch_import}
\alias{batch_import}
\title{Import multiple classroom observations}
\usage{
batch_import(
  wd = getwd(),
  file.type = "xls",
  pattern.1 = "",
  pattern.2 = "",
  pattern.3 = "",
  template.type = "default",
  excel.sheet = 1,
  row.skip = 0
)
}
\arguments{
\item{wd}{Select a working directory. By default, getwd() will be used to select your current working directory. A direct path to a folder can be specified}

\item{file.type}{Enter "xls" or "csv". This argument will be used to search for files in the directory ending with these file types. Note: "xls" will select both .xls and .xlsx file types.}

\item{pattern.1}{Enter a character string that will be used to filter files for upload.  By default, "" will be used so that only files matching your file type will be imported.}

\item{pattern.2}{Enter a character string that will be used to further filter files for upload.  By default, "" will be used by so that only files matching your "file type" and "pattern.1" will be imported.}

\item{pattern.3}{Enter a character string that will be used to further filter files for upload.  By default, "" will be used so that only files matching your "file type",  "pattern.1", and "pattern.2" will be imported.}

\item{template.type}{Optional. We are working on adding common classroom observation templates. Selecting a template type other than default will override "row.skip".}

\item{excel.sheet}{Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). This argument will be ignored if file.type != "xls".}

\item{row.skip}{Enter the number of rows to skip during import. The following row (i.e., "row.skip" + 1) will used to specify column headers. By default, 0 is used so that row 1 is used to specify column headers.}

\item{...}{Additional arguments from \code{\link[readxl:read_excel]{read_excel()}} or \code{\link[utils:read_csv]{read_csv()}} can be passed through this function. However, this is not recommended when specifying a template.type other than "default".}
}
\description{
Select a folder on your machine and all files matching a specified file type (e.g., .csv or .xls/.xlsx) will be imported.
Additional filtering can be made based on simple text patterns (e.g., all files that contain an instructor's name).
The filepath will be appended to the dataframe and can be used with add_metadata() to add additional metadata about the classroom observation.
Depending on the template used, column names may not import in a manner that is easily compatible with R (e.g., R does not like spaces and leading numbers within column names).
The function \code{\link{clean_codes}} and add_coding_scheme() can be used to reformat column names for optimal use in other ClassObserveR functions.
}
