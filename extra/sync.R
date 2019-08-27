library(fs)
library(glue)
library(here)
library(stringr)

release <- "1.16"
# release <- "master"

download_and_sync <- function(release = "master") {
  here_dir <- here()
  if (path_file(here_dir) != "almanac") {
    stop("Must be in the `almanac` RStudio Project!")
  }

  # Both download zip files
  if (release == "master") {
    download_url <- "https://api.github.com/repos/lballabio/QuantLib/zipball/master"
  } else {
    download_url <- glue("https://github.com/lballabio/QuantLib/releases/download/QuantLib-v{release}/QuantLib-{release}.zip")
  }

  tmp_dir <- tempdir()
  tmp_file <- path(tmp_dir, "quantlib.zip")

  download.file(download_url, tmp_file)
  on.exit(unlink(tmp_file))
  unzip(tmp_file, exdir = tmp_dir)

  if (release == "master") {
    tmp_dir_files <- dir_ls(tmp_dir)
    quantlib_dir <- tmp_dir_files[str_detect(tmp_dir_files, "lballabio-QuantLib")]
    quantlib_ql_dir <- path(quantlib_dir, "ql")
  } else {
    quantlib_ql_dir <- path(tmp_dir, glue("QuantLib-{release}"), "ql")
  }

  almanac_ql_dir <- path(here_dir, "src", "ql")

  files_to_update <- dir_ls(almanac_ql_dir, recurse = TRUE)
  files_to_update <- files_to_update[str_ends(files_to_update, c(".cpp", ".hpp"))]

  file_stubs <- gsub(almanac_ql_dir, "", files_to_update)
  files_to_copy <- path(quantlib_ql_dir, file_stubs)

  file_copy(files_to_copy, files_to_update, overwrite = TRUE)

  message(
    "Files copied!",
    "\n",
    "Now comment out pragmas in:",
    "\n",
    "src/ql/patterns/observable.cpp",
    "\n",
    "src/ql/time/date.cpp",
    "\n",
    "src/ql/time/imm.cpp",
    "\n",
    "src/ql/utilities/dataparsers.cpp",
    "\n",
    "src/ql/utilities/null.hpp",
    "\n",
    "Search for 'ALMANAC EDIT - COMMENT OUT PRAGMA'"
  )
}

download_and_sync(release)
