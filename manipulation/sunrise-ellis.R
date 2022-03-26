rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

# Import only certain functions of a package into the search path.

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("rlang"        ) # Language constructs, like quosures
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()

# Execute to specify the column types.  It might require some manual adjustment (eg doubles to integers).
#   OuhscMunge::readr_spec_aligned(config$path_raw_sunrise)
col_types <- readr::cols_only(
  Date           = readr::col_character(),
  Sunrise        = readr::col_time(),
  Sunset         = readr::col_time(),
  # Daylength      = readr::col_character()
)

# ---- load-data ---------------------------------------------------------------
# Read the CSVs

# Copied from https://geotsy.com/en/ukraine/vinnytsia-20279/sunrise-and-sunset
ds <- readr::read_tsv(config$path_raw_sunrise  , col_types=col_types)

rm(col_types)

# Print the first few rows of each table, especially if you're stitching with knitr (see first line of this file).
#   If you print, make sure that the datasets don't contain any PHI.
#   A normal `data.frame` will print all rows.  But `readr::read_csv()` returns a `tibble::tibble`,
#   which prints only the first 10 rows by default.  It also lists the data type of each column.
ds

# ---- tweak-data --------------------------------------------------------------
# OuhscMunge::column_rename_headstart(ds) # Help write `dplyr::select()` call.
ds <-
  ds |>
  dplyr::select(    # `dplyr::select()` drops columns not included.
    date                   = `Date`,
    sunrise                = `Sunrise`,
    sunset                 = `Sunset`,
  ) |>
  dplyr::mutate(
    date  = paste("2022", date),
    date  = as.Date(date, "%Y %d %b"), # Year day month 
  )  |>
  dplyr::arrange(date)

# ---- verify-values -----------------------------------------------------------
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_date(    ds$date    , any.missing=F , lower=as.Date("2022-01-01"), upper=as.Date("2022-12-31") , unique=T)
checkmate::assert_character(as.character(ds$sunrise), any.missing=F  , pattern = "^\\d{2}:\\d{2}:00$"  )
checkmate::assert_character(as.character(ds$sunset ), any.missing=F  , pattern = "^\\d{2}:\\d{2}:00$"  )

# ---- specify-columns-to-upload -----------------------------------------------
# Print colnames that `dplyr::select()`  should contain below:
#   cat(paste0("    ", colnames(ds), collapse=",\n"))

# Define the subset of columns that will be needed in the analyses.
#   The fewer columns that are exported, the fewer things that can break downstream.

ds_slim <-
  ds |>
  # dplyr::slice(1:100) |>
  dplyr::select(
    date,
    sunrise,
    sunset
  )

ds_slim

# ---- save-to-disk ------------------------------------------------------------
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
readr::write_csv(ds_slim, config$path_derived_sunrise_csv)
readr::write_rds(ds_slim, config$path_derived_sunrise_rds, compress="gz") # Save as a compressed R-binary file if it's large or has a lot of factors.
