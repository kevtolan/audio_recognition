
remotes::install_gitlab(
  repo = "vtcfwru/ammonitor@AMMonitor2.2",
  auth_token = Sys.getenv("GITLAB_PAT"),
  host = "code.usgs.gov",
  build_vignettes = FALSE,
  dependencies = TRUE,
  upgrade = "never")
##### if it times out during download, use this and add "# R_MAX_VSIZE=100Gb" to the Renvir file
# library(usethis)
# edit_r_environ()
# >>>> R_MAX_VSIZE=100Gb




options(timeout=1000) #override typical timeout to allow for larger audio files to load
# required packages
library(AMMonitor)
library(tidyverse)
library(RSQLite)
library(DBI)

#for other functions
# library(sf) # spatial analysis
# library(aws.s3) # to host audio use Amazon's web services
# library(padr) # pad dates of a timeseries
# library(plotly) # to make ggplots interactive




setwd('~') # The folder that you want the database's structure located in
ammCreateDirectories("xx", paste0(getwd())) # creates directory WITHIN current wd
setwd('~/xx') # set directory to the newly created directory. Set this as wd for all analysis in the future

dbCreate(
new_db_name = "yy.sqlite",
new_db_filepath = paste0(getwd(), "/database"),
db_source = "default")

db.path <- '~/xx/database/yy.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

# launch app to  create templates,add sites, add/view media, 
# manually annotate data, and verify model detections
AMMonitor::launchApp()

#### THIS IS HOW AUDIO DETECTIONS ARE RUN. *NOT* THROUGH THE APP
## After adding sites/visits/media using launchApp(), use this to subset files for analysis
# in this format: subset_files(conx, 'siteID', "firstdate", "lastdate")
# or create other vector/df of filenames for analysis (files must be included in database)

subset_files <- function(conx, site, start_date, stop_date) {
  mediafiles <- RSQLite::dbReadTable(conn = conx, name = 'media')
  mediafiles$Site <- str_extract(mediafiles$filename, "[^_]+")
  mediafiles <- mediafiles[mediafiles$Site == site, ]
  mediafiles$date <- paste0(mediafiles$start_date, " ", mediafiles$start_time) %>%
    as.POSIXlt()
  DATESTART <- as.Date(start_date)
  DATESTOP <- as.Date(stop_date)
  mediasubset <- mediafiles %>% filter(between(date, DATESTART, DATESTOP)) }

mediasubset <- subset_files(conx, 'siteID', "yyyy-mm-dd", "yyyy-mm-dd")

# run detections
scores <- scoresDetect(
  con = conx,
  recordingNames = mediasubset$filename,
  templateNames = 'templateID', # make templates in launchApp(), but can be shared as .rds once made
  # scoreThresholds = 12, # not currently functioning, only uses threshold from template
  recordingRootPath = 'filedirectory',
  ammlPath = paste0(getwd(), "/ammls"),
  dbInsert = T,
  showProgress = T
)



birdsDetect <- function(
    con,
    recordingNames = "all",
    minConfidence = 0.1,
    speciesList = NULL,
    modelVersion = "v2.4",
    language = "en_us",
    dbInsert = FALSE,
    showProgress = FALSE
) {

  if (!requireNamespace("birdnetR", quietly = TRUE)) {
    stop("Package 'birdnetR' is required. Install with install.packages('birdnetR').")
  }

  # ---- Resolve the registered BirdNET model row ----
  model_name <- paste0("BirdNET_", modelVersion)
  modelID <- DBI::dbGetQuery(
    con,
    paste0("SELECT pk_modelid FROM models WHERE model_name = '", model_name, "';")
  )[, 1]
  if (length(modelID) == 0) {
    stop(
      "No model named '", model_name, "' found in the models table. ",
      "Run Register_BirdNET_Model.R first (or register it under a different modelVersion)."
    )
  }

  # ---- Resolve which recordings to process ----
  if (identical(recordingNames, "all")) {
    media <- DBI::dbGetQuery(con, "SELECT pk_mediaid, filename, filepath FROM media WHERE media_type = 'audio';")
  } else {
    in_list <- paste(sprintf("'%s'", recordingNames), collapse = ", ")
    media <- DBI::dbGetQuery(con, paste0("SELECT pk_mediaid, filename, filepath FROM media WHERE filename IN (", in_list, ");"))
  }

  if (nrow(media) == 0) {
    message("No matching recordings found.")
    return(invisible(NULL))
  }

  # ---- Load the model once, reused across all recordings ----
  model <- birdnetR::birdnet_model_tflite(version = modelVersion, language = language)

  # ---- Resolve species filter ----
  # BirdNET's filter_species must be in its internal "Scientific name_Common
  # name" label format, not plain common names -- convert here.
  if (identical(speciesList, NA)) {
    filter_species <- NULL  # no filtering -- full global BirdNET species list
  } else {
    common_names <- if (is.null(speciesList)) {
      if (!exists("birdSpeciesList")) {
        stop("birdSpeciesList() not found -- source birdSpeciesList.R first, or pass speciesList explicitly.")
      }
      birdSpeciesList()
    } else {
      speciesList
    }

    all_labels <- birdnetR::read_labels(birdnetR::labels_path(model, language = language))
    label_common_names <- sub("^.*_", "", all_labels)

    filter_species <- all_labels[label_common_names %in% common_names]
    unmatched <- setdiff(common_names, label_common_names)
    if (length(unmatched) > 0) {
      warning(
        length(unmatched), " species in the species list don't match any BirdNET label and will be ignored: ",
        paste(unmatched, collapse = ", "),
        call. = FALSE
      )
    }
  }

  td <- tempdir(check = TRUE)
  all_results <- vector("list", nrow(media))

  for (i in seq_len(nrow(media))) {
    if (showProgress) cat(i, "/", nrow(media), ":", media$filename[i], "\n")

    fp <- media$filepath[i]
    is_remote <- grepl("^https?://", fp)
    local_path <- if (is_remote) {
      dest <- file.path(td, media$filename[i])
      ok <- tryCatch({
        utils::download.file(fp, dest, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) dest else NA_character_
    } else if (file.exists(fp)) {
      fp
    } else {
      NA_character_
    }

    if (is.na(local_path)) {
      warning("Could not access recording for ", media$filename[i], "; skipping.")
      next
    }

    preds <- tryCatch(
      birdnetR::predict_species_from_audio_file(
        model,
        local_path,
        min_confidence = minConfidence,
        filter_species = filter_species,
        keep_empty = TRUE
      ),
      error = function(e) {
        warning("BirdNET failed on ", media$filename[i], ": ", conditionMessage(e))
        NULL
      }
    )

    if (is_remote && file.exists(local_path)) unlink(local_path)
    if (is.null(preds)) next

    real_detections <- preds[!is.na(preds$common_name), ]

    if (nrow(real_detections) == 0) {
      all_results[[i]] <- data.frame(
        fk_mediaid = media$pk_mediaid[i],
        fk_modelid = modelID,
        fk_taxonid = "no-species",
        x_min = NA_real_,
        x_max = NA_real_,
        y_min = NA_real_,
        y_max = NA_real_,
        value_num = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      all_results[[i]] <- data.frame(
        fk_mediaid = media$pk_mediaid[i],
        fk_modelid = modelID,
        fk_taxonid = real_detections$common_name,
        x_min = real_detections$start,
        x_max = real_detections$end,
        y_min = NA_real_,
        y_max = NA_real_,
        value_num = real_detections$confidence,
        stringsAsFactors = FALSE
      )
    }
  }

  results <- do.call(rbind, all_results)

  if (is.null(results) || nrow(results) == 0) {
    message("No results produced.")
    return(invisible(NULL))
  }

  # Drop (with a warning) any species not yet registered as taxa -- a
  # foreign key violation would otherwise reject the whole insert.
  known_taxa <- DBI::dbGetQuery(con, "SELECT pk_taxonid FROM taxa;")[, 1]
  unknown <- setdiff(unique(results$fk_taxonid), known_taxa)
  if (length(unknown) > 0) {
    warning(
      length(unknown), " detected species are not yet registered in the taxa table and were dropped: ",
      paste(unknown, collapse = ", "),
      ". Run Register_BirdNET_Species.R to add them, then re-run birdsDetect().",
      call. = FALSE
    )
    results <- results[results$fk_taxonid %in% known_taxa, ]
  }

  if (nrow(results) == 0) {
    message("Nothing left to insert after dropping unregistered species.")
    return(invisible(NULL))
  }

  if (!dbInsert) {
    rownames(results) <- NULL
    return(results)
  }

  # Skip anything already in modeloutputs for this model+file+species+time,
  # so re-running birdsDetect() on the same recordings is a no-op for
  # anything already stored (mirrors scoresDetect's dedup-before-insert).
  existing <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT fk_mediaid, fk_taxonid, x_min, x_max FROM modeloutputs WHERE fk_modelid = ", modelID,
      " AND fk_mediaid IN (", paste(unique(results$fk_mediaid), collapse = ", "), ");"
    )
  )
  if (nrow(existing) > 0) {
    dup_key <- function(df) paste(df$fk_mediaid, df$fk_taxonid, df$x_min, df$x_max, sep = "|")
    results <- results[!(dup_key(results) %in% dup_key(existing)), ]
  }

  if (nrow(results) == 0) {
    message("All results already exist in modeloutputs; nothing new to insert.")
    return(invisible(NULL))
  }

  rownames(results) <- NULL
  DBI::dbAppendTable(con, "modeloutputs", results)
  message("Inserted ", nrow(results), " new modeloutputs rows.")
  invisible(NULL)
}



