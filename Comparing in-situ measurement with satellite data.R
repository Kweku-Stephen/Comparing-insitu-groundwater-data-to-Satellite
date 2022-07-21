#############################################################################################################
# checking R version ####
if(paste(version[["major"]], version[["minor"]], sep = ".") != "4.2.0") {
  message("Update R to latest version")
} else message("R up to date")
  

# Creating Directories for output plots and data ####
for ( i in c("Data_outputs", "Plots_outputs")){
  if (!dir.exists(i)) dir.create(i) else message("Directory already Created")
} 


# Checking existence of required packages ####
intersect <- setdiff(
  c("archive", "rio", "ggplot2", "magrittr"), # Poulate this vector with required packages
  installed.packages()
) 
if (length(intersect) >= 1) {
  message("installing package(s)")
  install.packages(intersect, dependencies = TRUE)#; sapply(intersect, require, character.only = TRUE)
} else message("packages already existing") # could be better


# Unziping file ####
dir(pattern = ".7z$") |> 
  archive::archive_extract() 

# # Unlinking/deleting the zipped file
# unlink(grep(".7z$", dir(), value = TRUE))


# Reading in all unarchived datasets into R ####
dir(path = "New folder/", pattern = ".csv$", full.names = TRUE) |> 
  rio::import_list() |> 
  lapply(
    \(data = "", col = "") data[!is.na(data[ ,col]), ],
    "Timestamps"
  ) |> 
  setNames(dir(path = "New folder", pattern = "csv$")) -> Stations


# function to Conversion timestamps to R date and date-time objects ####
date_time_conv <- function(data = "", col = ""){
  # condition for checking class of "col"
  if(class(col) != "character") stop("col must be a character")
  # condition for transforming dates-like objects to date ans date-time object in R
  ifelse(any(grep("\\:", data[ ,"Timestamps"])) == TRUE,
         data[ ,col] <- as.POSIXct(data[ ,col], format = "%m/%d/%y %H:%M:%S"),
         data[ ,col] <- as.POSIXct(data[ ,col], format = "%d/%m/%y")
  )
  # Returning output
  return(data)
}

require(magrittr)
# Initializing nodes (4 workers) using the Socket backend connection
cl <- parallel::makeCluster(
  spec = parallel::detectCores(logical = FALSE), 
  type = "PSOCK"
)

# Evaluating libraries and functions on all nodes
parallel::clusterEvalQ(
  # cluster of 4 nodes
  cl,
  #Evaluating the conversion to date and date-time function on all nodes
  date_time_conv <- function(data = "", col = ""){
    # condition for checking class of "col"
    if(class(col) != "character") stop("col must be a character")
    # condition for transforming dates-like objects to date ans date-time object in R
    ifelse(any(grep("\\:", data[ ,"Timestamps"])) == TRUE,
           data[ ,col] <- as.POSIXct(data[ ,col], format = "%m/%d/%y %H:%M:%S"),
           data[ ,col] <- as.POSIXct(data[ ,col], format = "%d/%m/%y")
    )
    # Returning output
    return(data)
  }
)

# Exporting data objects to the global environment of all 4 nodes
parallel::clusterExport(cl, "Stations")

# Running the function date_time_conv in parallel on all datasets in the list "Stations"
Stations %<>% parallel::clusterApplyLB(
  cl, 
  ., 
  date_time_conv, 
  col = "Timestamps"
)

# Renaming all elements of the output
Stations %<>% 
  setNames(dir(path = "New folder/"))


# Computing mean Rainfall, soil moisture, relative humidity and Soi Temperature ####



# stop cluster
parallel::stopCluster(cl)










