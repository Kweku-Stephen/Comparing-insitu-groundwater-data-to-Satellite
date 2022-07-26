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
         data[ ,col] <- as.POSIXct(data[ ,col], format = "%m/%d/%Y %H:%M"),
         data[ ,col] <- as.POSIXct(data[ ,col], format = "%d/%m/%Y")
  )
  # Returning output
  return(data)
}



# Converting all datetime-like character class of each element of the list to date/date-time class
Stations |>  
  lapply(
    date_time_conv, 
    col = "Timestamps"
  ) |> 
  lapply(
    # Function to loop over each element of the input data; which is a list
    \(data = "") if(ncol(data) > 3) {
      data[grep("Timestamps|rr|rh|sm|st", colnames(data), value = TRUE, perl = TRUE)]
    } else data
  ) -> Stations



# checking the class of variables in every element of the list "Stations"
# we only want date-times and numerics
ischaracter <- function(data = "") sapply(data, is.character)

# Looping the above function on all elements of the list "Stations"
lapply(
  Stations, \(data) any(ischaracter(data))
)

# Targeting only the element "Varenpare.csv" because its the only variable to return TRUE for the 
    #condition above
lapply(
  Stations[["Varenpare.csv"]],
  
  \(vec = "") {
    if (mode(vec) %in% "character") as.numeric(gsub("\\*|^\\s*$", NA, vec))
    else vec
  }
) |> 
  cbind.data.frame()-> Stations[["Varenpare.csv"]]


# lapply(
#   Stations[["Varenpare.csv"]],
#   \(vec = "") if (!isa(vec, what = "character")) gsub("\\*|^\\s*$", NA, vec) else vec
# )







# if your computer/hardware is constrained bcos you are using a single thread/core, you can 
    # uncheck the code chunck below make use of the multi-core properties of your machine by 
    # tweaking certain parameters such as logical argument and the workhorse function: clusterApply.

#require(magrittr)
# # Initializing nodes (4 workers) using the Socket backend connection
# cl <- parallel::makeCluster(
#   spec = parallel::detectCores(logical = FALSE), 
#   type = "PSOCK"
# )
# 
# # Evaluating libraries and functions on all nodes
# parallel::clusterEvalQ(
#   # cluster of 4 nodes
#   cl,
#   #Evaluating the conversion to date and date-time function on all nodes
#   date_time_conv <- function(data = "", col = ""){
#     # condition for checking class of "col"
#     if(class(col) != "character") stop("col must be a character")
#     # condition for transforming dates-like objects to date ans date-time object in R
#     ifelse(any(grep("\\:", data[ ,"Timestamps"])) == TRUE,
#            data[ ,col] <- as.POSIXct(data[ ,col], format = "%m/%d/%y %H:%M:%S"),
#            data[ ,col] <- as.POSIXct(data[ ,col], format = "%d/%m/%y")
#     )
#     # Returning output
#     return(data)
#   }
# )

# # Exporting data objects to the global environment of all 4 nodes
# parallel::clusterExport(cl, "Stations")
# 
# # Running the function date_time_conv in parallel on all datasets in the list "Stations"
# Stations %<>% parallel::clusterApply(
#   cl, 
#   ., 
#   date_time_conv, 
#   col = "Timestamps"
# )
# 
# # Renaming all elements of the output
# Stations %<>% 
#   setNames(dir(path = "New folder/"))


# # stop cluster
# parallel::stopCluster(cl)


# Computing mean Rainfall, soil moisture, relative humidity and Soi Temperature conditioned by hours####








