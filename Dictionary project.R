# R Script: CSV Data Cleaning and Processing for Russian Dictionary Project
# Author: Granite Ogborn https://github.com/GraniteOgborn
# Date: 04/10/2024
# Description: This script is part of my branch of the Russian Dictionary project hosted at https://github.com/Badestrand/russian-dictionary. 
#              It reads the tab-delimited CSV files which make up the dictionary, cleans them by replacing and removing characters, 
#              filters out words with no English translation, and exports the results to separate tab-delimited CSV files.

# Load libraries
library(tidyverse)

# Clean the data
clean_data <- function(data) {
  # Replace stress marks and semicolons, remove hyphens and double spaces
  data <- data %>%
    mutate_all(~ gsub("'", "́", .)) %>%
    mutate_all(~ gsub(";", ",", .)) %>%
    mutate_all(~ gsub("^-$", "", .)) %>%
    mutate_all(~ gsub(",", ", ", .)) %>%
    mutate_all(~ gsub("  ", " ", .)) 
  
  # Remove excess parentheses
  if("decl_f_inst" %in% colnames(data)){
    data$decl_f_inst <- gsub("[()]", "", data$decl_f_inst)
  }
  
  #Replace Boolean fields with words
  if("gender" %in% colnames(data)){
    data$gender <- ifelse(data$gender == "m", "male", 
                   ifelse(data$gender == "f", "female",
                   ifelse(data$gender == "n", "neuter", data$gender)))
  }
  if("animate" %in% colnames(data)){
    data$animate <- ifelse(data$animate == 1, "animate", "inanimate")
  }
  if("indeclinable" %in% colnames(data)){
    data$indeclinable <- ifelse(data$indeclinable == 1, "indeclinable, ", "")
  }
  if("sg_only" %in% colnames(data)){
    data$sg_only <- ifelse(data$sg_only == 1, "singular only", "")
  }
  if("pl_only" %in% colnames(data)){
    data$pl_only <- ifelse(data$pl_only == 1, "plural only", "")
  }
  
  # Remove NA cells
  data[is.na(data)] <- ""
  
  # Remove 'translations_de' column if it exists
  if ("translations_de" %in% colnames(data)) {
    data <- data %>% select(-translations_de)
  }
  
  # Rename 'translations_en' column to 'translations'
  colnames(data)[colnames(data) == "translations_en"] <- "translations"

  # Add numerical suffix to duplicate values in the 'bare' column
  data <- data %>%
    group_by(bare) %>%
    mutate(duplicate_count = row_number() - 1) %>%
    mutate(bare = ifelse(duplicate_count > 0, paste0(bare, duplicate_count), bare)) %>%
    ungroup() %>%
    select(-duplicate_count)
  
    return(data)
}

# File paths for the CSV files
csv_files <- c(
  "nouns.csv",
  "verbs.csv",
  "adjectives.csv",
  "others.csv"
)

# Iterate over each CSV file
for (csv_file in csv_files) {
  # Check if file exists
  if (!file.exists(csv_file)) {
    message("File not found:", csv_file)
    next
  }
  
  # Read the CSV file
  data <- read.delim(csv_file, sep = "\t", na.strings = "", strip.white = TRUE)
  
  # Clean it
  cleaned_data <- clean_data(data)
  
  # Filter out rows with no English translation
  cleaned_data <- cleaned_data %>% filter(translations != "")
  
  # Export modified data
  write.table(cleaned_data, file = paste0(gsub(".csv", "_cleaned.csv", csv_file)), sep = "\t", row.names = FALSE, col.names = TRUE)
  
  # Count the number of rows in the new file
  num_rows <- nrow(cleaned_data)
  
  # Count the number of duplicate rows that were edited
  num_duplicates <- sum(duplicated(data$bare))
  
  # Print processing message
  message(csv_file, " was successfully processed into ", gsub(".csv", "_cleaned.csv", csv_file), " with ", num_rows, " rows and ", num_duplicates, " duplicates.")
}


