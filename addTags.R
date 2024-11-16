# R Script: Tagging CSV Files with Excel Data for Russian Dictionary Project
# Author: Granite Ogborn https://github.com/GraniteOgborn
# Date: 04/10/2024
# Description: This script is part of the branch of the Russian Dictionary project hosted at https://github.com/Badestrand/russian-dictionary.
#              It reads the tab-delimited CSV files which make up the dictionary, adds tags based on corresponding Excel files,
#              and exports the results to separate tab-delimited CSV files.

# Load libraries
library(readxl)
library(dplyr)

# Function to add tags to CSV files based on Excel contents
add_tags <- function(csv_data, xlsx_data, xlsx_file_name) {
  # Extract file name without extension
  file_name <- gsub(".xlsx", "", basename(xlsx_file_name))
  
  # Create tags in Excel data
  xlsx_tags <- xlsx_data %>%
    group_by(Word) %>%
    reframe(
      tags = paste0(file_name, "|", gsub(" ", "", unique(Chapter)))
    ) %>%
    ungroup()  # Ungroup the data frame to avoid warning
  
  # Merge Excel tags with CSV data based on 'bare' column
  merged_data <- left_join(csv_data, xlsx_tags, by = c("bare" = "Word")) %>%
    mutate(tags = ifelse(is.na(tags), "", gsub(" ", "", tags)))
  
  return(merged_data)
}


# File paths for the CSV files
csv_files <- c("nouns_cleaned.csv", "verbs_cleaned.csv", "adjectives_cleaned.csv", "others_cleaned.csv")

# Read Excel files and process CSV files
xlsx_files <- c("Goloca Book 1.xlsx", "Goloca Book 2.xlsx")
for (csv_file in csv_files) {
  # Read the CSV file
  csv_data <- read.delim(csv_file, sep = "\t", na.strings = "", fileEncoding = "UTF-8")
  
  # Read Excel files
  xlsx_data <- lapply(xlsx_files, read_excel)
  
  # Add tags from each Excel file to the CSV data
  tagged_data <- lapply(seq_along(xlsx_files), function(i) add_tags(csv_data, xlsx_data[[i]], xlsx_files[i]))
  
  # Combine tags from all Excel files
  final_data <- bind_rows(tagged_data) %>%
    group_by(bare) %>%
    summarize(tags = paste(gsub(" ", "", tags), collapse = " "), .groups = "drop")
  
  # Merge final data with original CSV data to include all columns
  final_data <- left_join(csv_data, final_data, by = "bare")
  
  # Write final data with tags to a new CSV file
  write.table(final_data, file = gsub(".csv", "_tagged.csv", csv_file), sep = "\t", row.names = FALSE, col.names = TRUE, na = "", fileEncoding = "UTF-8")
  
  # Print message indicating completion
  message("Tags added to:", csv_file)
}
