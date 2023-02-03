### AUTHOR: DM
### WRITTEN IN: R version 4.2.2
### Purpose: extract EJI codebook from the EPA documentation PDF


# SET-UP -----------------------------------------------------------------------
library(tidyverse) 
library(docxtractr) # extract tables from docx
library(reticulate) # pdf to docx
library(writexl)    # exports to xlsx file

# FUNCTION DEFINITIONS ---------------------------------------------------------
# This function combines rows in the codebook that are split between 
# two pages in the technical documentation PDF. When extracting the table from
# word, rows split across pages are interpreted as two separate rows, when they 
# should just be a single row. Here, we combine information from the mostly-empty
# row (i) with the rest of the information in the above row (i-1). 
row_merge <- function(data) {
  # Get row indices for rows (i) that need to be merged with row above (i-1)
  rows_to_merge <- which(!complete.cases(data))
  
  # Loop through the indexed rows
  for (i in rows_to_merge) {
    # Loop over each column for each indexed row
    for (j in 1:ncol(data)) {
      # Merge row i with row i-1 (for each column, j); don't add space when merging URLs
      if (!is.na(data[i, j])) {
        if (grepl("https", data[i - 1, j])) {
          data[i - 1, j] <- paste0(data[i - 1, j], data[i, j])
          
        } else {
          data[i - 1, j] <-
            paste0(data[i - 1, j], " ", data[i, j])
        }
      }
    }
  }
  
  # Drop the extra rows (since data in these rows is now merged into the appropriate row)
  data <- data[-rows_to_merge, ]
  
  return(data)
} 

# DATA CLEANING ----------------------------------------------------------------
# Read in the docx file
doc <- read_docx("files/input/EJI-2022-Documentation.docx")

# Create a codebook
codebook <-
  docx_extract_all_tbls(docx = doc,
                        guess_header = TRUE,
                        trim = TRUE)[8:28] %>%
  map(., ~ .x[-1, ]) %>%
  reduce(., bind_rows) %>% 
  mutate(across(everything(), ~ na_if(., "")))

# Vector of new column names
names <-
  c(
    "variable_name",
    "description",
    "module",
    "domain",
    "data_source",
    "table_field_calc",
    "notes"
  )

# Assign column names
colnames(codebook) <- names

# Clean-up the messy rows
codebook <- row_merge(data = codebook)

# DATA EXPORTS -----------------------------------------------------------------
# Simple codebook export
write_csv(codebook, "files/output/eji_codebook.csv", na = "")