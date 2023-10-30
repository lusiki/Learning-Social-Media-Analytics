library(dplyr)
library(stringr)
library(stringi)

setDT(df)



# Words vector (for simplicity, using a shortened version)
words_vector <- as.character(cro_catoliq$name)

all <- all[SOURCE_TYPE=="web",]




batch_size <- 2000

# Calculate the number of batches
num_batches <- ceiling(nrow(all) / batch_size)

# Loop through each batch
for (i in 1:num_batches) {
  
  start_time <- Sys.time()
  # Calculate the start and end row indices for the current batch
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(all))
  
  # Print the current batch number and row indices
  cat(sprintf("Processing batch %d (rows %d to %d)...\n", i, start_idx, end_idx))
  
  # Subset the data table for the current batch and apply the operations
  all[start_idx:end_idx, `:=` (
    has_match = sapply(FULL_TEXT, function(x) any(sapply(words_vector, grepl, x))),
    matched_words = sapply(FULL_TEXT, function(x) paste(words_vector[sapply(words_vector, grepl, x)], collapse=", "))
  )]
  
  
  batch_data <- all[start_idx:end_idx]
#  assign(paste0("batch_", i), as.data.frame(batch_data))
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  
  # Print the duration for the current batch
  cat(sprintf("Batch %d processed in %f seconds.\n", i, duration))
  
 # filename <- paste0("D:/LUKA/Academic/HKS/Projekti/Dezinformacije/Data/katolički clanci/batch_", i, ".xlsx")
  
  # Write the current batch to an .xlsx file
#  write.xlsx(batch_data, filename)
  
#  cat(sprintf("Batch %d saved to %s.\n", i, filename))
  
  
}






check_matches <- function(text, words_vector) {
  any(stri_detect_fixed(text, words_vector, negate = FALSE))
}

# Loop through each batch
for (i in 1:num_batches) {
  
  start_time <- Sys.time()
  
  # Calculate the start and end row indices for the current batch
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(all))
  
  # Print the current batch number and row indices
  cat(sprintf("Processing batch %d (rows %d to %d)...\n", i, start_idx, end_idx))
  
  # Subset the data table for the current batch and apply the operations
  all[start_idx:end_idx, `:=` (
    has_match = sapply(FULL_TEXT, check_matches, words_vector),
    matched_words = sapply(FULL_TEXT, function(x) paste(words_vector[stri_detect_fixed(x, words_vector)], collapse=", "))
  )]
  
  batch_data <- all[start_idx:end_idx]
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  
  # Print the duration for the current batch
  cat(sprintf("Batch %d processed in %f seconds.\n", i, duration))
  
  # ... [rest of your loop code for saving etc.] ...
}


































