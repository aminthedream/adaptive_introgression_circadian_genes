#This script processes genetic segment data to identify high-probability archaic donor segments, focusing on segments that match archaic samples such as Neanderthals (e.g., Altai Neanderthal, Chagyrskaya, Vindija) and Denisovans. 
#The goal is to select segments with a high likelihood of archaic origin based on specific criteria, such as the number of matched lines within each segment and predefined ratios of "match" entries. 
#The results are saved as individual files for each high-probability segment.




data <- list.files(pattern = "\\.ndmatch$", full.names = TRUE)
data_list <- list()

for (file_path in data) {
  df <- read.table(file_path, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("Chromosome", "Position", "ID", "Reference", "Alternative", 
                    "Segment", "Allele", "Score", "ALTAI", "CHAGYRSKAYA", 
                    "VINDIJA", "DENISOVA")
  data_list[[file_path]] <- df
}

# Verify the 'Segment' column exists and contains non-empty values
for (file_path in names(data_list)) {
  if (!("Segment" %in% names(data_list[[file_path]]))) {
    stop("Segment column not found in file ", file_path)
  }
  if (any(is.na(data_list[[file_path]]$Segment)) || all(data_list[[file_path]]$Segment == "")) {
    stop("Segment column is empty or contains missing values in file ", file_path)
  }
}


segmented_list <- lapply(data_list, function(df) split(df, df$Segment))

for (file_path in names(segmented_list)) {
  segment_list <- segmented_list[[file_path]]
  population_name <- sub("\\.ndmatch$", "", basename(file_path), ignore.case = TRUE)

  for (segment_id in names(segment_list)) {
    segment_data <- segment_list[[segment_id]]
    num_lines <- nrow(segment_data)

    # Assess segment based on line count
    if (num_lines > 29) {
      result <- "pass"
    } else {
      result <- "fail"
    }
    cat("File", file_path, "- Segment", segment_id, "- Number of lines:", num_lines, "- Result:", result, "\n")

    # Filter data where 'DENISOVA' is not 'notcomp'
    pass_data <- segment_data[segment_data$DENISOVA != "notcomp", ]
    num_match <- sum(pass_data$DENISOVA == "match")
    num_mismatch <- sum(pass_data$DENISOVA == "mismatch")
    total_entries <- num_match + num_mismatch

    # Calculate ratios and apply conditions
    if (!is.na(total_entries) && total_entries != 0) {
      ratio <- num_match / total_entries

      # Filter data where 'ALTAI' is not 'notcomp'
      pass_data_neanderthal <- segment_data[segment_data$ALTAI != "notcomp", ]
      num_match2 <- sum(pass_data_neanderthal$ALTAI == "match")
      num_mismatch2 <- sum(pass_data_neanderthal$ALTAI == "mismatch")
      total_entries2 <- num_match2 + num_mismatch2

      if (!is.na(total_entries2) && total_entries2 != 0) {
        ratio2 <- num_match2 / total_entries2

        # Criteria: line count > 29, DENISOVA match ratio < 0.4, ALTAI match ratio > 0.6
        if (num_lines > 29 && ratio < 0.4 && ratio2 > 0.6) {
          cat("Segment", segment_id, "- Passed both conditions\n")
          cat("Match (DENISOVA):", num_match, " Mismatch (DENISOVA):", num_mismatch, " Ratio (DENISOVA):", ratio, "\n")
          cat("Match (ALTAI):", num_match2, " Mismatch (ALTAI):", num_mismatch2, " Ratio (ALTAI):", ratio2, "\n\n")

          file_name <- paste0(population_name, "_ALTAI_", segment_id, ".donorseg")
          write.table(pass_data, file = file_name, sep = "\t", row.names = FALSE, quote = FALSE)
          cat("Saved as:", file_name, "\n\n")
        }
      }
    }
  }
}

