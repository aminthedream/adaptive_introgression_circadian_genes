#This script identifies and outputs genomic segments likely to represent adaptive introgression based on a set of criteria. 
#For each .subset file, the script splits the data by segment, filters segments based on the number of lines and match/mismatch ratios, and then saves segments that meet specific criteria. 
#This approach helps pinpoint regions where introgression is likely adaptive.

# List all files with .subset extension
data <- list.files(pattern = "\\.subset$", full.names = TRUE)

# Load data from each file into a list of data frames
data_list <- lapply(data, function(file) {
  read.table(file, header = TRUE, stringsAsFactors = FALSE)
})

# Split each data frame by segment (column V8)
segmented_list <- lapply(data_list, function(df) split(df, df$V8))

# Process each segment to locate adaptive introgression regions
for (data_index in seq_along(segmented_list)) {
  segment_list <- segmented_list[[data_index]]
  population_name <- sub("\\.subset$", "", basename(data[data_index]), ignore.case = TRUE)

  for (segment_id in names(segment_list)) {
    segment_data <- segment_list[[segment_id]]
    num_lines <- nrow(segment_data)

    # Determine if segment passes based on line count
    result <- if (num_lines > 9) "pass" else "fail"
    cat("File", data[data_index], "- Segment", segment_id, "- Number of lines:", num_lines, "- Result:", result, "\n")

    # Filter segment data where V9 is not 'notcomp'
    pass_data <- segment_data[segment_data$V9 != "notcomp", ]
    num_match <- sum(pass_data$V9 == "match")
    num_mismatch <- sum(pass_data$V9 == "mismatch")
    total_entries <- num_match + num_mismatch

    # Check conditions based on match ratio
    if (!is.na(total_entries) && total_entries != 0) {
      ratio <- num_match / total_entries

      # Apply conditions for segment pass: line count > 9 and match ratio > 0.5
      if (num_lines > 9 && ratio > 0.5) {
        cat("Segment", segment_id, "- Passed both conditions\n")
        cat("Match:", num_match, " Mismatch:", num_mismatch, " Ratio:", ratio, "\n\n")

        # Save filtered data to file
        file_name <- paste0(population_name, "_AIsegment_", segment_id, ".txt")
        write.table(pass_data, file = file_name, sep = "\t", row.names = FALSE, quote = FALSE)
        cat("Saved as:", file_name, "\n\n")
      }
    }
  }
}

