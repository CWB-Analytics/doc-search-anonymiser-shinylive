# HELPER FUNCTIONS ####
upload_original_data <- function(selected_file) {
    
    if (file.size(selected_file) == 0) {
        stop("The selected file is empty and contains no data. Please upload alternative file")
    }
    
    original_data <- read.csv(file = selected_file, check.names = TRUE)
    
    if (nrow(original_data) == 0) {
        stop("The selected file contains column names but no data rows. Please upload alternative file")
    }
    
    text_cols <- sapply(original_data, is.character)
    
    if (sum(text_cols) < 2) {
        stop("The selected file only contains one valid column for matching. Please upload alternative file")
    }
    
    return(original_data)
}

create_name_keys <- function(original_names, col_name) {
    
    original_names[is.na(original_names)] <- ""
    
    step_split_case    <- gsub("([a-z])([A-Z])", "\\1 \\2", original_names)
    step_split_hyphen  <- gsub("-", " ", step_split_case)
    step_lower         <- tolower(step_split_hyphen)
    step_no_quotes     <- gsub("\\s*['\"].*?['\"]\\s*", "", step_lower)
    step_no_punct      <- gsub("[[:punct:]]", "", step_no_quotes)
    step_trimmed       <- trimws(step_no_punct)
    step_single_spaces <- gsub("\\s+", " ", step_trimmed)
    standardised_names <- gsub("(?i)^(master|mrs|mr|miss|ms|dr)\\s+",
                               "", step_single_spaces)
    
    if (!is.null(col_name)) {
        placeholder <- paste0("x ", col_name)
        standardised_names[standardised_names == ""] <- placeholder
    }
    
    words <- strsplit(standardised_names, "\\s+")
    
    first_initial <- vapply(words, function(x)
        substr(x[1], 1, 1), FUN.VALUE = character(1))
    
    last_word <- vapply(words, function(x) 
        x[length(x)], FUN.VALUE = character(1))
    
    return(paste0(first_initial, last_word))
}

calculate_distance_between_name_key_pairs <- function(df, patient_col, sender_col) {
    distances <- stringdist::stringdist(df[[patient_col]], df[[sender_col]], method = "osa")
    df$distance <- distances
    return(df)
}

filter_by_threshold <- function(df, threshold) {
    df[df$distance >= threshold, , drop = FALSE]
}

download_csv_js <- function() {
  tags$head(
    tags$script(
      HTML(
        "
        function downloadCSV(csvText, filename) {
          const blob = new Blob([csvText], { type: 'text/csv;charset=utf-8;' });
          const url = URL.createObjectURL(blob);

          const link = document.createElement('a');
          link.setAttribute('href', url);
          link.setAttribute('download', filename);

          document.body.appendChild(link);
          link.click();

          document.body.removeChild(link);
          URL.revokeObjectURL(url);
        }

        Shiny.addCustomMessageHandler('downloadCSV', function(message) {
          downloadCSV(message.csv, message.filename);
        });
        "
      )
    )
  )
}