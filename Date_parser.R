
# Convert character dates to proper Date format if not already Date
parse_if_needed <- function(date_col) {
  # If already Date, return as-is
  if (inherits(date_col, "Date")) {
    return(date_col)
  }
  
  # If character and contains slashes (likely d/m/Y or m/d/Y), try parsing
  if (is.character(date_col) || is.factor(date_col)) {
    date_col <- as.character(date_col)  # for safety
    
    if (all(grepl("/", date_col))) {
      # Try day/month/year first (commonly used in NZ exports)
      parsed <- as.Date(date_col, format = "%d/%m/%Y")
      
      # If conversion fails (NA introduced), try month/day/year
      if (any(is.na(parsed))) {
        parsed <- as.Date(date_col, format = "%m/%d/%Y")
      }
      
      return(parsed)
    }
    
    # Otherwise, try automatic ISO format (e.g., "2024-01-14")
    return(as.Date(date_col))
  }
  
  stop("Unrecognized date format or column type")
}

