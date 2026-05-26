# ==============================================================================
# ECCC PFAS Data Engine: Universal Table Extractor & Exporter
# ==============================================================================
# Description: Scrapes, aligns, cleans, and exports PFAS wastewater data from ECCC.
# Output: Saves all tables as natively-typed .rds files for Shiny, and .csv for audit.
# ==============================================================================

# --- 1. Dependencies ----------------------------------------------------------
suppressPackageStartupMessages({
  library(httr2)    # Modern HTTP requests
  library(rvest)    # HTML scraping
  library(janitor)  # Dataframe cleaning
  library(dplyr)    # Data manipulation
  library(stringr)  # String operations
  library(purrr)    # Functional programming
  library(readr)    # CSV writing
  library(tibble)   # Modern dataframes
})

# --- 2. Configuration ---------------------------------------------------------
CONFIG <- list(
  user_agent = "PFAS-Calculator-Data-Engine (Contact: your@email.com)",
  url = "https://www.canada.ca/en/environment-climate-change/services/national-pollutant-release-inventory/report/pfas.html",
  dir_rds = "data/rds",
  dir_csv = "data/csv_archive"
)

# --- 3. Helper Functions ------------------------------------------------------

#' Sanitize scientific column headers for R dataframes
sanitize_headers <- function(col_names) {
  col_names %>%
    str_replace_all(c(
      "<\\s?" = "lt_", 
      ">\\s?" = "gt_", 
      "%"     = "_pct_"
    )) %>%
    str_replace_all("[^[:alnum:]]", "_") %>% # Swap non-alphanumeric for underscores
    str_replace_all("_+", "_") %>%           # Collapse multiple underscores
    str_remove_all("^_|_$")                  # Trim leading/trailing underscores
}

#' Extract, align, and clean a single HTML table
extract_eccc_table <- function(table_node) {
  
  rows <- table_node %>% html_elements("tr")
  
  # Guard clause: Skip empty or single-row tables
  if (length(rows) <= 1) return(tibble())
  
  # Parse the header row (catching both th and td) to establish baseline dimensions
  header_row    <- rows[[1]] %>% html_elements("th, td") %>% html_text2()
  expected_cols <- length(header_row)
  
  # Dynamically locate the CAS Registry Number column
  cas_idx <- str_which(str_to_lower(header_row), "cas")[1]
  if (is.na(cas_idx)) cas_idx <- 0
  
  # Iteratively process the body rows
  processed_rows <- map(rows[-1], function(row_node) {
    
    cells <- row_node %>% html_elements("th, td") %>% html_text2() %>% str_trim()
    
    # Guard clause: Skip JavaScript footers or empty artifacts
    if (length(cells) == 0 || str_detect(cells[1], "(?i)^showing")) {
      return(NULL)
    }
    
    # Alignment Fix: If CAS is missing (e.g., 'TFA'), pad with NA to prevent left-shift
    is_missing_cas <- length(cells) == (expected_cols - 1) && cas_idx > 0
    if (is_missing_cas && !str_detect(cells[cas_idx], "[0-9]+-[0-9]+-[0-9]")) {
      cells <- append(cells, NA_character_, after = cas_idx - 1)
    }
    
    # Enforce uniform length and convert to a named row
    length(cells) <- expected_cols
    set_names(cells, header_row) %>% as_tibble_row(.name_repair = "unique")
    
  }) %>% 
    compact() %>% 
    list_rbind()  
  
  # Guard clause: Return empty tibble if no rows were processed
  if (nrow(processed_rows) == 0) return(tibble())
  
  # Final Polish: Clean nomenclature, standardize NAs, and drop UI indexes
  df_clean <- processed_rows %>%
    mutate(across(everything(), ~ na_if(.x, "Not Measured"))) %>%
    rename_with(sanitize_headers) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows", "cols"))
  
  # Drop the "Row Number" column (Column 1) if it is entirely numeric
  if (all(str_detect(df_clean[[1]], "^[0-9]+$"), na.rm = TRUE)) {
    df_clean <- df_clean %>% select(-1)
  }
  
  return(df_clean)
}

# --- 4. Main Execution Pipeline -----------------------------------------------
message("Step 1: Connecting to ECCC and fetching HTML...")

raw_html <- request(CONFIG$url) %>%
  req_user_agent(CONFIG$user_agent) %>%
  req_throttle(rate = 1) %>%
  req_retry(max_tries = 3) %>%
  req_perform() %>%
  resp_body_string()

table_nodes <- read_html(raw_html) %>% html_elements("table")
message(sprintf("Step 2: Found %d tables. Parsing and aligning data...", length(table_nodes)))

# Process all tables, silencing upstream name-repair warnings (due to ECCC duplicates)
all_tables_clean <- suppressMessages(
  map(table_nodes, extract_eccc_table)
)

# Rename list elements sequentially (Table_1, Table_2, etc.)
names(all_tables_clean) <- paste0("Table_", seq_along(all_tables_clean))

# --- 5. File Export & Cleanup -------------------------------------------------
message("Step 3: Exporting datasets to local directories...")

# Create target directories safely
dir.create(CONFIG$dir_rds, recursive = TRUE, showWarnings = FALSE)
dir.create(CONFIG$dir_csv, recursive = TRUE, showWarnings = FALSE)

# Dual-Export Loop
iwalk(all_tables_clean, function(df, table_name) {
  
  # 1. Save as RDS (Optimized for the Shiny App)
  rds_path <- file.path(CONFIG$dir_rds, paste0(table_name, ".rds"))
  saveRDS(df, rds_path)
  
  # 2. Save as CSV (Human-readable archive for validation)
  csv_path <- file.path(CONFIG$dir_csv, paste0(table_name, ".csv"))
  write_csv(df, csv_path)
  
})

# Clean up memory pointers to prevent RStudio 'xml_child' warnings
rm(raw_html, table_nodes, all_tables_clean)

message("\n==================================================")
message(" SUCCESS: PFAS Extraction & Export Complete \u2713")
message("==================================================")
message(sprintf("All tables are now available in '%s' for your Shiny app.", CONFIG$dir_rds))