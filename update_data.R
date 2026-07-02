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
  library(dplyr)    # Data manipulation (Using modern >= v1.1.0 paradigms)
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

#' Apply the ND/2 Protocol and Standardize Target Columns
apply_nd_protocol <- function(df) {
  # 1. Standardize core column names safely
  if ("pfas" %in% names(df))      df <- rename(df, substance_name = pfas)
  if ("substance" %in% names(df)) df <- rename(df, substance_name = substance)
  if ("cas" %in% names(df))       df <- rename(df, cas_rn = cas)
  if ("cas_rn_s" %in% names(df))  df <- rename(df, cas_rn = cas_rn_s)
  
  df %>%
    mutate(
      # 2. Trim whitespace on target strings
      across(
        any_of(c("substance_name", "cas_rn")),
        \(x) str_trim(as.character(x))
      ),
      # 3. Apply ND/2 calculation ONLY to concentration columns (Protecting metadata)
      across(
        matches("ng_l|ng_g|ug_l|mg_kg"),
        \(col) {
          val_str <- str_trim(str_to_upper(as.character(col)))
          
          # Boolean flags for condition routing
          is_pure_nd <- !is.na(val_str) & (val_str %in% c("ND", "N/D", "NON-DETECT", "NON DETECT"))
          is_lt_nd   <- !is.na(val_str) & str_starts(val_str, "<")
          
          # Strip all non-numeric characters (except scientific notation/decimals)
          num_val <- suppressWarnings(
            as.numeric(str_remove_all(str_replace_all(val_str, ",", ""), "[^0-9eE+.-]"))
          )
          
          # Modern case_when utilizing .default as a named argument (dplyr 1.1.0+)
          case_when(
            is_pure_nd ~ 0,
            is_lt_nd   ~ num_val / 2,
            .default   = num_val
          )
        }
      )
    )
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
    
    # Alignment Fix: If CAS is missing, verify the shifted cell doesn't look like a CAS before padding
    is_missing_cas <- length(cells) == (expected_cols - 1) && cas_idx > 0
    if (is_missing_cas && !str_detect(cells[cas_idx], "^\\d+-\\d{2,}-\\d$")) {
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
  
  # Base Polish: Clean nomenclature, drop UI indexes
  df_clean <- processed_rows %>%
    mutate(across(everything(), \(x) na_if(x, "Not Measured"))) %>%
    rename_with(sanitize_headers) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows", "cols"))
  
  # Conditionally apply mathematical ND/2 only if the table contains concentration metrics
  if (any(str_detect(names(df_clean), "ng_l|ng_g|ug_l|mg_kg"))) {
    df_clean <- apply_nd_protocol(df_clean)
  }
  
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
message(sprintf("Step 2: Found %d tables. Parsing, aligning, and processing...", length(table_nodes)))

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

# Capture the exact time of the scrape
scrape_timestamp <- format(Sys.Date(), "%B %d, %Y")

# Dual-Export Loop (Processing all tables to maintain a complete archive)
iwalk(all_tables_clean, function(df, table_name) {
  
  # Embed the timestamp into the dataframe as a hidden attribute
  attr(df, "scrape_date") <- scrape_timestamp
  
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
message(sprintf("All pre-processed tables are now available in '%s' for Shinylive.", CONFIG$dir_rds))