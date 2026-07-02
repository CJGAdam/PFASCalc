# ==============================================================================
#
# Purpose: PFAS Calculator to assist Wastewater Plants with NPRI Reporting
# Deployment: Shinylive - shinylive::export(".", "docs", include_files = "data/")
# Author: CJGAdam
# Criteria source: https://www.canada.ca/en/environment-climate-change/services/national-pollutant-release-inventory/report/pfas.html
# ==============================================================================
# Dependencies
# ==============================================================================
library(shiny)          # Core framework
library(bslib)          # UI components and layout
library(bsicons)        # SVG icons
library(reactable)      # Interactive data tables
library(htmltools)      # HTML construction
library(dplyr)          # Data manipulation
library(shinyWidgets)   # Enhanced UI inputs

# Base R fallback for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (is.atomic(x) && all(is.na(x))) return(y)
  x
}

# Client-side CSV download handler
# Workaround for file download in Shinylive environment
js_download_script <- tags$head(tags$script(HTML("
  $(document).on('shiny:sessioninitialized', function() {
    Shiny.addCustomMessageHandler('download_csv', function(message) {
      var blob = new Blob([message.csv_data], {type: 'text/csv;charset=utf-8;'});
      var url = URL.createObjectURL(blob);
      var a = document.createElement('a');
      a.style.display = 'none';
      a.href = url;
      a.download = message.filename;
      document.body.appendChild(a);
      a.click();
      URL.revokeObjectURL(url);
      document.body.removeChild(a);
    });
  });
")))

# ==============================================================================
# Configuration
# ==============================================================================
APP_CONFIG <- list(
  data_dir       = "data/rds/",
  limit_mld      = 10,
  threshold_kg   = 1.0,
  theme_primary  = "#2c3e50",
  ml_to_l        = 1e6,
  m3_to_l        = 1000,
  t_to_g         = 1e6,
  kg_to_g        = 1000,
  ng_to_kg       = 1e-12,
  zero_tol       = 1e-9
)

# Reference table mapping facility profiles to ECCC table columns
MATRIX_REGISTRY <- data.frame(
  treat = c(
    rep("Primary", 4),
    rep("Secondary", 5),
    rep("Tertiary / Advanced", 2)
  ),
  mix = c(
    "< 90%",
    "> 90%",
    "> 90%",
    "> 90%",
    "< 70%",
    "70% - 90%",
    "> 90%",
    "> 90%",
    "< 70%",
    "< 90%",
    "> 90%"
  ),
  solids = c(
    "Dewatering",
    "Dewatering",
    "Alkaline Treatment",
    "Anaerobic Digestion",
    "Anaerobic Digestion",
    "Anaerobic Digestion",
    "Anaerobic Digestion",
    "Dewatering",
    "Anaerobic Digestion & Pelletization",
    "Dewatering",
    "Dewatering"
  ),
  col_liq = c(
    "primary_treatment_with_lt_90_pct_residential_sources_ng_l",
    "primary_treatment_with_gt_90_pct_residential_sources_ng_l",
    "primary_treatment_with_gt_90_pct_residential_sources_ng_l",
    "primary_treatment_with_gt_90_pct_residential_sources_ng_l",
    "secondary_treatment_with_lt_70_pct_residential_sources_ng_l",
    "secondary_treatment_with_70_pct_90_pct_residential_sources_ng_l",
    "secondary_treatment_with_gt_90_pct_residential_sources_ng_l",
    "secondary_treatment_with_gt_90_pct_residential_sources_ng_l",
    "secondary_treatment_with_lt_70_pct_residential_sources_ng_l",
    "tertiary_advanced_treatment_with_lt_90_pct_residential_sources_ng_l",
    "tertiary_advanced_treatment_with_gt_90_pct_residential_sources_ng_l"
  ),
  col_sol = c(
    "primary_treatment_with_dewatering_of_solids_lt_90_pct_residential_sources_ng_g_dry_weight_basis",
    "primary_treatment_with_dewatering_of_solids_gt_90_pct_residential_sources_ng_g_dry_weight_basis",
    "primary_treatment_with_alkaline_solids_treatment_gt_90_pct_residential_sources_ng_g_dry_weight_basis_5",
    "primary_treatment_with_anaerobic_digestion_of_solids_gt_90_pct_residential_sources_ng_g_dry_weight_basis",
    "secondary_treatment_with_anaerobic_digestion_of_solids_lt_70_pct_residential_sources_ng_g_dry_weight_basis",
    "secondary_treatment_with_anaerobic_digestion_of_solids_70_pct_90_pct_residential_sources_ng_g_dry_weight_basis",
    "secondary_treatment_with_anaerobic_digestion_of_solids_gt_90_pct_residential_sources_ng_g_dry_weight_basis",
    "secondary_treatment_with_dewatering_of_solids_gt_90_pct_residential_sources_ng_g_dry_weight_basis",
    "secondary_treatment_with_anaerobic_digestion_and_pelletization_of_solids_lt_70_pct_residential_sources_ng_g_dry_weight_basis",
    "tertiary_treatment_with_dewatering_of_solids_lt_90_pct_residential_sources_ng_g_dry_weight_basis",
    "tertiary_treatment_with_dewatering_of_solids_gt_90_pct_residential_sources_ng_g_dry_weight_basis"
  ),
  label = c(
    "Primary Treatment (<90% Res)",
    "Primary Treatment (>90% Res)",
    "Primary/Alkaline (>90% Res)",
    "Primary/Anaerobic (>90% Res)",
    "Secondary/Anaerobic (<70% Res)",
    "Secondary/Anaerobic (70-90% Res)",
    "Secondary/Anaerobic (>90% Res)",
    "Secondary/Dewatering (>90% Res)",
    "Secondary/Pelletization (<70% Res)",
    "Tertiary/Dewatering (<90% Res)",
    "Tertiary/Dewatering (>90% Res)"
  ),
  stringsAsFactors = FALSE
)

# Initial UI defaults
INIT_TREAT  <- unique(MATRIX_REGISTRY$treat)[1]
INIT_MIXES  <- unique(MATRIX_REGISTRY$mix[MATRIX_REGISTRY$treat == INIT_TREAT])
INIT_SOLIDS <- unique(MATRIX_REGISTRY$solids[
  MATRIX_REGISTRY$treat == INIT_TREAT &
    MATRIX_REGISTRY$mix == INIT_MIXES[1]
])

# ==============================================================================
# Core Functions
# ==============================================================================

# Formats numeric values to a specified number of significant figures
fmt_sigfigs <- function(v, sig_figs) {
  if (is.na(v) || v == 0) return("0")
  sub(
    "\\.$",
    "",
    formatC(
      signif(v, sig_figs),
      digits = sig_figs,
      format = "fg",
      flag = "#"
    )
  )
}

# Standardizes column names and applies the ND/2 protocol for non-detect strings
# ND/2 is from the EC guidance
clean_pfas <- function(df) {
  names(df) <- tolower(gsub("[^[:alnum:]_]", "_", names(df)))
  
  if ("pfas" %in% names(df))      names(df)[names(df) == "pfas"] <- "substance_name"
  if ("substance" %in% names(df)) names(df)[names(df) == "substance"] <- "substance_name"
  if ("cas" %in% names(df))       names(df)[names(df) == "cas"] <- "cas_rn"
  if ("cas_rn_s" %in% names(df))  names(df)[names(df) == "cas_rn_s"] <- "cas_rn"
  
  df |>
    dplyr::mutate(
      dplyr::across(-dplyr::any_of(c("substance_name", "cas_rn")), \(val) {
        val_str <- trimws(toupper(as.character(val)))
        
        is_pure_nd <- !is.na(val_str) & (
          val_str %in% c("ND", "N/D", "NON-DETECT", "NON DETECT")
        )
        
        is_lt_nd <- !is.na(val_str) & startsWith(val_str, "<")
        
        # Keep digits, decimal points, scientific notation, plus signs, minus signs,
        # and commas. Hyphen is placed at the end of the character class to avoid
        # invalid regex range errors.
        num_val <- suppressWarnings(
          as.numeric(gsub(",", "", gsub("[^0-9eE+.-]", "", val_str)))
        )
        
        dplyr::case_when(
          is_pure_nd ~ 0,
          is_lt_nd   ~ num_val / 2,
          TRUE       ~ num_val
        )
      }),
      dplyr::across(
        dplyr::any_of(c("substance_name", "cas_rn")),
        \(val) trimws(as.character(val))
      )
    )
}

# Loads static RDS files and resolves relative paths for virtual environments
load_app_data <- function() {
  possible_paths <- unique(c(
    APP_CONFIG$data_dir,
    paste0("/", APP_CONFIG$data_dir),
    "./data/rds/",
    "data/rds/"
  ))
  
  existing_paths <- possible_paths[dir.exists(possible_paths)]
  dir_path <- existing_paths[1] %||% NA_character_
  
  if (is.na(dir_path)) {
    return(list(
      status = "Missing Data Directory",
      targets = data.frame(
        substance_name = character(),
        cas_rn = character(),
        stringsAsFactors = FALSE
      ),
      eff_spec = data.frame(),
      bio_spec = data.frame(),
      scrape_date = "N/A"
    ))
  }
  
  # Read the raw Table 1 file to extract the embedded date
  raw_table_1 <- readRDS(file.path(dir_path, "Table_1.rds"))
  
  # Extract the embedded date attribute (fallback to "Current ECCC Baseline" if missing)
  embedded_date <- attr(raw_table_1, "scrape_date") %||% "Current ECCC Baseline"
  
  list(
    targets     = clean_pfas(raw_table_1),
    eff_spec    = clean_pfas(readRDS(file.path(dir_path, "Table_7.rds"))),
    bio_spec    = clean_pfas(readRDS(file.path(dir_path, "Table_8.rds"))),
    scrape_date = embedded_date,
    status      = "Loaded"
  )
}

# Resolves matrix column names robustly.
# Handles the historical "anerobic" typo and future-corrected "anaerobic" spelling.
resolve_matrix_col <- function(target_df, requested_col) {
  if (
    is.null(target_df) ||
    !is.data.frame(target_df) ||
    is.null(requested_col) ||
    length(requested_col) == 0 ||
    is.na(requested_col)
  ) {
    return(NA_character_)
  }
  
  requested_col <- requested_col[1]
  
  if (requested_col %in% names(target_df)) {
    return(requested_col)
  }
  
  alt_col <- requested_col
  
  if (grepl("anerobic", alt_col, fixed = TRUE)) {
    alt_col <- gsub("anerobic", "anaerobic", alt_col, fixed = TRUE)
  } else if (grepl("anaerobic", alt_col, fixed = TRUE)) {
    alt_col <- gsub("anaerobic", "anerobic", alt_col, fixed = TRUE)
  }
  
  if (!identical(alt_col, requested_col) && alt_col %in% names(target_df)) {
    return(alt_col)
  }
  
  normalize_col <- function(x) {
    y <- tolower(x)
    y <- gsub("anerobic", "anaerobic", y, fixed = TRUE)
    y <- gsub("[^[:alnum:]_]", "_", y)
    y
  }
  
  requested_norm <- normalize_col(requested_col)
  target_norms <- normalize_col(names(target_df))
  match_idx <- which(target_norms == requested_norm)
  
  if (length(match_idx) > 0) {
    return(names(target_df)[match_idx[1]])
  }
  
  NA_character_
}

# Executes mass balance calculations for a given waste stream: liquid or solid
calc_stream <- function(base_df, target_df, reg_row, volume, unit, type) {
  
  safe_return <- function(msg, col_status = "ERR") {
    base_df |>
      dplyr::select(cas_rn) |>
      dplyr::mutate(
        conc         = NA_real_,
        mass_kg      = 0,
        res_col      = col_status,
        trace_vol    = NA_real_,
        trace_unit   = NA_character_,
        trace_conc   = NA_real_,
        trace_source = NA_character_,
        trace_err    = msg
      )
  }
  
  if (!is.data.frame(reg_row) || nrow(reg_row) == 0) {
    return(safe_return("Invalid registry configuration."))
  }
  
  if (is.null(target_df) || !is.data.frame(target_df) || nrow(target_df) == 0) {
    return(safe_return("Source matrix missing.", "N/A"))
  }
  
  requested_col <- if (type == "liq") {
    reg_row[["col_liq"]][1]
  } else {
    reg_row[["col_sol"]][1]
  }
  
  audit_label <- reg_row[["label"]][1] %||% "Unknown matrix"
  
  target_col <- resolve_matrix_col(target_df, requested_col)
  
  if (is.na(target_col) || !(target_col %in% names(target_df))) {
    return(safe_return(
      sprintf("Matrix column unavailable: %s", requested_col %||% "NULL"),
      "N/A"
    ))
  }
  
  safe_vol <- if (
    is.null(volume) ||
    length(volume) == 0 ||
    is.na(volume)
  ) {
    0
  } else {
    volume
  }
  
  unit_conv <- if (type == "liq") {
    if (unit == "ML/y") APP_CONFIG$ml_to_l else APP_CONFIG$m3_to_l
  } else {
    if (unit == "t/y") APP_CONFIG$t_to_g else APP_CONFIG$kg_to_g
  }
  
  target_df |>
    dplyr::select(cas_rn, conc = dplyr::all_of(target_col)) |>
    dplyr::mutate(
      mass_kg = dplyr::if_else(
        is.na(conc),
        0,
        safe_vol * unit_conv * conc * APP_CONFIG$ng_to_kg
      ),
      res_col      = target_col,
      trace_vol    = safe_vol,
      trace_unit   = unit,
      trace_conc   = conc,
      trace_source = audit_label,
      trace_err    = NA_character_
    )
}

# ------------------------------------------------------------------------------
# UI Building
# ------------------------------------------------------------------------------

lbl_with_tt <- function(icon_name, text, tt_text) {
  tags$div(
    class = "d-flex align-items-center gap-1 mb-1 mt-2",
    tags$label(
      HTML(paste(bs_icon(icon_name, class = "me-1 text-primary"), text)),
      class = "fw-semibold mb-0 d-block small"
    ),
    tooltip(
      bs_icon("info-circle", class = "text-muted opacity-75", size = "0.8rem"),
      tt_text
    )
  )
}

dense_metric_card <- function(title, value, subtext, icon_name, bg_class) {
  tags$div(
    class = sprintf(
      "d-flex align-items-center p-3 rounded shadow-sm text-white h-100 %s bg-gradient",
      bg_class
    ),
    tags$div(class = "me-3 ms-1 opacity-75", bs_icon(icon_name, size = "2rem")),
    tags$div(
      class = "d-flex flex-column justify-content-center",
      tags$span(class = "text-uppercase fw-bold text-white-50 small lh-1 mb-1", title),
      tags$span(class = "fs-4 fw-bold lh-1 mb-1", value),
      tags$span(class = "text-white-50 text-truncate small lh-1", subtext)
    )
  )
}

# Pre-load data to make `app_data` available to UI components at startup
app_data <- load_app_data()

# ==============================================================================
# UI Modules
# ==============================================================================

mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    width = 360,
    padding = "1rem",
    bg = "#f8f9fa",
    resizable = FALSE,
    collapse = FALSE,
    
    accordion(
      id = ns("sidebar_wizard"),
      multiple = TRUE,
      open = c(ns("step1"), ns("step2"), ns("step3")),
      
      accordion_panel(
        title = tags$span(
          class = "fw-bold text-primary small text-uppercase",
          "1. Reporting Scope"
        ),
        value = ns("step1"),
        icon = bs_icon("diagram-3", class = "text-primary"),
        
        lbl_with_tt("pin-map", "Facility Name", "Enter the site name for export documentation."),
        textInput(ns("site_name"), NULL, "e.g., North End WWTP", width = "100%"),
        
        lbl_with_tt("database", "Data Source", "Select ECCC modeled estimates or site-specific lab data."),
        radioGroupButtons(
          ns("data_source"),
          NULL,
          choices = c("ECCC Estimates" = "eccc", "Custom Lab Data" = "lab"),
          selected = "eccc",
          justified = TRUE,
          status = "outline-primary",
          size = "sm"
        ),
        
        uiOutput(ns("data_source_help")),
        
        lbl_with_tt("signpost-split", "Active Waste Streams", "Toggle the waste streams applicable to this facility."),
        checkboxGroupButtons(
          ns("active_streams"),
          NULL,
          choices = c("Effluent" = "liq", "Biosolids" = "sol"),
          selected = c("liq", "sol"),
          justified = TRUE,
          status = "outline-primary",
          size = "sm"
        )
      ),
      
      accordion_panel(
        title = tags$span(
          class = "fw-bold text-primary small text-uppercase",
          "2. Facility Profile"
        ),
        value = ns("step2"),
        icon = bs_icon("building-gear", class = "text-primary"),
        
        lbl_with_tt("funnel", "Treatment Tier", "The highest level of liquid treatment achieved by the facility."),
        pickerInput(
          ns("treat"),
          NULL,
          choices = unique(MATRIX_REGISTRY$treat),
          selected = INIT_TREAT,
          width = "100%"
        ),
        
        lbl_with_tt("people", "Influent Mix", "The percentage of influent originating from residential sources."),
        radioGroupButtons(
          ns("res_mix"),
          NULL,
          choices = INIT_MIXES,
          selected = INIT_MIXES[1],
          justified = TRUE,
          size = "sm",
          status = "outline-primary"
        ),
        
        lbl_with_tt("layer-forward", "Solids Processing", "The primary method used to process sludge and biosolids."),
        pickerInput(
          ns("solids_process"),
          NULL,
          choices = INIT_SOLIDS,
          selected = INIT_SOLIDS[1],
          width = "100%"
        )
      ),
      
      accordion_panel(
        title = tags$span(
          class = "fw-bold text-primary small text-uppercase",
          "3. Annual Volumes"
        ),
        value = ns("step3"),
        icon = bs_icon("speedometer", class = "text-primary"),
        
        conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].indexOf('liq') > -1",
            ns("active_streams"),
            ns("active_streams")
          ),
          lbl_with_tt("droplet", "Effluent Discharge", "Total liquid volume discharged per year."),
          layout_columns(
            col_widths = c(8, 4),
            gap = "0.5rem",
            numericInput(ns("flow_val"), NULL, value = 1825, min = 0, width = "100%"),
            radioGroupButtons(
              ns("flow_unit"),
              NULL,
              choices = c("ML/y", "m³/y"),
              selected = "ML/y",
              size = "sm",
              status = "outline-primary"
            )
          )
        ),
        
        conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].indexOf('sol') > -1",
            ns("active_streams"),
            ns("active_streams")
          ),
          tags$div(
            class = "mt-2",
            lbl_with_tt("box-seam", "Biosolids Production", "Total dry weight of biosolids generated per year."),
            layout_columns(
              col_widths = c(8, 4),
              gap = "0.5rem",
              numericInput(ns("dry_tonnes"), NULL, value = 50, min = 0, width = "100%"),
              radioGroupButtons(
                ns("solids_unit"),
                NULL,
                choices = c("t/y", "kg/y"),
                selected = "t/y",
                size = "sm",
                status = "outline-primary"
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = sprintf(
            "!input['%s'] || input['%s'].length == 0",
            ns("active_streams"),
            ns("active_streams")
          ),
          tags$div(
            class = "text-muted small fst-italic text-center py-2",
            "Please select at least one active pathway in Section 1."
          )
        )
      )
    ),
    
    tags$div(
      class = "mt-auto pt-3 border-top",
      tags$div(
        class = "d-grid gap-2 mb-2",
        actionButton(
          ns("export_csv"),
          "Generate NPRI Report",
          class = "btn-success w-100 fw-bold shadow-sm py-2",
          icon = icon("file-csv")
        )
      ),
      tags$div(
        class = "text-center small text-muted",
        "Source: ECCC PFAS Guidance",
        tags$br(),
        tags$span(sprintf("Updated: %s", app_data$scrape_date), class = "opacity-75")
      )
    )
  )
}

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  navset_card_underline(
    
    nav_panel(
      "Mass Balance",
      icon = bs_icon("table"),
      
      card(
        full_screen = TRUE,
        
        card_header(
          class = "bg-light py-3 border-bottom-0 pe-5",
          
          layout_columns(
            col_widths = c(5, 7),
            class = "align-items-center mb-0",
            
            tags$div(
              class = "mb-0",
              selectizeInput(
                ns("cas_filter"),
                NULL,
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Search CAS RN..."),
                width = "100%"
              )
            ),
            
            tags$div(
              class = "d-flex align-items-center justify-content-end gap-3",
              checkboxInput(ns("hide_zero"), "Hide zero mass", value = TRUE),
              
              tags$div(
                class = "d-flex align-items-center gap-2",
                tooltip(
                  tags$span("Sig Figs ", bs_icon("info-circle"), class = "text-muted small mb-0"),
                  "Adjust precision."
                ),
                tags$div(
                  class = "mb-0",
                  numericInput(ns("sig_figs"), NULL, value = 2, min = 1, max = 8, width = "80px")
                )
              )
            )
          )
        ),
        
        card_body(
          padding = 0,
          reactableOutput(ns("main_table"))
        )
      )
    ),
    
    nav_panel(
      "Methodology",
      icon = bs_icon("journal-text"),
      
      tags$div(
        class = "p-4",
        
        tags$div(
          class = "d-flex align-items-center gap-2 mb-4",
          bs_icon("book-half", size = "1.5rem", class = "text-primary"),
          tags$h4("Compliance Methodology", class = "mb-0 text-primary fw-bold")
        ),
        
        layout_columns(
          col_widths = c(12, 12, 12),
          gap = "1.5rem",
          
          card(
            card_header(
              class = "bg-primary text-white fw-bold d-flex align-items-center gap-2",
              bs_icon("bank"),
              "Regulatory Thresholds"
            ),
            card_body(
              class = "p-4",
              layout_columns(
                col_widths = c(6, 6),
                
                tags$div(
                  class = "border-end pe-4",
                  tags$h6("Individual Mass Threshold", class = "fw-bold text-danger text-uppercase mb-3"),
                  tags$p(
                    class = "text-muted",
                    HTML("Reporting is mandatory for <strong>each individual</strong> Part 1, Group C PFAS if its specific, combined facility-wide mass is >= 1.0 kg/year.")
                  ),
                  tags$div(
                    class = "d-inline-flex align-items-center gap-2 px-3 py-2 bg-danger bg-opacity-10 rounded border border-danger mb-2",
                    bs_icon("exclamation-circle-fill", class = "text-danger"),
                    tags$h4(">= 1.0 kg/year", class = "text-danger fw-bold mb-0")
                  )
                ),
                
                tags$div(
                  class = "ps-2",
                  tags$h6("Facility Exemption", class = "fw-bold text-secondary text-uppercase mb-3"),
                  tags$p(
                    class = "text-muted",
                    "WWTPs are entirely exempt from reporting requirements if their annual average discharge is:"
                  ),
                  tags$div(
                    class = "d-inline-flex align-items-center gap-2 px-3 py-2 bg-secondary bg-opacity-10 rounded border border-secondary",
                    bs_icon("droplet-fill", class = "text-secondary"),
                    tags$h4("< 10,000 m³/day", class = "text-secondary fw-bold mb-0")
                  )
                )
              )
            )
          ),
          
          card(
            card_header(
              class = "bg-info text-white fw-bold d-flex align-items-center gap-2",
              bs_icon("calculator"),
              "Engineering Math"
            ),
            card_body(
              class = "p-4 bg-light bg-opacity-50",
              layout_columns(
                col_widths = c(8, 4),
                
                tags$div(
                  class = "border-end pe-4",
                  tags$h6("Mass Balance Equations", class = "fw-bold text-info text-uppercase mb-3"),
                  
                  tags$div(
                    class = "p-3 bg-white rounded border shadow-sm mb-3",
                    tags$p(class = "fw-bold mb-1 text-primary small", "Liquid Pathway:"),
                    tags$code(
                      "PFAS (kg) = Volume (L) * Concentration (ng/L) * 10^-12 kg/ng",
                      class = "text-dark"
                    )
                  ),
                  
                  tags$div(
                    class = "p-3 bg-white rounded border shadow-sm",
                    tags$p(class = "fw-bold mb-1 text-success small", "Solid Pathway:"),
                    tags$code(
                      "PFAS (kg) = Weight (tonnes) * 10^6 g/t * Concentration (ng/g) * 10^-12 kg/ng",
                      class = "text-dark"
                    )
                  )
                ),
                
                tags$div(
                  class = "ps-2",
                  tags$h6("Non-Detect Protocol", class = "fw-bold text-info text-uppercase mb-3"),
                  tags$div(
                    class = "d-flex align-items-center justify-content-center py-3 bg-white rounded border border-info shadow-sm text-center mb-3",
                    tags$h4("ND / 2", class = "text-info fw-bold mb-0")
                  ),
                  tags$p(
                    "Values below the limit of quantification, for example '< 0.5', are halved for calculations.",
                    class = "small mb-0 text-muted"
                  )
                )
              )
            )
          ),
          
          card(
            card_header(
              class = "bg-dark text-white fw-bold d-flex align-items-center gap-2",
              bs_icon("shield-exclamation"),
              "By-Product Classification"
            ),
            card_body(
              class = "p-4",
              tags$p(
                "Standard NPRI reporting is subject to a >= 0.1% concentration threshold. ",
                tags$strong("This does not apply to wastewater facilities.", class = "text-danger")
              ),
              tags$p(
                "All PFAS collected in influent or generated via transformation are legally classified as ",
                tags$em("by-products"),
                ". All trace mass must be calculated regardless of concentration.",
                class = "mb-0 text-muted"
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      "Data QA/QC",
      icon = bs_icon("check2-square"),
      tags$div(class = "p-3", uiOutput(ns("diagnostic_ui")))
    )
  )
}

# ==============================================================================
# Server Modules
# ==============================================================================

mod_sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$treat, {
      req(input$treat)
      
      valid_mixes <- MATRIX_REGISTRY$mix[
        MATRIX_REGISTRY$treat == input$treat
      ] |>
        unique()
      
      updateRadioGroupButtons(
        session,
        "res_mix",
        choices = valid_mixes,
        selected = valid_mixes[1]
      )
    })
    
    observeEvent(c(input$treat, input$res_mix), {
      req(input$treat, input$res_mix)
      
      valid_solids <- MATRIX_REGISTRY$solids[
        MATRIX_REGISTRY$treat == input$treat &
          MATRIX_REGISTRY$mix == input$res_mix
      ] |>
        unique()
      
      updatePickerInput(
        session,
        "solids_process",
        choices = valid_solids,
        selected = valid_solids[1]
      )
    })
    
    output$data_source_help <- renderUI({
      req(input$data_source)
      
      if (input$data_source == "eccc") {
        tags$div(
          class = "small text-muted d-flex align-items-center gap-1 mb-2 mt-1",
          bs_icon("check2-circle", class = "text-success"),
          tags$em("Routing to ECCC Tables 7 & 8")
        )
      } else {
        tags$div(
          class = "small text-warning d-flex align-items-center gap-1 mb-2 mt-1",
          bs_icon("tools"),
          tags$em("Lab data upload in development")
        )
      }
    })
    
    return(list(
      state = reactive({
        list(
          site_name      = input$site_name,
          data_source    = input$data_source,
          active_streams = input$active_streams %||% character(0),
          treat          = input$treat,
          res_mix        = input$res_mix,
          solids_process = input$solids_process,
          flow_val       = input$flow_val,
          flow_unit      = input$flow_unit,
          dry_tonnes     = input$dry_tonnes,
          solids_unit    = input$solids_unit
        )
      }),
      export_click = reactive({
        input$export_csv
      })
    ))
  })
}

mod_dashboard_server <- function(id, sidebar_data, app_data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectizeInput(
        session,
        "cas_filter",
        choices = sort(unique(app_data$targets$cas_rn)),
        server = FALSE
      )
    })
    
    daily_flow_mld <- reactive({
      state <- sidebar_data$state()
      req(state$flow_unit)
      
      safe_flow <- state$flow_val
      
      safe_flow <- if (
        is.null(safe_flow) ||
        length(safe_flow) == 0 ||
        is.na(safe_flow)
      ) {
        0
      } else {
        safe_flow
      }
      
      if (state$flow_unit == "m³/y") {
        (safe_flow / 1000) / 365
      } else {
        safe_flow / 365
      }
    })
    
    base_data <- reactive({
      state <- sidebar_data$state()
      req(state$treat, state$res_mix, state$solids_process, state$data_source)
      
      base_frame <- app_data$targets |>
        dplyr::select(substance_name, cas_rn)
      
      if (state$data_source == "lab") {
        return(
          base_frame |>
            dplyr::mutate(
              mass_liq = 0,
              res_col_liq = "N/A",
              trace_err_liq = "Lab mode pending",
              trace_vol_liq = NA_real_,
              trace_unit_liq = NA_character_,
              trace_conc_liq = NA_real_,
              trace_source_liq = NA_character_,
              mass_sol = 0,
              res_col_sol = "N/A",
              trace_err_sol = "Lab mode pending",
              trace_vol_sol = NA_real_,
              trace_unit_sol = NA_character_,
              trace_conc_sol = NA_real_,
              trace_source_sol = NA_character_,
              aggregate_kg = 0
            )
        )
      }
      
      current_reg <- MATRIX_REGISTRY[
        MATRIX_REGISTRY$treat == state$treat &
          MATRIX_REGISTRY$mix == state$res_mix &
          MATRIX_REGISTRY$solids == state$solids_process,
      ]
      
      if ("liq" %in% state$active_streams) {
        liq_df <- calc_stream(
          base_frame,
          app_data$eff_spec,
          current_reg,
          state$flow_val,
          state$flow_unit,
          "liq"
        )
        
        base_frame <- base_frame |>
          dplyr::left_join(
            liq_df |>
              dplyr::select(-conc) |>
              dplyr::rename(
                mass_liq = mass_kg,
                res_col_liq = res_col,
                trace_err_liq = trace_err,
                trace_vol_liq = trace_vol,
                trace_unit_liq = trace_unit,
                trace_conc_liq = trace_conc,
                trace_source_liq = trace_source
              ),
            by = "cas_rn"
          )
      } else {
        base_frame <- base_frame |>
          dplyr::mutate(
            mass_liq = 0,
            res_col_liq = "N/A",
            trace_err_liq = "Stream manually disabled.",
            trace_vol_liq = NA_real_,
            trace_unit_liq = NA_character_,
            trace_conc_liq = NA_real_,
            trace_source_liq = NA_character_
          )
      }
      
      if ("sol" %in% state$active_streams) {
        sol_df <- calc_stream(
          base_frame,
          app_data$bio_spec,
          current_reg,
          state$dry_tonnes,
          state$solids_unit,
          "sol"
        )
        
        base_frame <- base_frame |>
          dplyr::left_join(
            sol_df |>
              dplyr::select(-conc) |>
              dplyr::rename(
                mass_sol = mass_kg,
                res_col_sol = res_col,
                trace_err_sol = trace_err,
                trace_vol_sol = trace_vol,
                trace_unit_sol = trace_unit,
                trace_conc_sol = trace_conc,
                trace_source_sol = trace_source
              ),
            by = "cas_rn"
          )
      } else {
        base_frame <- base_frame |>
          dplyr::mutate(
            mass_sol = 0,
            res_col_sol = "N/A",
            trace_err_sol = "Stream manually disabled.",
            trace_vol_sol = NA_real_,
            trace_unit_sol = NA_character_,
            trace_conc_sol = NA_real_,
            trace_source_sol = NA_character_
          )
      }
      
      base_frame |>
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("mass"), \(x) dplyr::coalesce(x, 0)),
          aggregate_kg = mass_liq + mass_sol
        )
    })
    
    final_display_data <- reactive({
      df <- base_data()
      
      if (!is.null(input$cas_filter) && length(input$cas_filter) > 0) {
        df <- df |>
          dplyr::filter(cas_rn %in% input$cas_filter)
      }
      
      if (isTRUE(input$hide_zero)) {
        df <- df |>
          dplyr::filter(aggregate_kg > APP_CONFIG$zero_tol)
      }
      
      df
    })
    
    output$top_ribbon <- renderUI({
      df <- final_display_data()
      state <- sidebar_data$state()
      
      is_dev <- state$data_source == "lab"
      avg_mld <- daily_flow_mld()
      
      is_mandatory <- isTRUE(avg_mld >= APP_CONFIG$limit_mld)
      total_val <- sum(df$aggregate_kg, na.rm = TRUE)
      report_count <- sum(df$aggregate_kg >= APP_CONFIG$threshold_kg, na.rm = TRUE)
      is_danger <- isTRUE(report_count > 0)
      
      status_text <- if (!is_mandatory) {
        "EXEMPT"
      } else if (!is_danger) {
        "BELOW LIMIT"
      } else {
        sprintf("REPORTABLE: %d PFAS", report_count)
      }
      
      status_sub <- if (!is_mandatory) {
        "Flow < 10 ML/d"
      } else if (!is_danger) {
        "All compounds < 1.0 kg/y"
      } else {
        ">= 1.0 kg/y and Flow >= 10 ML/d"
      }
      
      status_icon <- if (!is_mandatory) {
        "shield-check"
      } else if (!is_danger) {
        "check-circle"
      } else {
        "exclamation-triangle"
      }
      
      status_color <- if (!is_mandatory) {
        "bg-secondary"
      } else if (!is_danger) {
        "bg-success"
      } else {
        "bg-danger"
      }
      
      layout_columns(
        class = "mb-3",
        gap = "0.5rem",
        fill = FALSE,
        
        dense_metric_card(
          "Total PFAS Load",
          sprintf("%s kg", formatC(total_val, format = "f", digits = 2, big.mark = ",")),
          if (is_dev) "Site-Specific" else "ECCC Tables",
          "speedometer2",
          if (is_dev) "bg-info" else "bg-primary"
        ),
        
        dense_metric_card(
          "Compliance Status",
          status_text,
          status_sub,
          status_icon,
          status_color
        ),
        
        dense_metric_card(
          toupper(state$site_name %||% "Unknown Facility"),
          if (is_mandatory) "MANDATORY" else "EXEMPT",
          sprintf("Avg: %.1f ML/d", avg_mld),
          "building",
          if (is_mandatory) "bg-primary" else "bg-secondary"
        ),
        
        dense_metric_card(
          "PFAS Coverage",
          sprintf("%d Tracked", nrow(app_data$targets)),
          sprintf("%d Active | %d Over Limit", nrow(df), report_count),
          "list-check",
          "bg-info"
        )
      )
    })
    
    observeEvent(sidebar_data$export_click(), {
      req(sidebar_data$export_click() > 0)
      
      df <- final_display_data()
      state <- sidebar_data$state()
      
      export_df <- df |>
        dplyr::select(
          `Substance` = substance_name,
          `CAS` = cas_rn,
          `Liquids (kg_y)` = mass_liq,
          `Solids (kg_y)` = mass_sol,
          `Total (kg_y)` = aggregate_kg
        )
      
      csv_out <- character()
      con <- textConnection("csv_out", "w", local = TRUE)
      write.csv(export_df, con, row.names = FALSE)
      close(con)
      
      csv_string <- paste(csv_out, collapse = "\n")
      
      filename <- paste0(
        "NPRI-PFAS-",
        if (isTruthy(state$site_name)) {
          gsub("[^A-Za-z0-9_-]", "-", state$site_name)
        } else {
          "Facility"
        },
        "-",
        Sys.Date(),
        ".csv"
      )
      
      session$sendCustomMessage(
        "download_csv",
        list(
          filename = filename,
          csv_data = csv_string
        )
      )
    })
    
    output$main_table <- renderReactable({
      req(input$sig_figs)
      
      data <- final_display_data()
      is_mandatory <- isTRUE(daily_flow_mld() >= APP_CONFIG$limit_mld)
      
      reactable(
        data,
        pagination = FALSE,
        highlight = TRUE,
        defaultSorted = "aggregate_kg",
        defaultSortOrder = "desc",
        
        rowStyle = function(index) {
          if (!is.null(index) && data$aggregate_kg[index] >= APP_CONFIG$threshold_kg) {
            list(backgroundColor = "#f8d7da")
          }
        },
        
        details = function(index) {
          row <- data[index, ]
          
          liq_trace <- if (!is.na(row$trace_err_liq)) {
            tags$li(
              tags$strong("Effluent: "),
              tags$span(class = "text-danger", row$trace_err_liq)
            )
          } else if (is.na(row$trace_conc_liq)) {
            tags$li(
              tags$strong("Effluent: "),
              tags$span(class = "text-muted", "Not tested in this matrix.")
            )
          } else {
            tags$li(
              tags$strong("Effluent: "),
              sprintf(
                "(Vol: %s %s) * (Conc: %s ng/L) * 10^-12 = %s kg/y",
                row$trace_vol_liq,
                row$trace_unit_liq,
                signif(row$trace_conc_liq, 3),
                signif(row$mass_liq, 4)
              ),
              tags$br(),
              tags$span(
                class = "text-muted small ms-2",
                sprintf(" Source: %s", row$trace_source_liq)
              )
            )
          }
          
          sol_trace <- if (!is.na(row$trace_err_sol)) {
            tags$li(
              tags$strong("Biosolids: "),
              tags$span(class = "text-danger", row$trace_err_sol)
            )
          } else if (is.na(row$trace_conc_sol)) {
            tags$li(
              tags$strong("Biosolids: "),
              tags$span(class = "text-muted", "Not tested in this matrix.")
            )
          } else {
            tags$li(
              tags$strong("Biosolids: "),
              sprintf(
                "(Mass: %s %s) * (Conc: %s ng/g) * 10^-12 = %s kg/y",
                row$trace_vol_sol,
                row$trace_unit_sol,
                signif(row$trace_conc_sol, 3),
                signif(row$mass_sol, 4)
              ),
              tags$br(),
              tags$span(
                class = "text-muted small ms-2",
                sprintf(" Source: %s", row$trace_source_sol)
              )
            )
          }
          
          tags$div(
            class = "p-3 bg-light border rounded small m-2",
            tags$h6(class = "fw-bold text-primary mb-2", " Calculation Trace"),
            tags$ul(class = "list-unstyled mb-0 lh-lg", liq_trace, sol_trace)
          )
        },
        
        theme = reactableTheme(
          headerStyle = list(
            backgroundColor = APP_CONFIG$theme_primary,
            color = "#fff",
            position = "sticky",
            top = 0,
            zIndex = 1
          ),
          cellStyle = list(padding = "10px 8px")
        ),
        
        columns = list(
          substance_name = colDef(
            name = "Substance",
            minWidth = 250,
            cell = function(value, index) {
              is_over <- data$aggregate_kg[index] >= APP_CONFIG$threshold_kg
              
              if (is_over && is_mandatory) {
                htmltools::tagList(
                  tags$span(value, class = "fw-bold text-danger"),
                  tags$span(" REPORTABLE", class = "badge bg-danger ms-2")
                )
              } else if (is_over && !is_mandatory) {
                htmltools::tagList(
                  tags$span(value, class = "fw-bold text-warning"),
                  tags$span(" OVER MASS (EXEMPT FLOW)", class = "badge bg-warning text-dark ms-2")
                )
              } else {
                value
              }
            }
          ),
          
          cas_rn = colDef(
            name = "CAS RN",
            align = "center",
            width = 110
          ),
          
          mass_liq = colDef(
            name = "Liquids (kg/y)",
            align = "right",
            minWidth = 110,
            cell = \(v) fmt_sigfigs(v, input$sig_figs)
          ),
          
          mass_sol = colDef(
            name = "Solids (kg/y)",
            align = "right",
            minWidth = 110,
            cell = \(v) fmt_sigfigs(v, input$sig_figs)
          ),
          
          aggregate_kg = colDef(
            name = "Total (kg/y)",
            align = "right",
            minWidth = 130,
            cell = \(v) fmt_sigfigs(v, input$sig_figs),
            style = function(v) {
              bar_w <- min(100, (v / APP_CONFIG$threshold_kg) * 100)
              
              list(
                background = sprintf(
                  "linear-gradient(90deg, %s %s%%, transparent %s%%)",
                  if (v >= APP_CONFIG$threshold_kg) "#f8d7da" else "#e2e3e5",
                  bar_w,
                  bar_w
                ),
                fontWeight = "bold",
                color = if (v >= APP_CONFIG$threshold_kg) "#842029" else "inherit"
              )
            }
          ),
          
          res_col_liq = colDef(show = FALSE),
          trace_err_liq = colDef(show = FALSE),
          trace_vol_liq = colDef(show = FALSE),
          trace_unit_liq = colDef(show = FALSE),
          trace_conc_liq = colDef(show = FALSE),
          trace_source_liq = colDef(show = FALSE),
          
          res_col_sol = colDef(show = FALSE),
          trace_err_sol = colDef(show = FALSE),
          trace_vol_sol = colDef(show = FALSE),
          trace_unit_sol = colDef(show = FALSE),
          trace_conc_sol = colDef(show = FALSE),
          trace_source_sol = colDef(show = FALSE)
        )
      )
    })
    
    output$diagnostic_ui <- renderUI({
      df <- base_data()
      state <- sidebar_data$state()
      
      liq_active <- "liq" %in% state$active_streams
      sol_active <- "sol" %in% state$active_streams
      
      err_liq <- if (liq_active) sum(is.na(df$trace_conc_liq)) else 0
      err_sol <- if (sol_active) sum(is.na(df$trace_conc_sol)) else 0
      
      zero_liq <- if (liq_active) sum(df$trace_conc_liq == 0, na.rm = TRUE) else 0
      zero_sol <- if (sol_active) sum(df$trace_conc_sol == 0, na.rm = TRUE) else 0
      
      layout_columns(
        col_widths = 12,
        gap = "1rem",
        
        card(
          card_header(
            bs_icon("clipboard-data"),
            "Data Pipeline Health",
            class = "bg-primary text-white"
          ),
          
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              
              tags$ul(
                class = "list-group list-group-flush border-end pe-3",
                
                tags$li(
                  class = "list-group-item d-flex justify-content-between align-items-center",
                  "Target Substances",
                  tags$span(class = "badge bg-primary rounded-pill", nrow(app_data$targets))
                ),
                
                tags$li(
                  class = "list-group-item d-flex justify-content-between align-items-center mt-2",
                  "Missing Effluent Concentrations (NA)",
                  tags$span(
                    class = sprintf(
                      "badge rounded-pill %s",
                      if (!liq_active) {
                        "bg-secondary"
                      } else if (err_liq > 0) {
                        "bg-danger"
                      } else {
                        "bg-success"
                      }
                    ),
                    if (!liq_active) "Disabled" else err_liq
                  )
                ),
                
                tags$li(
                  class = "list-group-item d-flex justify-content-between align-items-center bg-light",
                  "Zero Concentration - Effluent (Tested but ND)",
                  tags$span(
                    class = sprintf(
                      "badge rounded-pill %s",
                      if (!liq_active) {
                        "bg-secondary"
                      } else if (zero_liq > 0) {
                        "text-bg-warning"
                      } else {
                        "bg-success"
                      }
                    ),
                    if (!liq_active) "Disabled" else zero_liq
                  )
                ),
                
                tags$li(
                  class = "list-group-item d-flex justify-content-between align-items-center mt-2",
                  "Missing Biosolid Concentrations (NA)",
                  tags$span(
                    class = sprintf(
                      "badge rounded-pill %s",
                      if (!sol_active) {
                        "bg-secondary"
                      } else if (err_sol > 0) {
                        "bg-danger"
                      } else {
                        "bg-success"
                      }
                    ),
                    if (!sol_active) "Disabled" else err_sol
                  )
                ),
                
                tags$li(
                  class = "list-group-item d-flex justify-content-between align-items-center bg-light",
                  "Zero Concentration - Biosolids (Tested but ND)",
                  tags$span(
                    class = sprintf(
                      "badge rounded-pill %s",
                      if (!sol_active) {
                        "bg-secondary"
                      } else if (zero_sol > 0) {
                        "text-bg-warning"
                      } else {
                        "bg-success"
                      }
                    ),
                    if (!sol_active) "Disabled" else zero_sol
                  )
                )
              ),
              
              tags$div(
                class = "ps-3",
                
                tags$p(
                  class = "small fw-bold text-muted mb-1 text-uppercase",
                  "Target Resolution State"
                ),
                
                tags$p(
                  class = "small fw-bold mb-1",
                  "Liquid Target Matrix Column:"
                ),
                
                tags$code(
                  na.omit(unique(df$res_col_liq))[1] %||% "None",
                  class = "d-block mb-3 text-break"
                ),
                
                tags$p(
                  class = "small fw-bold mb-1",
                  "Solid Target Matrix Column:"
                ),
                
                tags$code(
                  na.omit(unique(df$res_col_sol))[1] %||% "None",
                  class = "d-block text-break"
                )
              )
            )
          )
        )
      )
    })
  })
}

# ==============================================================================
# Application UI and Server Initialization
# ==============================================================================

# Use system fonts for theme to avoid downloads
my_theme <- bs_theme(
  bootswatch = "flatly",
  primary = APP_CONFIG$theme_primary,
  "font-size-base" = "0.85rem",
  base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
  heading_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
)

ui <- page_sidebar(
  title = "NPRI PFAS Compliance Engine",
  theme = my_theme,
  js_download_script,
  sidebar = mod_sidebar_ui("app_sidebar"),
  uiOutput(NS("app_dashboard", "top_ribbon")),
  mod_dashboard_ui("app_dashboard")
)

server <- function(input, output, session) {
  
  # ==============================================================================
  # Gatekeeper Modal
  # ==============================================================================
  # We wrap this in an onFlushed observer to ensure the WebAssembly environment
  # waits for the DOM to render before attempting to trigger the modal.
  observeEvent(session$onFlushed, once = TRUE, {
    showModal(
      modalDialog(
        title = "Restricted Access",
        tags$p("This PFAS calculator is an internal tool for authorized wastewater facility users only"),
        tags$p("Please enter the access code to continue:"),
        textInput("gate_pass", NULL, placeholder = "Enter password..."),
        actionButton("gate_submit", "Unlock", class = "btn-primary w-100"),
        footer = NULL,        # Removes the default "Dismiss" button
        keyboard = FALSE,     # Prevents closing with the Esc key
        backdrop = "static"   # Prevents closing by clicking outside the modal
      )
    )
  })
  
  # Check password when the button is clicked
  observeEvent(input$gate_submit, {
    if (input$gate_pass == "NPRI2026") {
      removeModal()
    } else {
      showNotification("Incorrect access code.", type = "error", duration = 3)
      # Clear the input field after a failed attempt
      updateTextInput(session, "gate_pass", value = "")
    }
  })
  # ==============================================================================
  
  sidebar_state <- mod_sidebar_server("app_sidebar")
  mod_dashboard_server("app_dashboard", sidebar_state, app_data)
}

shinyApp(ui, server)