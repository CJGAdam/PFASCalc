# ==============================================================================
# LIBRARIES
# ==============================================================================
library(shiny)
library(bslib)      # Modern UI layout
library(bsicons)    # Standardized icons (v0.1.2 Safe)
library(reactable)  # Interactive data tables
library(htmltools)  # Safe HTML building
library(dplyr)      # Data wrangling
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(janitor)
library(rlang)

# ==============================================================================
# CONFIGURATION & CONSTANTS
# ==============================================================================
APP_CONFIG <- list(
  data_dir       = "data/rds/",
  limit_mld      = 10,
  threshold_kg   = 1.0,
  theme_primary  = "#2c3e50",
  days_per_year  = 365,
  ml_to_l        = 1e6,
  ng_to_kg       = 1e-12,
  zero_tol       = 1e-9   # Tolerance for filtering empty mass rows
)

# ==============================================================================
# 1. GLOBAL HELPERS & TAXONOMY
# ==============================================================================

# EPA 1633 Anion-to-Acid Crosswalk Dictionary
PFAS_CROSSWALK <- tibble::tribble(
  ~lab_cas,      ~npri_cas,   ~mw_salt, ~mw_acid, ~npri_substance_name,
  "2795-39-3",   "1763-23-1", 538.22,   500.13,   "Perfluorooctane sulfonate (PFOS)",
  "3825-26-1",   "335-67-1",  431.10,   414.07,   "Perfluorooctanoic acid (PFOA)",
  "335-95-5",    "335-67-1",  436.06,   414.07,   "Perfluorooctanoic acid (PFOA)",
  "6014-75-1",   "335-67-1",  453.06,   414.07,   "Perfluorooctanoic acid (PFOA)",
  "29081-56-9",  "375-73-5",  338.19,   300.10,   "Perfluorobutane sulfonic acid (PFBS)",
  "3871-99-6",   "355-46-4",  438.20,   400.11,   "Perfluorohexane sulfonic acid (PFHxS)"
) |>
  mutate(conversion_factor = mw_acid / mw_salt)

# --- Pure Functions ---

fmt_sigfigs <- \(v, sig_figs) {
  if(is.na(v) || v == 0) return("0")
  sub("\\.$", "", formatC(signif(v, sig_figs), digits = sig_figs, format = "fg", flag = "#"))
}

clean_pfas <- \(df) {
  df <- df |> clean_names()
  if ("pfas" %in% names(df)) df <- df |> rename(substance_name = pfas)
  if ("substance" %in% names(df)) df <- df |> rename(substance_name = substance)
  if ("cas" %in% names(df)) df <- df |> rename(cas_rn = cas)
  
  df |> 
    mutate(
      across(-any_of(c("substance_name", "cas_rn")), \(val) {
        val_str <- as.character(val)
        num_val <- suppressWarnings(parse_number(val_str))
        if_else(str_starts(val_str, "<"), num_val / 2, num_val)
      }),
      across(any_of(c("substance_name", "cas_rn")), \(val) str_trim(as.character(val)))
    )
}

find_col <- \(df, keywords) {
  valid_keys <- keywords |> discard(\(key) key == "")
  names(df) |> detect(\(col) all(str_detect(col, paste0("(^|_)", valid_keys, "($|_)"))))
}

calc_stream <- \(df, scenario, treat, mix, volume, type) {
  if (is.null(df) || nrow(df) == 0) return(tibble(cas_rn = character(), mass_kg = numeric()))
  
  if (scenario == "s3") {
    keywords <- "median"
  } else {
    p1 <- str_to_lower(str_extract(treat, "^\\w+"))
    p3 <- case_when(mix == "< 90%" ~ "lt_90", mix == "> 90%" ~ "gt_90", mix == "< 70%" ~ "lt_70", .default = "70_pct_90")
    keywords <- c(p1, p3) 
  }
  
  target_col <- find_col(df, keywords)
  
  if (is.null(target_col)) {
    warning(sprintf("calc_stream: Target column not found for keywords '%s'", paste(keywords, collapse = ", ")))
    return(tibble(cas_rn = character(), mass_kg = numeric()))
  }
  
  mult <- if_else(type == "liq", APP_CONFIG$days_per_year, 1)
  
  df |> 
    select(cas_rn, conc = all_of(target_col)) |>
    mutate(mass_kg = (volume * APP_CONFIG$ml_to_l * mult * conc) * APP_CONFIG$ng_to_kg) |>
    select(cas_rn, mass_kg)
}

load_app_data <- \() {
  dir_path <- APP_CONFIG$data_dir
  if (!dir.exists(dir_path)) return(list(status = "Missing", targets = tibble()))
  
  raw_time <- file.info(file.path(dir_path, "Table_1.rds"))$mtime
  list(
    targets  = read_rds(file.path(dir_path, "Table_1.rds")) |> clean_pfas(),
    eff_spec = read_rds(file.path(dir_path, "Table_7.rds")) |> clean_pfas(),
    bio_spec = read_rds(file.path(dir_path, "Table_8.rds")) |> clean_pfas(),
    eff_gen  = read_rds(file.path(dir_path, "Table_9.rds")) |> clean_pfas(),
    bio_gen  = read_rds(file.path(dir_path, "Table_10.rds")) |> clean_pfas(),
    scrape_date = format(as.Date(raw_time), "%B %d, %Y"),
    status = "Loaded"
  )
}

app_data <- load_app_data()

# ==============================================================================
# 2. UI COMPONENTS
# ==============================================================================

ui_sidebar <- \() {
  sidebar(
    width = 330,
    bg = "#f8f9fa", 
    
    title = tags$div(
      class = "py-2 border-bottom mb-2 d-flex align-items-center gap-2",
      bs_icon("sliders", class = "fs-5 text-primary"), 
      tags$h6("Engine Parameters", class = "mb-0 fw-bold")
    ),
    
    # --- SECTION 1: REPORTING PATHWAY ---
    tags$div(
      class = "d-flex align-items-center gap-2 mt-2 mb-2 px-1",
      bs_icon("diagram-3", class = "text-primary"),
      tags$span(class = "fw-bold", "Reporting Pathway")
    ),
    
    tags$div(
      class = "bg-white p-2 rounded border shadow-sm mb-2",
      checkboxGroupInput("active_streams", tags$span(bs_icon("droplet-half", class="me-2 text-info"), "Active Streams:"), choices = list("Effluent" = "liq", "Biosolids" = "sol"), selected = c("liq", "sol"), inline = TRUE)
    ),
    
    tags$div(
      class = "bg-white p-2 rounded border shadow-sm mb-2", 
      input_switch("lab_toggle", tags$span(bs_icon("clipboard-data", class="me-2 text-info"), "Lab Data Available? ", tags$span("(Scenario 1)", class = "text-muted small text-nowrap")), value = FALSE),
      
      conditionalPanel(
        condition = "!input.lab_toggle",
        tags$div(class = "mt-2 pt-2 border-top",
                 selectInput("scenario", tags$span(bs_icon("calculator", class="me-2"), "Fall-back Scenario:"), choices = list("Scenario 2: Profile Baseline" = "s2", "Scenario 3: National Average" = "s3"))
        )
      ),
      conditionalPanel(
        condition = "input.lab_toggle",
        tags$div(class = "mt-2 pt-2 border-top",
                 fileInput("user_csv", tags$span(bs_icon("file-earmark-spreadsheet", class="me-2"), "Upload Lab Data (.csv)"), accept = ".csv", buttonLabel = bs_icon("upload")),
                 radioButtons("lab_acid_eq", tooltip(tags$span(bs_icon("info-circle", class="me-1"), "Is Acid Equivalent? "), "EPA 1633 reports anions. If 'No', the engine automatically converts these to parent acid mass equivalents."), choices = c("Yes", "No"), selected = "No", inline = TRUE)
        )
      )
    ),
    
    # --- SECTION 2: FACILITY PROFILE ---
    # [UX Polish]: Added border-top, pt-3, and mt-3 for clean visual delineation
    tags$div(
      class = "d-flex align-items-center gap-2 mt-3 mb-2 px-1 border-top pt-3",
      bs_icon("building", class = "text-primary"),
      tags$span(class = "fw-bold", "Facility Profile")
    ),
    
    tags$div(
      class = "bg-white p-2 rounded border shadow-sm",
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        numericInput("flow_mld", tags$span(bs_icon("droplet", class="me-1 text-primary"), "Flow (ML/d)"), value = 5, min = 0),
        numericInput("dry_tonnes", tags$span(bs_icon("box-seam", class="me-1 text-primary"), "Solids (t/y)"), value = 5, min = 0)
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6),
        selectInput("treat", tags$span(bs_icon("funnel", class="me-2 text-primary"), "Treatment:"), choices = list("Primary", "Secondary", "Tertiary / Advanced")),
        uiOutput("res_mix_ui") 
      )
    ),
    
    tags$div(class = "mt-auto text-center small text-muted pt-3", str_glue("Source: ECCC PFAS Guidance 2026"), tags$br(), tags$span(str_glue("Updated: {coalesce(app_data$scrape_date, 'N/A')}"), class = "opacity-75"))
  )
}

ui_main_tabs <- \() {
  navset_card_underline(
    nav_panel(
      title = "Mass Balance Table", icon = bs_icon("table"), 
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-light d-flex align-items-center justify-content-between gap-3 flex-wrap py-2",
          tags$div(
            class = "d-flex align-items-center mb-0",
            style = "flex: 1; min-width: 250px; max-width: 450px;",
            tags$div(class = "w-100", style = "margin-bottom: -15px;", 
                     selectizeInput("cas_filter", NULL, choices = NULL, multiple = TRUE, options = list(placeholder = 'Search CAS RN...'), width = "100%")
            )
          ),
          tags$div(
            class = "d-flex align-items-center gap-4 flex-wrap",
            tags$div(
              class = "d-flex align-items-center gap-2",
              tooltip(tags$span("Sig Figs ", bs_icon("info-circle"), class = "text-muted small"), "Adjust precision. Trailing zeros are preserved for scientific reporting."),
              tags$div(style = "margin-bottom: -15px;", numericInput("sig_figs", NULL, value = 2, min = 1, max = 8, width = "80px"))
            ),
            # [UX Polish]: Wrapped checkbox in flex container with pt-1 to align baselines
            tags$div(
              class = "d-flex align-items-center pt-1", style = "margin-bottom: -15px;", 
              checkboxInput("hide_zero", "Hide zero mass", value = TRUE)
            ),
            downloadButton("export_data", "Export CSV", class = "btn-outline-primary btn-sm")
          )
        ),
        card_body(padding = 0, reactableOutput("main_table"))
      )
    ),
    
    # --- 2. METHODOLOGY SUMMARY ---
    nav_panel(
      title = "Methodology Summary", icon = bs_icon("info-square"),
      card(
        card_header(
          class = "bg-light d-flex align-items-center gap-2",
          bs_icon("book-half"), tags$h5("Compliance Calculation Methodology", class = "mb-0")
        ),
        card_body(
          class = "p-4",
          tags$h6("1. Regulatory & Facility Thresholds", class="fw-bold text-primary mt-2"),
          tags$blockquote(class="border-start border-4 border-primary ps-3 mb-4 bg-light p-3 rounded",
                          tags$div(class="mb-2", 
                                   tags$strong("Mass Threshold (Aggregate):"), 
                                   " Reporting is mandatory if the total combined mass of all PFAS in this group is ", 
                                   tags$span(class="badge bg-danger fs-6", HTML(str_glue("&ge; {APP_CONFIG$threshold_kg} kg/year"))), "."
                          ),
                          tags$div(class="mb-2 text-muted small ps-3",
                                   tags$em("Note: Under ECCC NPRI Part 1C, if this aggregate threshold is met, "),
                                   tags$strong("all"), 
                                   tags$em(" individual PFAS within the group must be reported, regardless of their individual mass.")
                          ),
                          tags$div(
                            tags$strong("Facility Threshold:"), 
                            " WWTPs with an annual average discharge of ", 
                            tags$span(class="badge bg-secondary fs-6", HTML(str_glue("&lt; {APP_CONFIG$limit_mld} ML/day"))), 
                            " are entirely exempt."
                          )
          ),
          
          tags$h6("2. The 0.1% Concentration Rule & By-products", class="fw-bold text-primary mt-4"),
          tags$p(class="mb-4", HTML("Typically, PFAS reporting only applies to substances present at concentrations &ge; 0.1%. <strong>However</strong>, for wastewater treatment plants where PFAS is a <em>by-product</em>, the 0.1% threshold <strong>does not apply</strong>. This engine aggregates all trace mass regardless of concentration.")),
          
          tags$h6("3. Handling Non-Detects", class="fw-bold text-primary mt-4"),
          tags$p(class="mb-4", HTML("Following official guidance, when a substance is tested for but not detected (indicated by a <code>&lt;</code> symbol), the <strong>half-detection method</strong> is strictly applied. For example, a lab result of <code>&lt; 4.0</code> is calculated into the mass balance as <code>2.0</code>.")),
          
          tags$h6("4. Estimation Scenarios", class="fw-bold text-primary mt-4"),
          layout_column_wrap(
            width = "280px", gap = "1rem", class = "mb-4",
            card(class = "bg-light border-0 shadow-sm", card_body(tags$strong("Scenario 1: Site-Specific"), tags$br(), "Uses your facility's actual analytical laboratory data (e.g., EPA 1633). Highly recommended for accuracy.")),
            card(class = "bg-light border-0 shadow-sm", card_body(tags$strong("Scenario 2: Profile Baseline"), tags$br(), "Estimates mass using ECCC matrices matched exactly to your plant's treatment tier and residential mix.")),
            card(class = "bg-light border-0 shadow-sm", card_body(tags$strong("Scenario 3: National Average"), tags$br(), "Estimates mass based strictly on the national median concentrations for all Canadian WWTPs."))
          ),
          
          tags$h6("5. Acid Equivalent Conversion (The Anion Rule)", class="fw-bold text-primary mt-4"),
          tags$p(class="mb-2", HTML("NPRI requires all salts and anions be reported as an <strong>equivalent weight of the parent acid</strong>. If lab data is uploaded as anions, this engine applies the following formula automatically:")),
          tags$pre(class="bg-dark text-white p-3 rounded", HTML("Equivalent Acid Mass = Lab Mass &times; (MW Acid / MW Salt)"))
        )
      )
    ),
    
    nav_panel(
      title = "Data Diagnostics", icon = bs_icon("bug"),
      card(
        card_header("Background Data Integrity Audit", class = "bg-light"),
        card_body(uiOutput("diagnostic_ui"))
      )
    )
  )
}

# --- UI ASSEMBLY ---
ui <- page_sidebar(
  title = "NPRI PFAS Compliance Engine",
  theme = bs_theme(bootswatch = "flatly", primary = APP_CONFIG$theme_primary, "font-size-base" = "0.85rem"),
  sidebar = ui_sidebar(),
  uiOutput("top_ribbon"),
  ui_main_tabs()
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- \(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "cas_filter", choices = sort(unique(app_data$targets$cas_rn)), server = TRUE)
  })
  
  output$res_mix_ui <- renderUI({
    req(input$treat)
    choices <- switch(input$treat, "Primary" = c("< 90%", "> 90%"), "Secondary" = c("< 70%", "70% - 90%", "> 90%"), "Tertiary / Advanced" = c("< 90%", "> 90%"))
    selectInput("res_mix", HTML(paste(bs_icon("people", class="me-2 text-primary"), "Flow Mix:")), choices = choices)
  })
  
  user_lab_data <- reactive({
    req(input$lab_toggle, input$user_csv)
    lab_raw <- tryCatch(read_csv(input$user_csv$datapath, show_col_types = FALSE) |> clean_pfas(), error = \(e) NULL)
    
    validate(
      need(!is.null(lab_raw), "Invalid or unreadable CSV file."), 
      need(any(str_detect(names(lab_raw), "cas")), "Uploaded CSV must contain a 'CAS' column."), 
      need(any(str_detect(names(lab_raw), "conc")), "Uploaded CSV must contain a 'Conc' column.")
    )
    
    cas_col <- names(lab_raw) |> detect(\(col) str_detect(col, "cas"))
    conc_col <- names(lab_raw) |> keep(\(col) str_detect(col, "conc")) |> _[[1]]
    
    clean_df <- lab_raw |> 
      mutate(cas_rn = .data[[cas_col]], val = .data[[conc_col]]) |> 
      select(cas_rn, val)
    
    if (input$lab_acid_eq == "No") {
      clean_df <- clean_df |> 
        left_join(PFAS_CROSSWALK, by = c("cas_rn" = "lab_cas")) |>
        mutate(cas_rn = coalesce(npri_cas, cas_rn), val = val * coalesce(conversion_factor, 1)) |>
        group_by(cas_rn) |> 
        summarise(val = sum(val, na.rm = TRUE), .groups = "drop")
    }
    return(clean_df)
  })
  
  base_data <- reactive({
    req(input$treat, input$res_mix) 
    
    scen <- if(input$lab_toggle) "s1" else input$scenario
    eff_df <- if(scen == "s3") app_data$eff_gen else app_data$eff_spec
    bio_df <- if(scen == "s3") app_data$bio_gen else app_data$bio_spec
    
    liq <- if("liq" %in% input$active_streams) calc_stream(eff_df, scen, input$treat, input$res_mix, input$flow_mld, "liq") else tibble(cas_rn = character(), mass_kg = numeric())
    sol <- if("sol" %in% input$active_streams) calc_stream(bio_df, scen, input$treat, input$res_mix, input$dry_tonnes, "sol") else tibble(cas_rn = character(), mass_kg = numeric())
    
    app_data$targets |> 
      select(substance_name, cas_rn) |>
      left_join(liq |> rename(mass_liq = mass_kg), by = "cas_rn") |>
      left_join(sol |> rename(mass_sol = mass_kg), by = "cas_rn") |>
      mutate(
        across(starts_with("mass"), \(x) coalesce(x, 0)), 
        aggregate_kg = mass_liq + mass_sol
      )
  })
  
  final_display_data <- reactive({
    df <- base_data()
    if(input$lab_toggle) {
      lab <- user_lab_data()
      if(!is.null(lab)) {
        df <- df |> left_join(lab |> rename(lab_val = val), by = "cas_rn") |>
          mutate(
            mass_liq = if_else(!is.na(lab_val), (input$flow_mld * APP_CONFIG$ml_to_l * APP_CONFIG$days_per_year * lab_val) * APP_CONFIG$ng_to_kg, mass_liq), 
            aggregate_kg = mass_liq + mass_sol
          ) |>
          select(-lab_val)
      }
    }
    if(!is.null(input$cas_filter)) df <- df |> filter(cas_rn %in% input$cas_filter)
    if(isTRUE(input$hide_zero)) df <- df |> filter(aggregate_kg > APP_CONFIG$zero_tol)
    return(df)
  })
  
  output$top_ribbon <- renderUI({
    df <- final_display_data()
    
    total_val <- sum(df$aggregate_kg, na.rm = TRUE)
    is_danger <- isTRUE(total_val >= APP_CONFIG$threshold_kg)
    is_mandatory <- isTRUE(input$flow_mld >= APP_CONFIG$limit_mld)
    
    total_tracked <- nrow(df)
    
    # [UX Polish]: Logically correct compliance escalation text
    if(is_danger) {
      reportable_count <- total_tracked
      reportable_text <- "All Tracked Reportable"
      reportable_class <- "danger"
    } else {
      reportable_count <- sum(df$aggregate_kg >= APP_CONFIG$threshold_kg, na.rm = TRUE)
      reportable_text <- str_glue("{reportable_count} Over Limit")
      reportable_class <- if(reportable_count > 0) "danger" else "success"
    }
    
    path_text <- if(input$lab_toggle) "Scenario 1: Site Lab Data" else switch(input$scenario, "s2" = "Scenario 2: Profile Baseline", "s3" = "Scenario 3: National Average")
    
    card(
      class = "mb-3 border-0 shadow-sm rounded-3",
      style = "background-color: #f8f9fa;",
      card_body(
        class = "py-3 px-4",
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = 6, xl = 3),
          gap = "2rem",
          
          tags$div(
            class = "d-flex gap-3 flex-grow-1 align-items-center",
            bs_icon("speedometer2", class = "fs-1 text-primary opacity-75"),
            tags$div(
              class = "w-100", 
              tags$div(
                tags$h6("AGGREGATE PFAS MASS", class = "text-muted small fw-bold mb-1", style = "letter-spacing: 0.5px;"),
                tags$div(
                  class = "d-flex align-items-baseline gap-2 mb-2",
                  # [UX Polish]: Added text-nowrap to prevent massive kg numbers from splitting onto two lines
                  tags$span(str_glue("{formatC(total_val, format = 'f', digits = 2, big.mark = ',')} kg"), class = "fs-4 fw-bold text-dark text-nowrap"),
                  tags$span(path_text, class = "small text-muted text-nowrap")
                )
              ),
              tags$div(
                class = "progress rounded-pill bg-light", style = "height: 6px;",
                tags$div(
                  class = str_glue("progress-bar {if(is_danger) 'bg-danger' else 'bg-success'}"), 
                  role = "progressbar", 
                  style = str_glue("width: {min(100, (total_val / APP_CONFIG$threshold_kg) * 100)}%")
                )
              )
            )
          ),
          
          tags$div(
            class = "d-flex align-items-center gap-3 border-start ps-3",
            bs_icon(if(is_danger) "exclamation-triangle-fill" else "check-circle-fill", class = str_glue("fs-1 text-{if(is_danger) 'danger' else 'success'} opacity-75")),
            tags$div(
              tags$h6("THRESHOLD STATUS", class = "text-muted small fw-bold mb-1", style = "letter-spacing: 0.5px;"),
              tags$div(
                class = "d-flex align-items-baseline gap-2",
                tags$span(if(is_danger) "REPORTABLE" else "BELOW LIMIT", class = str_glue("fs-5 fw-bold text-{if(is_danger) 'danger' else 'success'}")),
                tags$span(if(is_danger) str_glue(">= {APP_CONFIG$threshold_kg} kg/y") else "Under limit", class = "small text-muted text-nowrap")
              )
            )
          ),
          
          tags$div(
            class = "d-flex align-items-center gap-3 border-start ps-3",
            bs_icon("building", class = str_glue("fs-1 text-{if(is_mandatory) 'primary' else 'secondary'} opacity-75")),
            tags$div(
              tags$h6("FACILITY STATUS", class = "text-muted small fw-bold mb-1", style = "letter-spacing: 0.5px;"),
              tags$div(
                class = "d-flex align-items-baseline gap-2",
                tags$span(if(is_mandatory) "MANDATORY" else "EXEMPT", class = str_glue("fs-5 fw-bold text-{if(is_mandatory) 'primary' else 'secondary'}")),
                tags$span(str_glue("Ref: {APP_CONFIG$limit_mld} ML/d Limit"), class = "small text-muted text-nowrap")
              )
            )
          ),
          
          tags$div(
            class = "d-flex align-items-center gap-3 border-start ps-3",
            bs_icon("list-check", class = "fs-1 text-info opacity-75"),
            tags$div(
              tags$h6("SUBSTANCE COVERAGE", class = "text-muted small fw-bold mb-1", style = "letter-spacing: 0.5px;"),
              tags$div(
                class = "d-flex align-items-baseline gap-2",
                tags$span(str_glue("{total_tracked} Tracked"), class = "fs-6 fw-bold text-dark text-nowrap"),
                tags$span("|", class = "text-muted opacity-50"),
                tags$span(reportable_text, class = str_glue("fs-6 fw-bold text-{reportable_class} text-nowrap"))
              )
            )
          )
        )
      )
    )
  })
  
  output$export_data <- downloadHandler(
    filename = function() { paste("pfas-report-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(final_display_data(), file, row.names = FALSE) }
  )
  
  output$main_table <- renderReactable({
    req(input$sig_figs)
    data <- final_display_data()
    
    reactable(
      data, 
      pagination = TRUE, 
      highlight = TRUE, 
      compact = TRUE,
      defaultSorted = "aggregate_kg",
      defaultSortOrder = "desc",
      details = \(index) {
        row <- data[index, ]
        tags$div(class = "p-3 bg-light border rounded small",
                 tags$h6("Compliance Audit Trail", class = "fw-bold text-primary mb-2"),
                 tags$ul(class = "mb-0",
                         tags$li(str_glue("Substance: {row$substance_name} (CAS: {row$cas_rn})")),
                         tags$li(str_glue("Liquids Contribution: {fmt_sigfigs(row$mass_liq, input$sig_figs)} kg/y")),
                         tags$li(str_glue("Solids Contribution: {fmt_sigfigs(row$mass_sol, input$sig_figs)} kg/y")),
                         if(input$lab_toggle && input$lab_acid_eq == "No" && row$cas_rn %in% PFAS_CROSSWALK$npri_cas) 
                           tags$li(tags$b("Audit:"), " Anion-to-Acid conversion applied.")
                 )
        )
      },
      theme = reactableTheme(headerStyle = list(backgroundColor = APP_CONFIG$theme_primary, color = "#fff")),
      columns = list(
        substance_name = colDef(name = "Substance", minWidth = 300),
        cas_rn = colDef(name = "CAS RN", align = "center"),
        mass_liq = colDef(name = "Liquids (kg/y)", align = "right", cell = \(v) fmt_sigfigs(v, input$sig_figs)),
        mass_sol = colDef(name = "Solids (kg/y)", align = "right", cell = \(v) fmt_sigfigs(v, input$sig_figs)),
        aggregate_kg = colDef(
          name = "Total (kg/y)", align = "right",
          style = \(v) {
            bar_width <- min(100, (v / APP_CONFIG$threshold_kg) * 100)
            is_over <- v >= APP_CONFIG$threshold_kg
            bar_color <- if(is_over) "#f8d7da" else "#e2e3e5"
            list(background = str_glue("linear-gradient(90deg, {bar_color} {bar_width}%, transparent {bar_width}%)"), fontWeight = "bold", color = if(is_over) "#842029" else "inherit")
          },
          cell = \(v) fmt_sigfigs(v, input$sig_figs),
          header = function(v, n) htmltools::span(title = "Aggregate per-substance mass", "Total (kg/y) \u24D8")
        )
      )
    )
  })
  
  output$diagnostic_ui <- renderUI({
    status_color <- if(app_data$status == "Loaded") "bg-success" else "bg-danger"
    
    tags$div(
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = 6),
        tags$div(
          class = "p-3 border rounded h-100",
          tags$h6(bs_icon("hdd-network"), " System Metadata", class = "border-bottom pb-2"),
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tbody(
              tags$tr(tags$td(tags$strong("App Status:")), tags$td(tags$span(app_data$status, class = str_glue("badge {status_color}")))),
              tags$tr(tags$td(tags$strong("Scrape Date:")), tags$td(coalesce(app_data$scrape_date, "N/A"))),
              tags$tr(tags$td(tags$strong("Working Dir:")), tags$td(tags$code(getwd())))
            )
          )
        ),
        tags$div(
          class = "p-3 border rounded h-100",
          tags$h6(bs_icon("server"), " ECCC Data Integrity (Row Counts)", class = "border-bottom pb-2"),
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tbody(
              tags$tr(tags$td("Targets (Table 1):"), tags$td(tags$span(nrow(app_data$targets), class = "badge bg-secondary"))),
              tags$tr(tags$td("Effluent Spec (Table 7):"), tags$td(tags$span(nrow(app_data$eff_spec), class = "badge bg-secondary"))),
              tags$tr(tags$td("Biosolids Spec (Table 8):"), tags$td(tags$span(nrow(app_data$bio_spec), class = "badge bg-secondary")))
            )
          )
        )
      ),
      tags$div(
        class = "p-3 border rounded mt-3",
        tags$h6(bs_icon("activity"), " Active Data Flow Audit", class = "border-bottom pb-2"),
        tags$p("Current Active Columns Rendered in Final Display:", class = "mb-1 small text-muted"),
        tags$div(
          class = "d-flex flex-wrap gap-2",
          map(names(final_display_data()), \(col_name) tags$span(col_name, class = "badge bg-light text-dark border border-secondary"))
        )
      )
    )
  })
}

shinyApp(ui, server)