# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ░ Gall Fly Block Wrangling ░ ###################################################
# ░     By Kinglee Tham      ░ ###################################################
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
library(bsicons)
library(bslib)
library(dplyr)
library(DT)
library(htmltools)
library(purrr)
library(shiny)
library(sortable)
library(writexl)
sortable::enable_modules()

# (￣▽￣)ノ ####
# ╔══════════════════╗ +++++++++++++++++++
# ║ Some Logic Setup ║ ###################
# ╚══════════════════╝ +++++++++++++++++++

## ╔══════════════════════════╗
## │ Data Frame for Variables │ ####
## ╚══════════════════════════╝

load(file = "./data/gall_and_weather_data.RData")

variable_info <- data.frame(
  variable = c(
    "year", "semester", "site", "height", "diameter",
    "es_alive", "es_dead", "es_absent", "eo", "eg", "mordellistena",
    "woodpecker", "chickadee", "unknown", "double",
    "low_temp", "high_temp", "warmest_min_temp", "coldest_max_temp",
    "avg_min_temp", "avg_max_temp", "mean_temp", "total_precip",
    "total_snowfall", "snowdepth", "max_24hr_snowfall", "max_24hr_precip"
  ),

  type = c(
    "categorical",                                      # year
    "categorical",                                      # semester
    "categorical",                                      # site
    "continuous",                                       # height
    "continuous",                                       # diameter
    "categorical",                                      # es_alive
    "categorical",                                      # es_dead
    "categorical",                                      # es_absent
    "categorical",                                      # eo
    "categorical",                                      # eg
    "categorical",                                      # mordellistena
    "categorical",                                      # woodpecker
    "categorical",                                      # chickadee
    "categorical",                                      # unknown
    "categorical",                                      # double
    "continuous",                                       # low_temp
    "continuous",                                       # high_temp
    "continuous",                                       # warmest_min_temp
    "continuous",                                       # coldest_max_temp
    "continuous",                                       # avg_min_temp
    "continuous",                                       # avg_max_temp
    "continuous",                                       # mean_temp
    "continuous",                                       # total_precip
    "continuous",                                       # total_snowfall
    "continuous",                                       # snowdepth
    "continuous",                                       # max_24hr_snowfall
    "continuous"                                        # max_24hr_precip
  ),

  description = c(
    "year of sampling",                                        # year
    "fall or spring sampling season",                          # semester
    "field site code",                                         # site
    "goldenrod gall height (cm)",                              # height
    "gall diameter (mm)",                                      # diameter
    "1 if Eurosta solidaginis larva found alive",              # es_alive
    "1 if Eurosta solidaginis larva found dead",               # es_dead
    "1 if Eurosta solidaginis larva absent",                   # es_absent
    "1 if Eurytoma obtusiventris present",                     # eo
    "1 if Eurytoma gigantea present",                          # eg
    "1 if Mordellistena convicta present",                     # mordellistena
    "1 if woodpecker predation observed",                      # woodpecker
    "1 if chickadee predation observed",                       # chickadee
    "1 if predator/parasitoid could not be identified",        # unknown
    "1 if there were 2 galls on the same plant",               # double
    "coldest temp of the sampling season (C)",                  # low_temp
    "warmest temp of the sampling season (C)",                  # high_temp
    "warmest of the daily lows of the sampling season (C)",     # warmest_min_temp
    "coldest of the daily highs of the sampling season (C)",    # coldest_max_temp
    "average daily low of the sampling season (C)",             # avg_min_temp
    "average daily high of the sampling season (C)",            # avg_max_temp
    "mean temperature of the sampling season (C)",              # mean_temp
    "total precipitation of the sampling season (mm)",          # total_precip
    "total snowfall of the sampling season (mm)",               # total_snowfall
    "peak snow depth of the sampling season (mm)",              # snowdepth
    "greatest 24hr snowfall of the sampling season (mm)",       # max_24hr_snowfall
    "greatest 24hr precipitation of the sampling season (mm)"   # max_24hr_precip
  )
)

## ╔════════════════════════╗
## │ Vectors for User Input │ ####
## ╚════════════════════════╝

# Lookup key for each block type. Left is the name used by the server and right is the "value" shown in the app.
block_types <- c(
  select = "Select Columns",
  filter = "Filter Rows",
  group  = "Group By",
  mean   = "Calculate Mean",
  prop   = "Calculate Proportion"
)

# Lookup key for variable type. Left is the variable name and right is the variable type "value" (categorical vs. continuous).
# So, typing var_types[["height"]] would return "continuous".
var_types <- setNames(variable_info$type, variable_info$variable)

# Variable names split by type of variable (categorical vs. continuous)
cat_vars  <- names(var_types)[var_types == "categorical"]
cont_vars <- names(var_types)[var_types == "continuous"]

# Get the block's dropdown selection options in the whole dataset
# Left is the name of the block type, right is the "value" (aka the options)
block_choices <- list(
  select = names(gall_with_weather),
  filter = names(gall_with_weather),
  group  = cat_vars,
  mean   = cont_vars,
  prop   = cat_vars
)

## ╔══════════════════════╗
## │ JavaScript Callbacks │ ####
## ╚══════════════════════╝

# Sortable.js has functions to transfer data from the server back to R, but they would not work for multiple blocks of the same kind since there is no unique ID...

# When a block is dropped into the pipeline, give it a unique ID and tell the server (input$block_added).
js_on_add <- htmlwidgets::JS(
  "
  function (event) {
    var block = event.item;
    window.blockCounter = (window.blockCounter || 0) + 1;        // Increments by 1
    var id = 'block_' + Date.now() + '_' + window.blockCounter;  // Give each block a unique ID defined by the date and its number
    block.id = id;                                               // Give the block the ID
    block.setAttribute('data-id', id);                           // Set the ID as an attribute Shiny can access
    Shiny.setInputValue('block_added',                           // Send this input to Shiny
      { id: id, type: block.getAttribute('data-type') });        // Shiny receives the ID and block type (filter, mean, etc.)
  }"
)

# When the pipeline changes, send the order of the block IDs (input$pipeline_order).
js_send_order <- htmlwidgets::JS(
  "
  function (event) {
    Shiny.setInputValue('pipeline_order', this.toArray());       // Sends the order of blocks to Shiny
  }"
)

## ╔═════════════════════════╗
## │ UI Functions for Blocks │ ####
## ╚═════════════════════════╝
# Using Bootstrap's classes instead of CSS

make_block <- function(type) {
  div(
    class = paste0("block bg-", block_colors[[type]], " rounded p-2 my-1"),
    style = htmltools::css(cursor = "grab"),            # Hand cursor when hovering over a block
    `data-type` = type,                                 # Save the type into a variable
    div(class = "block-title d-flex gap-3 align-items-center", bs_icon(block_icons[[type]]), block_types[[type]]),
    div(class = "block-body")
  )
}

# Bootstrap color for each block type
# Left is the name used by the server and right is the "value" for the Bootstrap colors
block_colors <- c(
  select = "primary",                                   # Blue
  filter = "success",                                   # Green
  group  = "warning",                                   # Yellow
  mean   = "danger",                                    # Red
  prop   = "info"                                       # Light blue
)

# Bootstrap icon for each block type
block_icons <- c(
  select = "list-check",
  filter = "funnel",
  group  = "collection",
  mean   = "calculator",
  prop   = "percent"
)

# ヽ(・∀・)ﾉ ####
# ░░░░░░░░░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ░ User Interface ░ ###################################################
# ░░░░░░░░░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

## ┌────────────────┐
## │ Navigation Bar │
## └────────────────┘

ui <- page_navbar(
    title = "Gall Fly Lab Block Wrangling",
    footer = div(class = "text-center text-secondary small py-3", "Kinglee Tham 2026"),
    fillable = FALSE,

    ## ┌──────────────────────┐
    ## ║ Main Tab (Wrangling) ║ ####
    ## └──────────────────────┘

    nav_panel(
      "Data Wrangling",

      p("This public app uses a truncated unpublished data set with permission of Dr. Thea Kristensen.",
        style = "color: red;"),

      ### │                         │
      ### │ Row 1: Pipeline Builder │ ####
      ### │                         │

      card(
        card_header(h4("Pipeline Builder")),
        layout_columns(
          col_widths = c(5, 2, 5),

          # --- Left Column: Block Library ---
          card(
            card_header("Block Library: drag from here", class = "border-bottom-0"),
            # Create one block for every item in block_types
            div(
              id = "block_library",
              make_block("select"),
              make_block("filter"),
              make_block("group"),
              make_block("mean"),
              make_block("prop")
            )
          ),

          # ----⠀⠀⠀⠀Middle Column: Arrow⠀⠀⠀⠀----
          div(
            # Bootstrap classes to align the arrow in the middle
            class = "d-flex align-items-center justify-content-center h-100 text-secondary",
            bs_icon("arrow-right", size = "3em")
          ),

          # ----⠀⠀⠀⠀Right Column: Pipeline⠀⠀⠀⠀----
          card(
            card_header("Pipeline: drop blocks here", class = "border-bottom-0"),
            div(id = "pipeline_blocks", style = htmltools::css(min_height = "300px"))
          )
        ),

        # ----⠀⠀⠀⠀Trash Bin Row: unwanted blocks⠀⠀⠀⠀----
        card(
          class = "border-danger position-relative",
          card_header("Drop here to remove a block", class = "border-bottom-0 justify-content-center text-danger"),
          div(class = "text-center text-danger", bs_icon("trash", size = "3em")),
          div(id = "trashbin", class = "position-absolute top-0 start-0 w-100 h-100")
        )
      ),

      ###⠀⠀│                                 │
      ###⠀⠀⠀ Sortable Settings for Library   ⠀ ####
      ###⠀⠀│                                 │

      sortable_js("block_library", options = sortable_options(
        group = list(                                   # Acts in a group
          name = "block_group",                         # Can interact with the pipeline
          pull = "clone",                               # Dragging out creates a copy
          put  = FALSE                                  # Blocks cannot be dragged into the library
        ),
        sort = FALSE                                    # Standalone setting: blocks cannot be reordered in the library
      )),

      ###⠀⠀│                                          │
      ###⠀⠀⠀ Sortable Settings for Pipeline and Trash ⠀ ####
      ###⠀⠀│                                          │

      sortable_js(
        "pipeline_blocks",
        options = sortable_options(
          group = list(name = "block_group", pull = TRUE, put = TRUE),
          # Individual settings for each pipeline block
          onAdd    = js_on_add,                         # New block -> give it an ID
          onSort   = js_send_order,                     # Sort -> resend the order
          onRemove = js_send_order                      # Block removed -> resend the order
        )
      ),
      sortable_js(
        "trashbin",
        options = sortable_options(
          group = list(name = "block_group", pull = FALSE, put = TRUE),
          # Standalone settings
          sort  = FALSE,                                # Blocks cannot be reordered
          onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
        )
      ),

      ### │                                    │
      ###⠀│ Row 2: Wrangled Dataset / Download │ ####
      ### │                                    │

      card(
        card_header(h2("Result")),
        DTOutput("result_table"),
      ),
      downloadButton(outputId = "downloadData", label = "Download as Excel")
    ),

    ## ┌───────────┐
    ## ║ About Tab ║ ####
    ## └───────────┘


    nav_panel(
      "About",

      p("This public app uses a truncated unpublished data set with permission of Dr. Thea Kristensen.",
        style = "color: red;"),

      ### │              │
      ###⠀│ Row 1: About │ ####
      ### │              │

      card(
        h2("About"),
        p("The dplyr package (https://dplyr.tidyverse.org/) in R provides a set of functions that allow users to manipulate data sets, including very large ones. This app recreates some of the most common dplyr functions that biologists may need for more robust statistical analyses. By using visual code blocks, scientists can focus on building logical skills for data analysis without dealing with syntax errors associated with traditional coding."),
        p("A \"tidy\" dataset should be organized in a table format so that each variable is a column (a single measurement or trait observed in each observation) and each observation is a row (a single unit). Luckily for you, this dataset is already in a tidy format!"),
        tags$ul(
          tags$li(tags$code("select"), " picks variables (columns) based on their name"),
          tags$li(tags$code("filter"), " picks observations (rows) based on a certain value"),
          tags$li(tags$code("proportion"), " calculates the fraction of the observations that fall into a specific category (such as RED or BLUE) of a categorical variable (such as COLOR)"),
          tags$li(tags$code("mean"), " calculates the mean of a quantitative variable"),
          tags$li(tags$code("group by"), " comes before ", tags$code("proportion"), " and ", tags$code("mean"), ", which allows you to perform operations \"by group\" (such as mean temperature by season)")
        ),
        p("To use this app, drag blocks from the library (left column) to the pipeline (right column). Blocks can be removed by dragging them into the trash bin. The pipeline runs blocks from top to bottom, and the data table at the bottom of the screen updates according to the pipeline. A typical workflow often looks like this:"),
        p(tags$code("select -> filter -> group by -> calculate mean/proportion")),
      ),

      ###⠀⠀│                    │
      ###⠀⠀⠀ Purpose of Project ⠀ ####
      ###⠀⠀│                    │

      card(
        h2("Purpose"),
        p("As a biology student, I noticed that many of us came up with research questions without knowing what data we would need to answer them or how we would even manipulate the dataset to find certain statistics. Part of the problem was the dataset itself, since we had very little information on weather conditions. Naturally, some students wanted to know whether bird predation happened more often in warmer or colder months, but the dataset could not answer that."),
        p("This app tackles both problems. First, I pulled weather data from NOAA and merged it with the field data to make a richer dataset. Second, the app lets students and teaching assistants work through the logic of organizing data without knowing how to code in R. As you add blocks, the data table updates right away, so you can see exactly what each step does.")
      ),

      ###⠀ │                │
      ###⠀⠀⠀ Variable Table ⠀ ####
      ### ⠀│                │

      card(
        h2("List of Variables"),
        p("Fall weather data includes October and November of the same calendar year."),
        p("Spring weather data includes December of the previous calendar year, and January and February of the current calendar year."),
        DTOutput("variable_table")
      ),

      ###⠀⠀│                   │
      ###⠀⠀⠀ Raw Dataset Table ⠀ ####
      ###⠀⠀│                   │

      card(
        h2("Entire Raw Dataset"),
        p("Depending on the year of data collection, some variables might be missing."),
        DTOutput("dataset_table")
      )
    ),

    ## ┌─────────────────────┐
    ## ║ Acknowledgments Tab ║ ####
    ## └─────────────────────┘

    nav_panel(
      "Acknowledgments",

      p("This public app uses a truncated unpublished data set with permission of Dr. Thea Kristensen.",
        style = "color: red;"),

      card(
        card_header(h2("R Packages Used")),
        p("For this Shiny application:"),
        tags$ul(
          tags$li(tags$a(href = "https://github.com/rstudio/bsicons", "bsicons", target = "_blank"), " - app icons"),
          tags$li(tags$a(href = "https://rstudio.github.io/bslib/", "bslib", target = "_blank"), " - modern UI integrating CSS-like components"),
          tags$li(tags$a(href = "https://dplyr.tidyverse.org/", "dplyr", target = "_blank"), " - the data wrangling package that this app aims to recreate!"),
          tags$li(tags$a(href = "https://rstudio.github.io/DT/", "DT", target = "_blank"), " - render data tables"),
          tags$li(tags$a(href = "https://rstudio.github.io/htmltools/", "htmltools", target = "_blank"), " - dynamic cursor"),
          tags$li(tags$a(href = "https://shiny.posit.co/", "shiny", target = "_blank"), " - web app"),
          tags$li(tags$a(href = "https://datastorm-open.github.io/shinymanager/", "shinymanager", target = "_blank"), " - authentication system"),
          tags$li(tags$a(href = "https://rstudio.github.io/sortable/", "sortable", target = "_blank"), " - drag and drop functions"),
          tags$li(tags$a(href = "https://docs.ropensci.org/writexl/", "writexl", target = "_blank"), " - exporting data as an Excel file")
        ),
        p("For data scraping and wrangling:"),
        tags$ul(
          tags$li(tags$a(href = "https://dplyr.tidyverse.org/", "dplyr", target = "_blank"), " - manipulating and summarizing raw data"),
          tags$li(tags$a(href = "https://httr2.r-lib.org/", "httr2", target = "_blank"), " - sending HTTP requests to fetch weather data from the National Oceanic and Atmospheric Administration's API"),
          tags$li(tags$a(href = "https://readxl.tidyverse.org/", "readxl", target = "_blank"), " - reading the initial raw dataset"),
          tags$li(tags$a(href = "https://tidyr.tidyverse.org/", "tidyr", target = "_blank"), " - restructuring and joining raw data with weather data")
        )
      ),
      card(
        card_header(h2("Weather Data Sources")),
        p("The National Oceanic and Atmospheric Administration's API provides access to the Global Historical Climatology Network - Daily and Global Summary of the Month datasets."),
        tags$ul(
          tags$li(
            tags$a(href = "https://www.ncdc.noaa.gov/cdo-web/webservices/v2",
                   "NOAA Climate Data Online (CDO) Web Services API v2 Documentation", target = "_blank")
          ),
          tags$li(
            tags$a(href = "https://www.ncei.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf",
                   "Global Historical Climatology Network – Daily (GHCND)", target = "_blank"),
            " - daily minimum (TMIN) and maximum (TMAX) temperatures, used to derive the warmest minimum and coldest maximum temperatures"
          ),
          tags$li(
            tags$a(href = "https://www.ncei.noaa.gov/pub/data/cdo/documentation/gsom_doc.pdf",
                   "Global Summary of the Month (GSOM)", target = "_blank"),
            " - monthly extremes, averages, and totals for temperature, precipitation, and snowfall"
          )
        )
      ),
      card(
        card_header(h2("Field Data")),
        p("The field data were collected by the faculty and students of the Amherst College BIOL181 - Adaptation & the Organism course from 1995 - present.")
      ),
      card(
        card_header(h2("Thank you!")),
        p("I am incredibly grateful to Dr. Thea Kristensen (Biology), Prof. Katharine Correia (Statistics), and Prof. Amy Wagaman (Statistics) for their support during my time as an undergraduate student at Amherst College! Thea -- thank you so much for bolstering my interest in science and scientific communication during my first biology lab. To Prof. Correia and Prof. Wagaman, thank you for teaching me about the fundamentals of statistical theory and teaching me how to even use R! If you asked me during my first year what I would major in at Amherst, I would never have guessed that I would end up somewhere between biology and statistics.")
      )
    )
  )

# (.❛ ᴗ ❛.) ####
# ░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ░ Server ░ ###################################################
# ░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

server <- function(input, output, session) {

  output$variable_table <- renderDT({
    variable_info
  }, options = list(paging = FALSE))
  output$dataset_table <- renderDT({
    gall_with_weather
  }, options = list(scrollX = TRUE))

  ## ┌─────────────────────────┐
  ## │ Keeping Track of Blocks │ ####
  ## └─────────────────────────┘

  # create a lookup table with [name] and type of block (value)
  # e.g. block_type[block4] -> mean
  block_type <- new.env()
  n_blocks_added <- reactiveVal(0) # initialize a reactive value starting at 0

  # IDs of the blocks in the pipeline, in order (top to bottom)
  pipeline_ids <- reactive({
    n_blocks_added()                                    # If the block counter value changes, re run this block of code
    ids <- unlist(input$pipeline_order)                 # Get the list of block ids in order
    if (is.null(ids))                                   # If no blocks, then just return empty list
      return(character(0))
    ids[ids %in% ls(block_type)]                        # Else return the list
  })

  # A block was dropped into the pipeline -> update the server
  observeEvent(input$block_added, {
    id   <- input$block_added$id                        # Get ID of the newest block dropped
    type <- input$block_added$type                      # Get type of the newest block dropped
    assign(id, type, envir = block_type)                # Save the ID (name) and type (value) in the lookup table
    n_blocks_added(n_blocks_added() + 1)                # Index by 1

    insertUI(                                           # Puts the dropdown div inside the block
      selector = paste0("#", id, " .block-body"),       # Pointer to the block body
      ui = div(
        class = "rounded p-2 mt-2",
        uiOutput(paste0(id, "_var_ui")),                # Generic reactive dropdown that renderUI fills later
        if (type == "filter")
          uiOutput(paste0(id, "_filter_input")),        # Slider or value picker for filter blocks
        div(class = "small opacity-50", textOutput(paste0(id, "_info")))
      )
    )

    # Update the selection options in the block depending on the type of block
    output[[paste0(id, "_var_ui")]] <- renderUI({
      ids <- pipeline_ids()
      pos <- match(id, ids)
      req(!is.na(pos))                                  # Wait until the block is in the pipeline
      upstream <- run_blocks(ids[seq_len(pos - 1)])     # Data coming INTO this block
      allowed  <- intersect(block_choices[[type]], names(upstream))

      if (type %in% c("select", "group")) {             # Multi-choice dropdowns for select and group
        input_id <- paste0(id, "_vars")
        selectInput(
          inputId  = input_id,
          label    = switch(type,                       # Different labels for each type
                            select = "Keep these variables:",
                            group  = "Group by these variables:"),
          choices  = allowed,
          selected = intersect(isolate(input[[input_id]]), allowed),  # Keep old picks that still exist (empty at first)
          multiple = TRUE
        )
      } else {                                          # Single-choice dropdowns for filter,  mean, and prop
        input_id <- paste0(id, "_var")
        old <- isolate(input[[input_id]])               # Get the inputs and save it somewhere
        selectInput(
          inputId  = input_id,
          label    = switch(type,                       # Different labels for each type
                            filter = "Within this variable:",
                            mean   = "Calculate the mean of this column:",
                            prop   = "Calculate the proportion of this column:"),
          choices  = c("Choose a column" = "", allowed), # "" as the first choice makes the dropdown start empty
          selected = if (isTRUE(old %in% allowed)) old   # If the inputs are in the allowed inputs, keep the inputs
          else "" # Otherwise, if the inputs are not, then remove it
        )
      }
    })

    # Status line contains the rows and columns at point in the pipeline
    output[[paste0(id, "_info")]] <- renderText({
      ids <- pipeline_ids()                             # Get the block IDs in the pipeline
      pos <- match(id, ids)                             # Get the position of this block ID
      req(!is.na(pos))                                  # Stop running if the block isn't in the pipeline
      d <- run_blocks(ids[seq_len(pos)])                # Run the blocks up to and including this one
      paste0(nrow(d), " row(s) and ", ncol(d), " column(s)")
    })

    # Filter is giving me more problems :( Making a specific chunk for this...
    if (type == "filter") {
      last_var <- NULL                                  # Declare which variable the filter input was last drawn for
      output[[paste0(id, "_filter_input")]] <- renderUI({
        var <- input[[paste0(id, "_var")]]              # Saves the selected variable name
        req(var)                                        # Stops here while the selected variable name is still empty ("")
        ids <- pipeline_ids()
        pos <- match(id, ids)
        req(!is.na(pos))
        upstream <- run_blocks(ids[seq_len(pos - 1)])   # Run blocks above and fetch data coming INTO this filter
        req(var %in% names(upstream))
        values <- upstream[[var]]
        same_var <- identical(var, last_var)            # Boolean on whether the user changed the selection
        last_var <<- var                                # Update last_var either way

        # Next, change update the selections depending on a variable's data type
        if (var_types[[var]] == "continuous") {         # If the variable name corresponds to continuous type
          sliderInput(                                  # Slider still exists
            paste0(id, "_range"),
            "Keep values between:",
            min = min(values, na.rm = TRUE),
            max = max(values, na.rm = TRUE),
            value = if (same_var &&                     # If the selection is unchanged and the selection is null,
                        !is.null(isolate(input[[paste0(id, "_range")]])))
              isolate(input[[paste0(id, "_range")]])    # Keep it the same
            else
              range(values, na.rm = TRUE)               # If the selection changed, update it
          )
        } else {                                        # Else if the variable name corresponds to categorical type
          selectInput(
            paste0(id, "_levels"),
            "Keep these values:",
            choices = sort(unique(values)),             # Available choices
            selected = if (same_var)                    # If the variable selection hasn't changed, keep the slider's values
              intersect(isolate(input[[paste0(id, "_levels")]]), sort(unique(values))),
            multiple = TRUE                             # If the variable selection changed, reset the slider's values
          )
        }
      })
    }
  })

  ## ┌──────────────────┐
  ## │ Dataset Pipeline │ ####
  ## └──────────────────┘

  run_blocks <- function(ids) {                         # Basically run all transformations on the data set
    data <- gall_with_weather
    for (index in seq_along(ids)) {
      # In order of blocks from top to bottom, save the ID of the block, block type, and variable(s) selected
      id   <- ids[index]
      type <- block_type[[id]]
      var  <- input[[paste0(id, "_var")]]
      vars <- input[[paste0(id, "_vars")]]
      if (identical(var, "")) var <- NULL               # Empty dropdown (with value of just "") means
                                                        # nothing picked yet, so just keep it the same
      # See if any variables are missing due to blocks above being updated
      missing <- setdiff(c(var, vars), names(data))     # Missing column names is just the difference between selected variables vs the actual data
      if (length(missing) != 0) {
        attr(data, "broken_pos") <- index               # Remember the position of the block that broke
        attr(data, "missing")    <- missing             # Columns the block wanted

        showNotification(
          paste0(
            "Invalid Pipeline: Block #",
            index,
            " is missing column(s) \"",
            paste(missing, collapse = ", "),
            "\", which were removed by a block above :("
          ),
          type = "warning",
          duration = 5,
          id = "dummy"                                  # Give the notification an ID so it doesn't spam the screen with the same message
        )

        break                                           # Stop running the blocks below it
      }

      # Select block functionality
      if (type == "select" && length(vars) > 0) {
        data <- select(data, all_of(vars))

        # Filter block functionality with cases
      } else if (type == "filter" && !is.null(var)) {   # If the user selected anything
        if (var_types[[var]] == "continuous") {         # See if the user selected a continuous variable
          r <- input[[paste0(id, "_range")]]            # and apply the filter
          if (!is.null(r))
            data <- filter(data, .data[[var]] >= r[1], .data[[var]] <= r[2])
        } else {                                        # Otherwise, the user selected a categorical variable
          keep <- input[[paste0(id, "_levels")]]        # See what values are selected and apply the filter
          if (length(keep) > 0)
            data <- filter(data, .data[[var]] %in% keep)
        }

        # Group block functionality
      } else if (type == "group" && length(vars) > 0) {
        data <- group_by(data, across(all_of(vars)))

        # Mean block functionality
      } else if (type == "mean" && !is.null(var)) {
        data <- summarise(data, mean = mean(.data[[var]], na.rm = TRUE),
                          n    = sum(!is.na(.data[[var]]))
        )

        # Proportion block functionality
      } else if (type == "prop" && !is.null(var)) {
        data <- summarise(data, proportion = mean(.data[[var]] == 1, na.rm = TRUE),
                          n    = sum(!is.na(.data[[var]]))
        )
      }
    }
    data                                                # Need to include this because this is what is returned
  }
  result_data <- reactive(run_blocks(pipeline_ids()))

  ## ┌──────────────────────────┐
  ## │ Wrangled Dataset Preview │ ####
  ## └──────────────────────────┘

  output$result_table <- renderDT({
    # Get the position of the block that broke the table, if applicable
    broken_pos <- attr(result_data(), "broken_pos")
    if (is.null(broken_pos)) {                          # If no block is broken, return the table
      result_data()
    } else {                                            # Else show an error message by creating a data frame
      data.frame(
        Error = paste0(
          "Invalid Pipeline: Block #",
          broken_pos,
          " is missing column(s) \"",
          paste(attr(result_data(), "missing"), collapse = ", "),
          "\", which were removed by a block above :("
        )                                               # This will basically flash for a second, but it stops the code from crashing
      )
    }
  }, options = list(scrollX = TRUE))

  ## ┌─────────────────────────┐
  ## │ Download Dataset Button │ ####
  ## └─────────────────────────┘

  output$downloadData <- downloadHandler(               # downloadHandler takes a filename and content parameter
    filename = function() {                             # Wrapping code in function() so this only runs once the user clicks the button
      paste0("wrangled_gall_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(result_data(), file)                   # Write the dataset as an Excel sheet to the `file` that will be downloaded
    }
  )
}

# \(^ヮ^)/) ####
# ░░░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ░ Launch!! ░ ###################################################
# ░░░░░░░░░░░░ ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

shinyApp(ui, server)