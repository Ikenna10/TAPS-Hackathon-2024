#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
#library(shiny.semantic)

library(bslib)
library(shinyWidgets)
library(shinydashboard)

library(shiny.info)
library(spsComps)
library(dplyr)
library(ggplot2)
library(scales)





ui = bslib::page_sidebar(
  
  shiny.info::version(),
  
  #includeCSS("style.css"),
  #
  
  title = "IrrigSense",
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "yeti",
    #base_font = font_google("Pacifico"),
    base_font = bslib::font_google("Inter"),
    #heading_font = font_google("Pacifico"),
    primary = "#512888",
    secondary = "#a991d4",
    info = "#009CDC"
    #navbar_bg = "#593196"
  ),
  
  sidebar = bslib::sidebar(
    shinyjs::useShinyjs(),
    
    # shiny::div(shiny::HTML( <br><br>
    #                               ")),
    #            style = paste0("color: darkblue; font-size:", 12, "px")
    # ),
    
    shiny::selectInput(
      inputId = "crop",
      label = "Crop",
      choices = c("Corn"),
      selected = "Corn",
      width = "100%"),
    
    
    ##################################################
    ## Sorghum built as default
    ## Wheat options built as update in server fuction
    ##################################################
    
    
    # conditionalPanel(condition = "input.crop == 'Sorghum'",
    #
    #                  ),
    # Disabled added on Sept 10. Contrastr not available on the server
    
    shiny::radioButtons(
      inputId = "mat_grp",
      label = "Maturity Group",
      choices = c("Early" = 'early', 'Intermidiate' = 'mid', 'Late' = 'late'),
      selected = character(0),
      width = "100%"),
    
    
    shiny::conditionalPanel(
      condition = "input.mat_grp == 'early' || input.mat_grp == 'mid' || input.mat_grp == 'late'",
      
      shiny::dateInput("date6", "Date of Emergence:",
                       startview = "year"),
      
      ## Options for soil texture
      
      shiny::selectInput("stx",
                         choices = c(
                           "",
                           "Clay" = "clay",
                           "Sitty Clay" = "sitty_clay",
                           'Clay Loam' = 'clay_loam',
                           'Sandy Clay Loam' = 'sandy_clay_loam',
                           'Loam' = 'loam',
                           'Silt Loam' = 'silt_loam',
                           'Fine Sandy Loam' = 'fine_sandy_loam',
                           'Sandy Loam' = 'sandy_loam',
                           'Loamy Sand' = 'loam_sand',
                           'Sand' = 'sand'),
                         label = "Soil Texture Class",
                         selected = NULL,
                         width = "100%")
    ),
    
    
    
    
    
    
    
    
    
    
    
    ## Options if sand moisture type
    shiny::conditionalPanel(
      condition="input.stx == 'clay' || input.stx == 'sitty_clay' || input.stx == 'clay_loam' || input.stx == 'sandy_clay_loam'",
      shiny::numericInput("fc_clay",
                          
                          label = "Field Capacity",
                          value = 0.4,
                          step = 0.1,
                          width = "100%"),
      
      # Assign WP by soil texture
      shiny::conditionalPanel(
        condition = "input.stx == 'clay'",
        
        shiny::numericInput("wilt_pt_clay",
                            
                            label = "Wilting Point",
                            value = 0.28,
                            step = 0.01,
                            width = "100%")
        
      ),
      
      shiny::conditionalPanel(
        condition = "input.stx == 'sitty_clay'",
        
        shiny::numericInput("wilt_pt_sitty_clay",
                            
                            label = "Wilting Point",
                            value = 0.26,
                            step = 0.01,
                            width = "100%")
        
      ),
      
      
      
      shiny::conditionalPanel(
        condition = "input.stx == 'clay_loam'",
        
        shiny::numericInput("wilt_pt_clay_loam",
                            
                            label = "Wilting Point",
                            value = 0.24,
                            step = 0.01,
                            width = "100%")
        
      ),
      
      shiny::conditionalPanel(
        condition = "input.stx == 'sandy_clay_loam'",
        
        shiny::numericInput("wilt_pt_sandy_clay_loam",
                            
                            label = "Wilting Point",
                            value = 0.22,
                            step = 0.01,
                            width = "100%")
        
      )),
    
    
    
    # Silt loam
    
    shiny::conditionalPanel(
      condition="input.stx == 'silt_loam' ",
      shiny::numericInput("fc_silt_loam",
                          
                          label = "Field Capacity",
                          value = 0.39,
                          step = 0.1,
                          width = "100%"),
      
      
      
      
      shiny::numericInput("wilt_pt_silt_loam",
                          
                          label = "Wilting Point",
                          value = 0.19,
                          step = 0.01,
                          width = "100%")
    ),
    
    
    # Loam
    
    shiny::conditionalPanel(
      condition="input.stx == 'loam' ",
      
      shiny::numericInput("fc_loam",
                          
                          label = "Field Capacity",
                          value = 0.34,
                          step = 0.1,
                          width = "100%"),
      
      
      
      
      shiny::numericInput("wilt_pt_loam",
                          
                          label = "Wilting Point",
                          value = 0.16,
                          step = 0.01,
                          width = "100%")
    ),
    
    
    # Fine Sandy loam
    
    shiny::conditionalPanel(
      condition="input.stx == 'fine_sandy_loam' ",
      
      shiny::numericInput("fc_fine_sandy_loam",
                          
                          label = "Field Capacity",
                          value = 0.30,
                          step = 0.1,
                          width = "100%"),
      
      
      
      
      shiny::numericInput("wilt_pt_fine_sandy_loam",
                          
                          label = "Wilting Point",
                          value = 0.13,
                          step = 0.01,
                          width = "100%")
    ),
    
    
    # Sandy loam
    
    shiny::conditionalPanel(
      condition="input.stx == 'sandy_loam' ",
      
      shiny::numericInput("fc_sandy_loam",
                          
                          label = "Field Capacity",
                          value = 0.23,
                          step = 0.1,
                          width = "100%"),
      
      
      
      
      shiny::numericInput("wilt_pt_sandy_loam",
                          
                          label = "Wilting Point",
                          value = 0.09,
                          step = 0.01,
                          width = "100%")
    ),
    
    
    # Loamy sand
    
    shiny::conditionalPanel(
      condition="input.stx == 'loam_sand' ",
      
      shiny::numericInput("fc_loam_sand",
                          
                          label = "Field Capacity",
                          value = 0.18,
                          step = 0.1,
                          width = "100%"),
      
      
      
      
      shiny::numericInput("wilt_pt_loam_sand",
                          
                          label = "Wilting Point",
                          value = 0.07,
                          step = 0.01,
                          width = "100%")
    ),
    
    # Sandy loam
    
    shiny::conditionalPanel(
      condition="input.stx == 'sand' ",
      
      shiny::numericInput("fc_sand",
                          
                          label = "Field Capacity",
                          value = 0.12,
                          step = 0.1,
                          width = "100%"),
      
      
      
      
      shiny::numericInput("wilt_pt_sand",
                          
                          label = "Wilting Point",
                          value = 0.05,
                          step = 0.01,
                          width = "100%")
    ),
    
    # Data Source
    
    shiny::conditionalPanel(
      
      condition = "input.stx == 'clay' | input.stx == 'sitty_clay' | input.stx == 'clay_loam' | input.stx == 'sandy_clay_loam' | input.stx == 'sand' | input.stx == 'fine_sandy_loam' | input.stx == 'loam' | input.stx == 'loam_sand' | input.stx == 'sandy_loam' | input.stx == 'silt_loam' ", 
      
      shiny::radioButtons('soil_mst',
                          choices = c('Import Data' = 'import_sm', 'Input Value (inches)' = 'input_sm'),
                          selected = character(0),
                          label = 'Soil Moisture'
      )
      
      
      
    ),
    
    ## Options for soil moisture - import
    shiny::conditionalPanel(
      condition="input.soil_mst == 'import_sm'",
      
      shiny::fileInput("upload_for_sm", "Soil moisture csv file", accept = c(".csv")),
      
      selectInput('sm_prec_input_date','Select the Date Column', ""),
      
      selectInput('sm_prec_input_soil_mst','Select the Moisture Column', ""),
      
      
      shiny::div(shiny::HTML(paste0("Upload CSV file type only.")),
                 style = paste0("color: darkblue; font-size:", 12, "px")
      )
    ),
    
    ## Options for soil moisture - import
    shiny::conditionalPanel(
      condition="input.soil_mst == 'input_sm'",
      
      numericInput('input_sm_value',
                   label = "Soil Moisture Value (inches)",
                   value = 0.32)
    ),
    
    
    
    shiny::conditionalPanel(
      condition = "input.soil_mst == 'input_sm' | input.soil_mst == 'import_sm'",
      
      shiny::radioButtons('et',
                          choices = c('Import Data' = 'import_et', 'Input Value (inches/day)' = 'input_et'),
                          selected = character(0),
                          label = 'Evapotranspiration'
      )
      
    ),
    
    
    
    ## Options for et - import
    shiny::conditionalPanel(
      condition="input.et == 'import_et'",
      
      shiny::fileInput("upload_for_et", "Choose evapotranspiration csv file", accept = c(".csv")),
      
      
      selectInput('et_input_date','Select the Date Column', ""),
      
      selectInput('et_input_et','Select the ET Column', ""),
      
      
      shiny::div(shiny::HTML(paste0("Upload CSV file type only.")),
                 style = paste0("color: darkblue; font-size:", 12, "px")
      )
    ),
    
    ## Options for et - input
    shiny::conditionalPanel(
      condition="input.et == 'input_et'",
      
      numericInput('input_et_value',
                   label = "Evapotranspiration (inches/day)",
                   value = 0.5)
    ),
    
    
    shiny::conditionalPanel(
      condition = "input.et == 'input_et' | input.et == 'import_et'",
      
      shiny::radioButtons('rain',
                          choices = c('Import Data' = 'import_rain', 'Input Value (inches/day)' = 'input_rain'),
                          selected = character(0),
                          label = 'Precipitation'
      )
      
    ),
    
    
    ## Options for et - import
    shiny::conditionalPanel(
      condition="input.rain == 'import_rain'",
      
      shiny::fileInput("upload_for_rain", "Choose precipitation csv file", accept = c(".csv")),
      
      selectInput('rain_input_date','Select the Date Column', ""),
      
      selectInput('rain_input_rain','Select the Rain Column', ""),
      
      
      shiny::div(shiny::HTML(paste0("Upload CSV file type only.")),
                 style = paste0("color: darkblue; font-size:", 12, "px")
      )
    ),
    
    ## Options for et - input
    shiny::conditionalPanel(
      condition="input.rain == 'input_rain'",
      
      numericInput('input_rain_value',
                   label = "Precipitation (inches/day)",
                   value = 0)
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ## If fst is selected
    ########################
    ########################
    ########################
    
    ########################
    ########################
    ########################
    
    
    
    shiny::actionButton("submit_query","Submit",
                        class = "btn btn-primary btn-block submit-btn",
                        shiny::icon("paper-plane")
    )
    
    # uiOutput("ui_for_hetgroup", width = "100%")
    
    
    
  ),
  
  # This outputs the dynamic UI component
  
  # this is ET and prep plot
  
  shiny::fluidRow(
    #shiny::h3("ET and Precipitation Plot", style = "text-align: center; margin-top: 20px;"),
    plotly::plotlyOutput("ui_two", width = "100%"),
    
  ), shiny::br(),
  
  # this is table
  
  shiny::fluidRow(
    #shiny::h3("Management Data", style = "text-align: center; margin-top: 20px;"),
    shiny::column(6, DT::DTOutput("ui_one")),
    
  ), shiny::br(),
  
  # this is deplection fraction and irrigation count
  shiny::fluidRow(
    #shiny::h3("Depletion Fraction and Irrigation Count", style = "text-align: center; margin-top: 20px;"),
    shiny::column(6, plotly::plotlyOutput("ui_three", height = "450px")),
    shiny::column(6, plotly::plotlyOutput("ui_four", height = "450px")),
    #shiny::column(6, plotly::plotlyOutput("fst_corr_female", height = "650px"))
  ), shiny::br(),
  
  #tableOutput("ui_one"),
  
  
  
  #plotOutput("ui_three", width = "100%"),
  
  #shinydashboard::dashboardBody(shiny::uiOutput("ui"))
  shiny::uiOutput("ui")
  
  
)

# Define UI for application that draws a histogram








# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  
  
  import_for_sm <- shiny::reactive({
    shiny::req(input$upload_for_sm)
    
    ext <- tools::file_ext(input$upload_for_sm$name)
    switch(ext,
           csv = read.csv(input$upload_for_sm$datapath, header = T), # changed this from vroom to read.csv - vroom caused some problems downstream
           shiny::validate("Invalid file; Please upload a .csv file")
    )
  })
  
  
  
  import_for_et <- shiny::reactive({
    shiny::req(input$upload_for_et)
    
    ext <- tools::file_ext(input$upload_for_et$name)
    switch(ext,
           csv = read.csv(input$upload_for_et$datapath, header = T), # changed this from vroom to read.csv - vroom caused some problems downstream
           shiny::validate("Invalid file; Please upload a .csv file")
    )
  })
  
  
  import_for_rain <- shiny::reactive({
    shiny::req(input$upload_for_rain)
    
    ext <- tools::file_ext(input$upload_for_rain$name)
    switch(ext,
           csv = read.csv(input$upload_for_rain$datapath, header = T), # changed this from vroom to read.csv - vroom caused some problems downstream
           shiny::validate("Invalid file; Please upload a .csv file")
    )
  })
  
  
  #Observe file being selected
  observeEvent(input$upload_for_sm, {
    
    #Store loaded data in reactive
    
    
    #Update select input
    #Update select input
    updateSelectInput(session, inputId = 'sm_prec_input_date', label = 'Select the Date Column', choices  = colnames(import_for_sm()), selected = "")
    
    updateSelectInput(session, inputId = 'sm_prec_input_soil_mst', label = 'Select the Moisture Column', choices  = colnames(import_for_sm()),  selected = "")
    
  })
  
  
  observeEvent(input$upload_for_et, {
    
    #Store loaded data in reactive
    
    #Update select input
    #Update select input
    updateSelectInput(session, inputId = 'et_input_date', label = 'Select the Date Column', choices  = colnames(import_for_et()), selected = "")
    
    updateSelectInput(session, inputId = 'et_input_et', label = 'Select the ET Column', choices  = colnames(import_for_et()), selected = "")
    
  })
  
  
  observeEvent(input$upload_for_rain, {
    
    #Store loaded data in reactive
    
    #Update select input
    #Update select input
    updateSelectInput(session, inputId = 'rain_input_date', label = 'Select the Date Column', choices  = colnames(import_for_rain()), selected = "")
    
    updateSelectInput(session, inputId = 'rain_input_rain', label = 'Select the Rain Column', choices  = colnames(import_for_rain()), selected = "")
    
  })
  
  
  shiny::observeEvent(input$submit_query, {
    
    #browser()
    # get inputs
    #
    #browser()
    
    get_mat = tolower(input$mat_grp)
    days_of_emergence = as.Date(input$date6)
    get_soil_tex = tolower(input$stx)
    
    # "input.stx == 'clay' | input.stx == 'sitty_clay' | input.stx == 'clay_loam' | input.stx == 'sandy_clay_loam' | input.stx == 'sand' | input.stx == 'fine_sandy_loam' | input.stx == 'loam' | input.stx == 'loam_sand' | input.stx == 'sandy_loam' | input.stx == 'silt_loam' "
    
    if(input$stx == 'clay'){
      field_capacity = input$fc_clay
      wilting_point = input$wilt_pt_clay
      
    }else if(input$stx == 'sitty_clay'){
      field_capacity = input$fc_clay
      wilting_point = input$wilt_pt_sitty_clay
      
    }else if(input$stx == 'clay_loam'){
      field_capacity = input$fc_clay
      wilting_point = input$wilt_pt_clay_loam
      
    }else if(input$stx == 'sandy_clay_loam'){
      field_capacity = input$fc_clay
      wilting_point = input$wilt_pt_sandy_clay_loam
      
    }else if(input$stx == 'sand'){
      field_capacity = input$fc_sand
      wilting_point = input$wilt_pt_sand
      
    }else if(input$stx == 'loam'){
      field_capacity = input$fc_loam
      wilting_point = input$wilt_pt_loam
      
    }else if(input$stx == 'fine_sandy_loam'){
      field_capacity = input$fc_fine_sandy_loam
      wilting_point = input$wilt_pt_fine_sandy_loam
      
    }else if(input$stx == 'loam_sand'){
      field_capacity = input$fc_fine_sandy_loam
      wilting_point = input$wilt_pt_fine_sandy_loam
      
    }else if(input$stx == 'sandy_loam'){
      field_capacity = input$fc_fine_sandy_loam
      wilting_point = input$wilt_pt_fine_sandy_loam
      
    }else if(input$stx == 'silt_loam'){
      field_capacity = input$fc_fine_sandy_loam
      wilting_point = input$wilt_pt_fine_sandy_loam
      
    }
    
    
    # field_capacity = input$fc
    # wilting_point = input$wilt_pt
    
    # browser()
    # get soil moisture data
    if(input$soil_mst == 'import_sm'){
      
      soil_moisture_date = input$sm_prec_input_date
      
      soil_moisture = input$sm_prec_input_soil_mst
      
      take_sm = c(soil_moisture_date, soil_moisture)
      
      soil_moisture_dta = import_for_sm() %>% select(all_of(take_sm))
      
      colnames(soil_moisture_dta) = c('date', 'soil_moisture')
      
      # Convert 'Date' to Date format and calculate the daily average for water content
      soil_moisture_dta$date <- as.Date(soil_moisture_dta$date, format="%d/%m/%Y")
      daily_sensor_data <- soil_moisture_dta %>%
        group_by(date) %>%
        summarize(daily_moisture = mean(soil_moisture, na.rm = TRUE))
      
      daily_sensor_data$daily_moisture <- daily_sensor_data$daily_moisture / 100
      
      
    }else if(input$soil_mst == 'input_sm'){
      water_content = input$input_sm_value
    }
    
    
    
    # get et data
    if(input$et == 'import_et'){
      
      et_date = input$et_input_date
      
      evap_tp = input$et_input_et
      
      take_et = c(et_date, evap_tp)
      
      et_dta = import_for_et() %>% select(all_of(take_et))
      
      colnames(et_dta) = c('date', 'evapotranspiration')
      
      
      et_dta$evapotranspiration <- et_dta$evapotranspiration / 25.4
      
    }else if(input$et == 'input_et'){
      evapotranspiration = input$input_et_value
    }
    
    
    
    # get rainfall data
    if(input$rain == 'import_rain'){
      
      rain_date = input$rain_input_date
      
      rain = input$rain_input_rain
      
      take_rain = c(rain_date, rain)
      
      rain_dta = import_for_rain() %>% select(all_of(take_rain))
      
      colnames(rain_dta) = c('date', 'rain')
      
      
      rain_dta$rain <- rain_dta$rain / 25.4
      
    }else if(input$rain == 'input_rain'){
      rain = input$input_et_value
    }
    
    
    
    all_data <- data.frame(
      day = as.Date(rain_dta$date, format="%m/%d/%Y"), # Adjusted fo, # Adjust format if needed
      water_content = daily_sensor_data$daily_moisture,
      rainfall = rain_dta$rain,
      evapotranspiration = et_dta$evapotranspiration#,
      #depletion_fraction =(field_capacity - daily_sensor_data$daily_moisture) / (field_capacity - wilting_point)
    )
    
    
    
    
    
    # Fixed params
    root_depth_min <- 4 # Minimum root depth in inches
    root_depth_max <- 12 # Maximum root depth in inches
    days_to_max_root_depth <- 70 # Days to reach max root depth
    
    #present_date = as.Date(Sys.Date())
    
    
    
    
    
    
    # Global Functions
    # Function to calculate available water content
    
    #browser()
    
    ##################################################################
    ##################################################################
    ######################This is when value is inputed ##############
    ##################################################################
    
    
    
    
    # if(input$et == 'input_et' & input$soil_mst == 'input_sm'){
    #   
    #   awc <- calculate_awc(field_capacity, wilting_point)
    #   
    #   dag <- as.numeric(day[i] - days_of_emergence) #as.Date("2023-08-28")) # Assuming germination date is Aug 28, 2023
    #   dag <- max(dag, 0) # Ensure non-negative value
    #   
    #   # Perform calculations for each variable
    #   growth_factor <- calculate_root_growth(dag, 70)  # Example: 30 days to max root depth
    #   rd <- calculate_root_depth(growth_factor)
    #   taw <- calculate_taw(awc, rd)
    #   df <- calculate_depletion_fraction(field_capacity, wilting_point, data$water_content[i])
    #   ad <- calculate_allowable_depletion(taw, df)
    #   
    #   # Get the current row's depletion fraction and rainfall
    #   #depletion_fraction <- data$depletion_fraction[i]
    #   
    #   rainfall <- data$rainfall[i]
    #   
    #   # Check if depletion fraction is >= 0.5
    #   if (df >= 0.5) {
    #     paste("No")
    #     i_a = abs(ad - rainfall)
    #     # Perform rainfall check
    #     if (rainfall >= 1) {
    #       paste("No")
    #       i_a = abs(ad - rainfall)
    #       #print(paste("Day:", test_data$day[i], "- No need for irrigation"))
    #     } else {
    #       #print(paste("Day:", test_data$day[i], "- Consider irrigation - rainfall insufficient"))
    #       paste("Yes")
    #       
    #       # Calculate 'i_a' as the difference between allowable depletion and rainfall
    #       #i_a = abs(results$allowable_depletion[i] - rainfall)
    #       i_a = abs(ad - rainfall)
    #       #print(paste("i_a (Allowable depletion - Rainfall):", i_a))
    #     }
    #   } else {
    #     #print(paste("Day:", test_data$day[i], "- No need for irrigation"))
    #     i_a = abs(ad - rainfall)
    #   }
    # 
    #   
    # 
    #   out = list(days_aft_em = dag, root_growth_factor = growth_factor, root_depth = rd, total_available_water=taw, depletion_fraction = df, allowable_depletion = ad, max_days_to_irrigate = tmax)
    # 
    # }
    
    ##################################################################
    ##################################################################
    ######################This is when csv is imported ##############
    ##################################################################
    
    
    # Function to calculate allowable depletion
    calculate_allowable_depletion <- function(total_available_water, depletion_fraction) {
      allowable_depletion <- total_available_water * depletion_fraction
      return(allowable_depletion)
    }
    
    
    # Define additional functions as placeholders for your logic
    calculate_awc <- function(field_capacity, wilting_point) {
      return(field_capacity - wilting_point)  # Example implementation
    }
    
    calculate_root_growth <- function(dag, days_to_max_root_depth) {
      rf = (dag / days_to_max_root_depth)
      if (rf > 1){
        rf = 1
      }
      return(rf)  # Example implementation
    }
    
    # Function to calculate rooting depth based on growth factor
    calculate_root_depth <- function(growth_factor) {
      root_depth <- root_depth_min + (root_depth_max - root_depth_min) * growth_factor
      return(root_depth)
    }
    
    
    calculate_taw <- function(awc, rd) {
      return(awc * rd)  # Example implementation
    }
    
    calculate_depletion_fraction <- function(field_capacity, wilting_point, water_content) {
      depletion_fraction <- (field_capacity - water_content) / (field_capacity - wilting_point)
      return(depletion_fraction)
    }
    
    
    # Main function to perform calculations for each day in the dataset
    perform_calculations <- function(data) {
      
      # Calculate constant available water content
      awc <- calculate_awc(field_capacity, wilting_point)
      
      # Initialize empty lists to store results for each calculation
      days_after_germination <- numeric()
      root_growth_factor <- numeric()
      root_depth <- numeric()
      total_available_water <- numeric()
      depletion_fraction <- numeric()
      allowable_depletion <- numeric()
      irrigation_amount <- numeric()
      moisture_status <- character()
      irrigation_decision <- character()  # For storing "YES" or "NO" actions
      
      # Identify the cutoff date for automatic "NO" (14 days before the last day)
      cutoff_date <- max(data$day) - 14
      
      # Initialize a variable to track the last irrigation date when "LOW" occurred
      last_irrigation_date <- as.Date(NA)  # Start with NA
      
      # Loop through each row in the data
      for (i in 1:nrow(data)) {
        # Sample day after germination calculation
        dag <- as.numeric(data$day[i] - days_of_emergence) #as.Date("2024-06-11"))
        dag <- max(dag, 0)  # Ensure non-negative value
        
        # Perform calculations for each variable
        growth_factor <- calculate_root_growth(dag, 70)
        rd <- calculate_root_depth(growth_factor)
        taw <- calculate_taw(awc, rd)
        df <- calculate_depletion_fraction(field_capacity, wilting_point, data$water_content[i])
        ad <- calculate_allowable_depletion(taw, df)
        
        # Get the current row's rainfall
        rainfall <- data$rainfall[i]
        
        # Determine irrigation_decision if we are at or past the cutoff date
        if (data$day[i] >= cutoff_date) {
          moisture_status <- c(moisture_status, "GOOD")  # Automatically set to GOOD
          irrigation_decision <- c(irrigation_decision, "NO")  # Automatically set to NO
        } else {
          # Determine irrigation decision based on conditions
          if (df >= 0.5) {
            if (rainfall >= 1) {
              moisture_status <- c(moisture_status, "GOOD")  # NO needed
              irrigation_decision <- c(irrigation_decision, "NO")  # No action needed
            } else {
              moisture_status <- c(moisture_status, "LOW")  # Irrigation needed
              
              # Check if at least seven days have passed since the last "LOW" decision
              if (is.na(last_irrigation_date) || as.numeric(data$day[i] - last_irrigation_date) >= 7) {
                irrigation_decision <- c(irrigation_decision, "YES")  # Irrigation action needed
                last_irrigation_date <- data$day[i]  # Update last irrigation date
              } else {
                irrigation_decision <- c(irrigation_decision, "NO")  # No irrigation due to <7 days
              }
            }
          } else {
            moisture_status <- c(moisture_status, "GOOD")  # No irrigation needed
            irrigation_decision <- c(irrigation_decision, "NO")  # No action needed
          }
        }
        
        # Calculate irrigation amount as difference between allowable depletion and rainfall
        i_a <- abs(ad - rainfall)
        
        # Append results to each list
        days_after_germination <- c(days_after_germination, dag)
        root_growth_factor <- c(root_growth_factor, growth_factor)
        root_depth <- c(root_depth, rd)
        total_available_water <- c(total_available_water, taw)
        depletion_fraction <- c(depletion_fraction, df)
        allowable_depletion <- c(allowable_depletion, ad)
        irrigation_amount <- c(irrigation_amount, i_a)
      }
      
      # Add calculated values to the data frame
      # out = list(days_after_germination= round(days_after_germination, 3), root_growth_factor= round(root_growth_factor,3), root_depth= round(root_depth,3),
      #            total_available_water= round(total_available_water,3), depletion_fraction=round(depletion_fraction,3), allowable_depletion=round(allowable_depletion,3), irrigation_amount=round(irrigation_amount,3))
      
      # Return a list of specified variables
      out <- list(
        days_after_germination = round(days_after_germination, 3),
        total_available_water = round(total_available_water,3),
        depletion_fraction = round(depletion_fraction,3),
        allowable_depletion = round(allowable_depletion,3),
        irrigation_amount = round(irrigation_amount,3),
        moisture_status = moisture_status,
        irrigation_decision = irrigation_decision
      )
      
      # data$days_after_germination <- days_after_germination
      # data$root_growth_factor <- root_growth_factor
      # data$root_depth <- root_depth
      # data$total_available_water <- total_available_water
      # data$depletion_fraction <- depletion_fraction
      # data$allowable_depletion <- allowable_depletion
      # data$irrigation_amount <- irrigation_amount
      # data$moisture_status <- moisture_status
      # data$irrigation_decision <- irrigation_decision  # Add irrigation_decision column
      
      out_new = do.call('cbind', out)
      
      out_new = out_new %>% as.data.frame()
      
      # colnames(out_new) = c('days_after_germination', 'root_growth_factor', 'root_depth', 'total_available_water', 'depletion_fraction', 'allowable_depletion', 'irrigation_amount')
      
      colnames(out_new) = c('days_after_germination', 'total_available_water', 'depletion_fraction', 'allowable_depletion', 'irrigation_amount', 'moisture_status', 'irrigation_decision')
      
      return(out_new)
    }
    
    # Run the calculations on the sample data
    results <- perform_calculations(all_data)
    
    results_out = all_data %>% select(day) %>% cbind(results)
    
    
    ###########################################################
    #############################################################
    
    # all_data <- data.frame(
    #   day = as.Date(rain_dta$date, format = "%m/%d/%Y"),
    #   water_content = as.numeric(daily_sensor_data$daily_moisture),
    #   rainfall = as.numeric(rain_dta$rain),
    #   evapotranspiration = as.numeric(et_dta$evapotranspiration)
    # )
    # # Ensure date columns are in Date format
    # all_data$day <- as.Date(all_data$day, format = "%m/%d/%Y")
    # #daily_evapotranspiration$Timestamp <- as.Date(daily_evapotranspiration$Timestamp, format = "%m/%d/%Y")
    # 
    # # Calculate range limits
    precip_min <- min(all_data$rain, na.rm = TRUE)
    precip_max <- max(all_data$rain, na.rm = TRUE)
    evapo_min <- min(all_data$evapotranspiration, na.rm = TRUE)
    evapo_max <- max(all_data$evapotranspiration, na.rm = TRUE)
    # 
    # 
    # 
    # # Define a scaling factor to align the evapotranspiration data with precipitation scale
    scaling_factor <- (precip_max - precip_min) / (evapo_max - evapo_min)
    # 
    #browser()
    # #Create the plot
    p1 = ggplot() +
      geom_line(data = all_data, aes(x = day, y = rainfall, color = "Precipitation"), linewidth = 1) +
      geom_line(data = all_data, aes(x = day, y = evapotranspiration * scaling_factor, color = "Evapotranspiration"), linewidth = 1) +
      scale_y_continuous(
        name = "(inches/day)",
        limits = c(precip_min, precip_max),
        sec.axis = sec_axis(~ . / scaling_factor, name = "Evapotranspiration (inches/day)")
      ) +
      scale_color_manual(values = c("Precipitation" = "blue", "Evapotranspiration" = "red")) +
      labs(title = " Precipitation and Evapotranspiration (inches/day)", x = "Date", color = "Legend") +
      theme_minimal() +
      theme(plot.title = element_text(size = 24, vjust = 0.5, face = "bold"),
            axis.title.y.left = element_text(color = "blue", size = 18),
            axis.title.y.right = element_text(color = "red", size = 18))
    
    p1 = plotly::ggplotly(p1)


    ### Outputs Plot ###
    
    #browser()
    
    # Create a line plot of depletion_fraction over time
    p2 = ggplot(results_out, aes(x = day, y = as.numeric(depletion_fraction))) +
      geom_line(color = "blue", linewidth = 1) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +  # Horizontal line at y = 0.5
      labs(title = "Depletion Fraction Over Time",
           x = "Date",
           y = "Depletion Fraction") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 24, vjust = 0.5, face = "bold"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 14)
      )
    
    
    p2 = plotly::ggplotly(p2)
    
    #browser()
    # Function to generate bar plot for irrigation count and total irrigation amount
    plot_irri <- function(results_df) {
      # Create a data frame from results to facilitate data manipulation
      irrigate_data <- data.frame(
        irrigation_decision = results$irrigation_decision,
        irrigation_amount = as.numeric(results$irrigation_amount) # Ensure numeric type
      )
      
      # Filter data to only include entries with "Irrigate" action
      irrigate_summary <- irrigate_data %>%
        filter(irrigation_decision == "YES") %>%
        summarize(
          irrigate_count = n(),
          total_irrigation_amount = sum(irrigation_amount, na.rm = TRUE) # Handle any NAs
        )
      
      # Convert summary data to long format for plotting
      irrigate_summary_long <- tidyr::pivot_longer(
        irrigate_summary,
        cols = c(irrigate_count, total_irrigation_amount),
        names_to = "Metric",
        values_to = "Value"
      )
      
      # Create bar plot
      ggplot(irrigate_summary_long, aes(x = Metric, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values = c("skyblue", "lightgreen")) +
        labs(
          title = "Irrigation Report",
          x = "",
          y = "",
          fill = "Metric"
        ) +
        theme_minimal()+
        theme(
          plot.title = element_text(size = 24, vjust = 0.5, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 14)
        )
    }
    
    
    p3 = plotly::ggplotly(plot_irri(results_out))
    
    
    # res = do.call('rbind', out)
    # 
    # colnames(res) = 'Values'
    # 
    # res = as.data.frame(res) %>% tibble::rownames_to_column('id')
    
    output$ui_one = DT::renderDT(results_out)
    
    output$ui_two = plotly::renderPlotly(p1)
    output$ui_three = plotly::renderPlotly(p2)
    output$ui_four = plotly::renderPlotly(p3)
    
    
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



