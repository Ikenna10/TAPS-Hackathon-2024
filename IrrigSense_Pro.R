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
  "Contacts: emmanuel.adeyemo@corteva.com, siobhan.moore@corteva.com. <br><br>
####################
####################
clay",
  sitty_clay",
clay_loam",
  sandy_clay_loam",
      )),
loam",
  silt_loam",
loam",
  sandy_loam",
fine_sandy_loam",
  loam",
sandy_loam",
  sand",
loam_sand",
  sand",
,
sm", "Soil moisture csv file", accept = c(".csv")),

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
shiny::fluidRow(
  shiny::column(8, DT::DTOutput("ui_one")),
  shiny::column(4, plotly::plotlyOutput("ne_plot"))
),

#tableOutput("ui_one"),

plotOutput("ui_two", width = "100%"),

plotOutput("ui_three", width = "100%"),

shinydashboard::dashboardBody(shiny::uiOutput("ui"))


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
    
    
    
    # get et data
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
    water, depletion_fraction) {
    # browser()
    # Loop through each row in the data
    for (i in 1:nrow(data)) {
      # Sample day after germination calculation (for example purpose)
      dag <- as.numeric(data$day[i] - days_of_emergence) #as.Date("2023-08-28")) # Assuming germination date is Aug 28, 2023
      factor)
field_capacity, wilting_point, data$water_content[i])
ad <- calculate_allowable_depletion(taw, df)
depletion[i] - rainfall)
out = list(days_after_germination=days_after_germination, root_growth_factor=root_growth_factor, root_depth=root_depth,
           total_available_water=total_available_water, depletion_fraction=depletion_fraction, allowable_depletion=allowable_depletion, irrigation_amount=irrigation_amount)
# Add calculated values to the data frame
# data$days_after_germination <- days_after_germination
# data$root_growth_factor <- root_growth_factor
# data$root_depth <- root_depth
# data$total_available_water <- total_available_water
# data$depletion_fraction <- depletion_fraction
# data$allowable_depletion <- allowable_depletion
# data$irrigation_amount <- irrigation_amount
#data$irrigation_decision <- irrigation_decision

out_new = do.call('cbind', out)

out_new = out_new %>% as.data.frame()

colnames(out_new) = c('days_after_germination', 'root_growth_factor', 'root_depth', 'total_available_water', 'depletion_fraction', 'allowable_depletion', 'irrigation_amount')

return(out_new)
    }
    
    # Run the calculations on the sample data
    results <- perform_calculations(all_data)
    
    
    
    
    ###########################################################
    #############################################################
    
    all_data <- data.frame(
      day = as.Date(rain_dta$date, format="%m/%d/%Y"), # Adjusted fo, # Adjust format if needed
      water_content = daily_sensor_data$daily_moisture,
      rainfall = rain_dta$rain,
      evapotranspiration = et_dta$evapotranspiration#,
      #depletion_fraction =(field_capacity - daily_sensor_data$daily_moisture) / (field_capacity - wilting_point)
    )
    # Ensure date columns are in Date format
    all_data$day <- as.Date(all_data$day, format = "%m/%d/%Y")
    #daily_evapotranspiration$Timestamp <- as.Date(daily_evapotranspiration$Timestamp, format = "%m/%d/%Y")
    
    # Calculate range limits
    precip_min <- min(all_data$rain, na.rm = TRUE)
    precip_max <- max(all_data$rain, na.rm = TRUE)
    evapo_min <- min(all_data$evapotranspiration, na.rm = TRUE)
    evapo_max <- max(all_data$evapotranspiration, na.rm = TRUE)
    
    
    # Define a scaling factor to align the evapotranspiration data with precipitation scale
    scaling_factor <- (precip_max - precip_min) / (evapo_max - evapo_min)
    
    # Create the plot
    p1 = ggplot() +
      geom_line(data = all_data, aes(x = day, y = rain, color = "Precipitation"), linewidth = 1) +
      geom_line(data = all_data, aes(x = day, y = evapotranspiration * scaling_factor, color = "Evapotranspiration"), linewidth = 1) +
      scale_y_continuous(
        name = "Precipitation (inches/day)",
        limits = c(precip_min, precip_max),
        sec.axis = sec_axis(~ . / scaling_factor, name = "Evapotranspiration (inches/day)")
      ) +
      scale_color_manual(values = c("Precipitation" = "blue", "Evapotranspiration" = "red")) +
      labs(x = "Date", color = "Legend") +
      theme_minimal() +
      theme(axis.title.y.left = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "red"))
    
    
    ### Outputs Plot ###
    
    
    
    # Create a line plot of depletion_fraction over time
    p2 = ggplot(results, aes(x = day, y = depletion_fraction)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +  # Horizontal line at y = 0.5
      labs(title = "Depletion Fraction Over Time",
           x = "Date",
           y = "Depletion Fraction") +
      theme_minimal()
    
    
    
    
    # res = do.call('rbind', out)
    #
    # colnames(res) = 'Values'
    #
    # res = as.data.frame(res) %>% tibble::rownames_to_column('id')
    
    output$ui_one = renderDataTable(results)
    
    output$ui_two = plotOutput(p1)
    output$ui_three = plotOutput(p2)
    
    
    
    
  })



  }

# Run the application
shinyApp(ui = ui, server = server)