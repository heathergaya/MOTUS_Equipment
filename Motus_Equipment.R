library(shiny)
library(shinydashboard)
library(sqldf)
library(dplyr)
library(DT)
library(tidyr)
library(stringr)
library(shinyWidgets)
library(formattable)
library(shinybusy)
library(shinyjs)
#library(leaflet)
library(scales)

spinnerCSS <- "
#loading {
  display: none; /* Hidden by default */
  position: fixed; /* Sit on top of the page content */
  width: 100%; /* Full width */
  height: 100%; /* Full height */
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(0,0,0,0.5); /* Black background with opacity */
  z-index: 2; /* Specify a stack order in case you're using a z-index scale */
  cursor: progress; /* Add a progress cursor */
}

.spinner {
  border: 16px solid #f3f3f3; /* Light grey */
  border-top: 16px solid #3498db; /* Blue */
  border-radius: 50%;
  width: 120px;
  height: 120px;
  animation: spin 2s linear infinite;
  position: absolute;
  top: 50%;
  left: 50%;
  margin-right: -50%;
  transform: translate(-50%, -50%);
}

@keyframes spin {
  0% { transform: translate(-50%, -50%) rotate(0deg); }
  100% { transform: translate(-50%, -50%) rotate(360deg); }
}
"

# Custom CSS (dashboard colors)
myCSS <- "
/* Main header */
.skin-blue .main-header .navbar {
  background-color: #353839; /* Dark gray color for the main header */
}

/* Menu header when collapsed */
.skin-blue .main-sidebar, .left-side {
  background-color: #192841; /* Dark gray color for the sidebar */
}

/* Side panel where tabs are located */
.skin-blue .sidebar-menu > li.active > a {
  border-left-color: #353839; /* Indigo color for the active tab */
}

.skin-blue .sidebar-menu > li > a {
  color: white; /* White text color for better readability */
}

/* Boxes around sections with rounded corners and a light gray background */
.box-section {
  border: 1px solid #ddd; /* Light gray border */
  border-radius: 8px; /* Rounded corners */
  padding: 10px; /* Padding inside the box */
  margin-bottom: 20px; /* Space between sections */
  background-color: #f9f9f9; /* Light gray background */
}

/* Vertical line between columns */
.vertical-line {
  border-left: 2px solid #ccc; /* Solid line on the left of a column */
  padding-left: 20px; /* Space to the left inside the column */
}

/* Custom CSS to adjust the text size and alignment for menu items */
      .treeview-menu > .conditional-item > a {
        font-size: 6px; /* Adjust the font size as needed */
        text-align: center; /* Center align the text */
        line-height: 40px; /* Adjust line height to vertically center the text */
      }
      
/* Custom CSS to hide the arrows */
 .treeview > a > .fa-angle-left {
  display: none !important;
"


# Append spinnerCSS to myCSS
myCSS <- paste(myCSS, spinnerCSS, sep = "\n")

### Grab Parts Lists 
antenna_options <<- read.csv("antenna_options.csv")
receiver_options <- read.csv("receiver_options.csv")
receiver_options$Cost <- as.numeric(sub('.', '',receiver_options$Cost)) #get rid of dollar sign
structure_options <- read.csv("structure_options.csv")
structure_options$Cost <- as.numeric(sub('.', '',structure_options$Cost)) #get rid of dollar sign
antennaspresent <<- FALSE
#initialize antennas_selected object 
antennas_selected <<- antenna_options
towerheight <<- 25 #fix this later once we add in structure stuff
struc <<- TRUE
################################################################################

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      img(src = './MOTUS.png', height = "15px", style = "vertical-align: middle; margin-right: 5px;"),
      "MOTUS Equipment"
    ),
    titleWidth = 230
  ),
  dashboardSidebar(
    sidebarMenuOutput("sidebarmenu")
    ),
  dashboardBody(
    useShinyjs(),  
    tags$head(tags$style(HTML(myCSS))),
    tabItems(
      tabItem(tabName = "about",
              h1("Beta Version"),
              p("This is pre-release version of a motus station planning application. Users of this beta version
              are encouraged to report any issues they encounter, which allows developers to make necessary 
                adjustments and improvements. This phase is crucial for improving the quality and stability of 
                the software, ensuring that it meets user expectations and performs well in real-world conditions."),
              br(),
              h1("About this Application"),
              br(),
              p("This application allows users to estimate the supplies needed for specific MOTUS setups. While we tried to think of common combinations of features, every MOTUS tower is unique and may require additional mounting brackets, structural support or wiring not represented by this application. Links and prices may change over time as companies adjust to market demand. Shoot (someone) an email if you notice something looks off"),
              div(style = "position: absolute; bottom: 10px; right: 10px;",
                  img(src = "MOTUS.png", height = "125px"))
      ),
  #### BUILD STATION 1 ###########################################
      tabItem(tabName = "station_plan",
              fluidRow(
                column(6, class = "vertical-line",
                       div(class = "box-section",
                           h1("Station Specifications"),
                             selectInput("receiver_type", "Select Receiver Type:",
                                         choices = c("Sensorgnome" = "Sensorgnome",
                                                     "CTT SensorStation" = "CTT",
                                                     "Lotek Basestation" = "lotek"),
                                         selected = "CTT"
                                         ),
                           conditionalPanel(
                             condition = "input.receiver_type == 'CTT'",
                             selectInput("connection_type", "Connection to Internet:",
                                         choices = c("Cellular" = "LTE",
                                                     "Local Internet" = "local_internet",
                                                     "Iridium" = "Iridium",
                                                     'Manually' = 'manual_upload'),
                                         selected = "cellular")
                            ),
                           conditionalPanel(
                             condition = "input.receiver_type != 'CTT'",
                             selectInput("connection_type_b", "Connection to Internet:",
                                         choices = c("Cellular" = "LTE",
                                                     "Local Internet" = "local_internet",
                                                     'Manually' = 'manual_upload'),
                                         selected = "cellular")
                           ),
              
                          selectInput("structure_type", "Tower Structure:",
                            choices = c("Brackted Rohn Tower (30 ft)" = "rohn_bracket_30",
                                        "Brackted Rohn Tower (20 ft)" = "rohn_bracket_20",
                                        "Guyed Rohn Tower (30 ft)" = "rohn_guyed_30",
                                        "Guyed Rohn Tower (20 ft)" = "rohn_guyed_20",
                                        "Guyed Telesecoping Mast (~25 ft)" = "guyed_telescope_25",
                                        "Other (Supplies will not be listed)" = "no_structure"
                                        #"Rooftop Mount (Ballast Mount)" = "ballast_roof",
                                        #"Rooftop Mount (Pole-Pole Mount)" = "pole_roof"
                                        ),
                            selected = "rohn_bracket_30"
                          ),
                          
                          selectInput("power_source", "Power Source:",
                                      choices = c("Solar" = "solar_power",
                                                  "Grid (120V AC)" = "grid_power",
                                                  "Battery Tender" = "battery_tend_power"
                                      ),
                                      selected = "grid_power"
                          ),
                          tags$script(HTML("
                                        $(document).ready(function(){
                                        $('#connector_kit1').prop('checked', false);
                                        $('#connector_kit1').prop('disabled', true);
                                        });
                                    ")),
                          checkboxInput('connector_kit1', 'Do you want to purchase coax cable pre-cut?', value = FALSE)
                    )), #end column 1
                column(6, class = 'vertical-line',
                       div(class = "box-section",
                           h1("Antenna Specifications"),
                           ### Frequencies - will need to add in iridium at some point 
                           selectInput("frequency_selector", "Frequencies Scanned:",
                                       choices = c("166.380 MHz" = "tags_166",
                                                   "434 MHz" = "tags_434",
                                                   "Both 166.380 and 434 MHz" = "tags_166_434"
                                       ),
                                       selected = "tags_166_434"
                           ),
                           #shiny for mac has a bug that doesn't let step = 1? 
                           #numericInput("antenna_count_selector", "Unique Antenna Types", value = 1, min = 1, max = 8, step = .5),
                           uiOutput("dynamic_dropdown"),
                           uiOutput("dynamic_counts")
                           ##### To do: ADD IN CUSTOM ANTENNA - BUT MAKE FOLKS PUT IN VALUES ####
                           )) #end column 2
                ),
              fluidRow(
                column(4),
                column(4,
                       div(class = "box-section",
                        numericInput("station_count1", "Stations with this design", value = 1, min = 1, step = .5),
                        )
                       )
                ), #end row
              #fluidRow(
              #  column(4,
              #         actionButton("toggle_hidden", "Create an additional station plan"))),
              #Turned off the option to do more than 1 station type at a time for now
                
            #  ), #end station 1 plan
    ), #end tab 
  tabItem(tabName = "supplies_needed",
          h1("Supplies Needed"),
          p("Costs do not include tax or shipping and are based on prices of items at time of application development. 
            Please plan accordingly."),
            DTOutput('station1_costs')) #end supplies tab
  )))

################################################################################
# Server side

server <- function(input, output, session) {

  observe({ #tower build 1
    relevant_options <<- ifelse(input$frequency_selector == 'tags_166',
                               subset(antenna_options, Frequency == 166), 
                               ifelse(input$frequency_selector == 'tags_434', 
                                      subset(antenna_options, Frequency == 434),
                                      subset(antenna_options, Frequency != 0)))
  })
  
  # vector of selected antenna
  preds_selected <<- reactive({
    unlist(lapply(seq_len(input$antenna_count_selector), function(i) {
      input[[paste0("antenna_options_", i)]]
    }))
  })
  
  output$dynamic_dropdown <- renderUI({
    req(input$frequency_selector)  # Ensure selection type is available
      list(
             selectInput("antenna_options", "Antenna Choice:",
                         choices = relevant_options[[1]], multiple = TRUE
                         ))
  })
  
  output$dynamic_counts <- renderUI({
    req(input$antenna_options)  # Ensure selection type is available
    #print(input$antenna_options)
    TotAntenna <<- length(input$antenna_options)
    lapply(1:length(input$antenna_options), function(i) {
    list(
             numericInput(paste0("Count_", i), label = paste0("How many ", input$antenna_options[i], "?"), value=1, min = 1, max = 8, step = .5))
    })
  })

  output$sidebarmenu <- renderMenu({
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Build Your Station", tabName = "station_plan", icon = icon("screwdriver-wrench")),
      menuItem("Supplies Needed", tabName = "supplies_needed", icon = icon("file-invoice-dollar"))
      )})
  
  observe({
    #req(input$antenna_options)
    antennas_selected <<- subset(antenna_options, Antenna %in% input$antenna_options)
  })
  
    # Initialize antenna_calcs as a reactiveValues object
  antenna_calcs <- reactiveValues(
      antenna_type = character(0),
      count_antenna = numeric(0),   
      freq = numeric(0) 
    )
  
  tower_components <- reactiveValues() #prep this object
  
  
  #antenna_array <- reactive({ ## this is not the cleanest code ever... 
  observe({
    req(input$antenna_options)
    #myinput_df <- NULL;   #will throw a warning error, but can ignore
    #myinput_df_c <- NULL; mydf_row_c <- NULL; 
    df_row <- vector(mode = "list", length = length(input$antenna_options))
    antenna_names <- input$antenna_options
    #print(names(input))
    antenna_count_nums <- grep('Count', names(input))
    
    for(i in antenna_count_nums){
      df_row[[i]] <- c(antenna_names[i], input[[names(input)[i]]])
    }
    #print(df_row)
    myinput_df <- as.data.frame(do.call('rbind', df_row))
    
    antenna_calcs$antenna_type <- myinput_df$V1
    antenna_calcs$count_antenna <- myinput_df$V2
    antenna_calcs$freq <- antenna_options[match(myinput_df$V1, antenna_options$Antenna), 'Frequency']
  })
  
  observe({
    req(antennas_selected)
    #req(input$antenna_options)
    req(antenna_calcs)
    print(paste0("the number of selected antenna types is: ", nrow(antennas_selected)))
    
    print(nrow(antennas_selected)); print(length(antenna_calcs$count_antenna))
    
    
      #print(antenna_calcs$count_antenna)
      #print(antennas_selected)
      if(length(antenna_calcs$count_antenna) == length(antennas_selected$Amt_unit)){
        print('aha!')
        antennas_selected$Amt_unit <- antenna_calcs$count_antenna
      } 
      
      antenna_connectors <- subset(antenna_options, Frequency == 0)
      antennas_selected$TotalCost <- antennas_selected$Cost*as.numeric(antennas_selected$Amt_unit)
      antenna_connectors$Amt_unit <- ifelse(antenna_connectors$Antenna == 'N-type male connector ',
                                            ceiling(sum(as.numeric(antenna_calcs$count_antenna))*2/10), #N-type packs = 1/5 total antennas
                                            (towerheight + 20)*sum(as.numeric(antennas_selected$Amt_unit)))
      antenna_connectors$TotalCost <- antenna_connectors$Cost*as.numeric(antenna_connectors$Amt_unit)
      #print(antenna_connectors)
      
    tower_components$antenna <- rbind(antennas_selected[,c("Item", 'Needed_For', "Source", "Cost", "Amt_unit", "Source_1", "Source_2", "TotalCost")],
                                     antenna_connectors[,c("Item", 'Needed_For', "Source", "Cost", "Amt_unit", "Source_1", "Source_2", "TotalCost")]) 
    antennaspresent <<- nrow(tower_components$antenna) >0
    
  })
  
  observe({
    req(input$structure_type)
    req(antenna_calcs)
    guyline <- ifelse(length(grep("guyed", input$structure_type)) !=0, 'Guyed', 'Bracket')
    Towertype <- ifelse(length(grep("rohn", input$structure_type)) !=0, "Rohn", "PopUp")
    towerheight <<- as.numeric(substr(input$structure_type, nchar(input$structure_type)-1, nchar(input$structure_type)))
    #print(guyline); print(Towertype); print(towerheight)
    tower_components$structure <- subset(structure_options, Req %in% c(Towertype, 'Both') & 
                                          Guyed %in% c(guyline, 'Either'))
    print(tower_components$structure[,1:9])
    
  })
  
  
  ## NEED TO ADD IN SOLAR AND PHOTOS AND MULTIPLY FOR MULTIPLE TOWERS OF SAME TYPE #####
  
  observe({ ## grab receiver info, plus relevant information for funcubes/RMA cable counts
    chosenrec <- input$receiver_type
    if(chosenrec == 'CTT'){
      mydatplan <- input$connection_type
    } else {
      mydatplan <<- input$connection_type_b
    }
    
    req(antenna_calcs)
    tower_components$receiver <- subset(receiver_options, Req %in% c(chosenrec, 'All') & 
                                  Data %in% c(mydatplan, 'All'))
  
  })
  supplylist <- reactiveValues()
  
####### Observe the receiver and structure information, adjust, make final data table ######
  output$station1_costs <- renderDataTable({
    #req(antennas_selected)
    req(antenna_calcs)
    req(input$structure_type)
    
    tot166 <- sum((antenna_calcs$freq == 166)*as.numeric(antenna_calcs$count_antenna))
    tot434 <- sum((antenna_calcs$freq == 434)*as.numeric(antenna_calcs$count_antenna))
    if(is.na(tot166)){tot166 <- 0}
    if(is.na(tot434)){tot434 <- 0}
    receiver_related <- tower_components$receiver
    structure_related <- tower_components$structure
    struc <<- TRUE
    if(input$structure_type == "no_structure"){struc <<- FALSE}
    
    print(tot166); print(tot434)
    
    ### change amounts for items that change depending on station specifications
    if("sma connector" %in% receiver_related$ShinyName){
      itemnum_a <- which(receiver_related$ShinyName =="sma connector")
      receiver_related[itemnum_a, 'Amt_unit'] <- ceiling(tot166/2) #166/2+1 
    }
    
    if("sma bulkhead 166" %in% receiver_related$ShinyName){
      itemnum_b <- which(receiver_related$ShinyName == "sma bulkhead 166")
      receiver_related[itemnum_b, 'Amt_unit'] <- tot166
    }
    
    print(receiver_related$ShinyName)
    if("funcube" %in% receiver_related$ShinyName){
      itemnum_c <- which(receiver_related$ShinyName == "funcube")
      receiver_related[itemnum_c, 'Amt_unit'] <- tot166
    }
    
    if("sma bulkhead 434" %in% receiver_related$ShinyName){
      itemnum_d <- which(receiver_related$ShinyName == "sma bulkhead 434")
      receiver_related[itemnum_d, 'Amt_unit'] <- tot434
    }
    
    if("ctt dongle" %in% receiver_related$ShinyName){
      itemnum_e <- which(receiver_related$ShinyName == "ctt dongle")
      receiver_related[itemnum_e, 'Amt_unit'] <- tot434
    }
    
    if("big box" %in% receiver_related$ShinyName){
      itemnum_f <- which(receiver_related$ShinyName == "big box")
      itemnum_g <- which(receiver_related$ShinyName == "small box")
      print(itemnum_g); print(itemnum_f); print(tot434); print(tot166)
      if(tot434 + tot166 > 2){
        receiver_related[itemnum_f, 'Amt_unit'] <- 1
        receiver_related <- receiver_related[-itemnum_g, ] #remove small box
      } else {
        receiver_related[itemnum_g, 'Amt_unit'] <- 1
        receiver_related <- receiver_related[-itemnum_f, ] #get rid of big box
      }
    }
    
    if("sensorgnome bulkhead" %in% receiver_related$ShinyName){
      itemnum_h <- which(receiver_related$ShinyName == "sensorgnome bulkhead")
      receiver_related[itemnum_h, 'Amt_unit'] <- tot434 + tot166
    }
    
    if("USB Hub" %in% receiver_related$ShinyName){
      itemnum_i <- which(receiver_related$ShinyName == "USB Hub")
      receiver_related[itemnum_i, 'Amt_unit'] <- 1
      if(tot434 + tot166 < 5){receiver_related <- receiver_related[-itemnum_i, ]} #don't need hu b unless you have 5 or more antennas
    }
    
    if("bnc dust" %in% receiver_related$ShinyName){
      itemnum_j <- which(receiver_related$ShinyName == "bnc dust")
      receiver_related[itemnum_j, 'Amt_unit'] <- tot434 + tot166
    }
    
    if('ethernet cable' %in% receiver_related$ShinyName){
      itemnum_k <- which(receiver_related$ShinyName == "ethernet cable")
      if(mydatplan == 'local_internet' & input$receiver_type != 'CTT'){
        receiver_related[itemnum_k, 'Amt_unit'] <- 1
      } else{
        receiver_related <- receiver_related[-itemnum_k, ] #only need ethernet for sensorgnome local connection to internet
      }
    }
    
    if('rpi cellular kit' %in% receiver_related$ShinyName){
      itemnum_l <- which(receiver_related$ShinyName == "rpi cellular kit")
      itemnum_m <- which(receiver_related$ShinyName == "proxicast antenna")
      if(mydatplan == 'LTE'){
        receiver_related[c(itemnum_l,itemnum_m) , 'Amt_unit'] <- 1
      } else{
        receiver_related <- receiver_related[-c(itemnum_l,itemnum_m), ] #only need rpi for sensorgnome LTE connections
      }
    }
    
    receiver_related$TotalCost <-  receiver_related$Cost*receiver_related$Amt_unit
    
    #### Same idea, but for structure items ####
    if("T_thru" %in% structure_related$ShinyName & tot166 > 1){
      s_item_a <- which(structure_related$ShinyName =="T_thru")
      structure_related[s_item_a, 'Amt_unit'] <- 1 #only need if >1 big antenna 
    }
    
    if("Roof" %in% structure_related$ShinyName & tot434 > 3){
      s_item_b <- which(structure_related$ShinyName =="Roof")
      structure_related[s_item_b, 'Amt_unit'] <- 1 #only need if >3 CTT antenna 
    }
    
    if("main" %in% structure_related$ShinyName){
      s_item_c <- which(structure_related$ShinyName =="main")
      structure_related[s_item_c, 'Amt_unit'] <- max(floor(towerheight/10 -1), 0)
    }
    
    if("small spool" %in% structure_related$ShinyName){
      s_item_d <- which(structure_related$ShinyName =="small spool")
      #s_item_d2 <- which(structure_related$ShinyName =="big spool")
     guylength <- ceiling(sqrt((towerheight^2)+((towerheight*0.7)^2)))*3+15
     #if(guylength <= 250){
      structure_related[s_item_d, 'Amt_unit'] <- 1
      #structure_related[s_item_d2, 'Amt_unit'] <- 0
      #}
      #### NOTE THIS WILL CHANGE WHEN WE IMPLEMENT MULTIPLE TOWERS OF THE SAME TYPE
    }
    
    if("thimble" %in% structure_related$ShinyName){
      s_item_e <- which(structure_related$ShinyName =="thimble")
      structure_related[s_item_e, 'Amt_unit'] <- 3
      if(input$structure_type == "guyed_telescope_25") {structure_related[s_item_e, 'Amt_unit'] <- 6} #6 anchors for pop up towers
    }
    
    if("clamps" %in% structure_related$ShinyName){
      s_item_f <- which(structure_related$ShinyName =="clamps")
      structure_related[s_item_f, 'Amt_unit'] <- 24
      if(input$structure_type == "guyed_telescope_25") {structure_related[s_item_f, 'Amt_unit'] <- 48} #6 lines for popups
    }
    
    if("turnbuckle" %in% structure_related$ShinyName){
      s_item_g <- which(structure_related$ShinyName =="turnbuckle")
      structure_related[s_item_g, 'Amt_unit'] <- 3
      if(input$structure_type == "guyed_telescope_25") {structure_related[s_item_g, 'Amt_unit'] <- 6} #6 lines for popups
    }
    
    if("shackle" %in% structure_related$ShinyName){
      s_item_h <- which(structure_related$ShinyName =="shackle")
      structure_related[s_item_h, 'Amt_unit'] <- 3
    }
    
    if("anchor" %in% structure_related$ShinyName){
      s_item_i <- which(structure_related$ShinyName =="archor")
      structure_related[s_item_i, 'Amt_unit'] <- 3
      if(input$structure_type == "guyed_telescope_25") {structure_related[s_item_i, 'Amt_unit'] <- 6} #6 lines for popups
    }
    
    structure_related$TotalCost <- structure_related$Cost* structure_related$Amt_unit*struc
    
    ### adjust for number of stations
    station_count1 <- as.numeric(input$station_count1)
    print(station_count1)
    structure_related$Amt_unit <- structure_related$Amt_unit*station_count1
    receiver_related$Amt_unit <- receiver_related$Amt_unit*station_count1
    antenna_related <- tower_components$antenna[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")]
    antenna_related$Amt_unit <- as.numeric(antenna_related$Amt_unit)*station_count1
    antenna_related$TotalCost <- as.numeric(antenna_related$Amt_unit)*as.numeric(antenna_related$Cost)
    
    total_total <- receiver_related[0,] #grab names
    total_total[1, 'TotalCost'] <- sum(receiver_related$Cost*receiver_related$Amt_unit)+ sum(antenna_related$TotalCost) + (sum(structure_related$Cost* structure_related$Amt_unit)*struc)
    total_total[1, 'Item'] <- 'Total Cost' 
    
    print(total_total)
    
    #print(antennaspresent)
    #print(tower_components$antenna)
    if(antennaspresent){
      if(struc){ #antennas and structure
        mylist <- rbind(antenna_related[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                        receiver_related[,c("Item","Needed_For", "Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                        structure_related[,c("Item","Needed_For", "Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                        total_total[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")])
        mylist$Cost <- dollar(mylist$Cost)
        mylist$TotalCost <- dollar(mylist$TotalCost)
        print('end of rendering')
        supplylist$final <- mylist
    #print(tower_components$antenna)
      } else { #antennas but no structure
        mylist <- rbind(antenna_related[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                        receiver_related[,c("Item","Needed_For", "Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                        total_total[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")])
        mylist$Cost <- dollar(mylist$Cost)
        mylist$TotalCost <- dollar(mylist$TotalCost)
        print('end of rendering- no structure')
        supplylist$final <- mylist
    }} #end antennas = TRUE
      else {
        if(struc){
            mylist <- rbind(receiver_related[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                            structure_related[,c("Item","Needed_For", "Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                                 total_total[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")])
            mylist$Cost <- dollar(mylist$Cost)
            mylist$TotalCost <- dollar(mylist$TotalCost)
    supplylist$final <- mylist      
            print('other - structure')
        } else {
            mylist <- rbind(receiver_related[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")],
                          total_total[,c("Item", "Needed_For","Source", "Cost", "Amt_unit", "TotalCost", "Source_1", "Source_2")])
          mylist$Cost <- dollar(mylist$Cost)
          mylist$TotalCost <- dollar(mylist$TotalCost)
          supplylist$final <- mylist      
          print('just receiver')
        }
    }
    #print(tail(supplylist$final$Item, n = 10))
    print('end of table')
    #browser()
    # Use DataTables to render the data
    #return(
      DT::datatable(
        data = mylist[mylist$Amt_unit > 0 | mylist$Item == 'Total Cost', ],
        extensions = 'Buttons',
        selection = 'none',
        rownames= FALSE,
        options = list(
          pageLength = 40,
          autoWidth = TRUE,
          dom = 'Bfrtip',  # Define the table control elements to appear on the web page
          buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    #)
    })
}

# Run the app
shinyApp(ui = ui, server = server) #, options = list(display.mode="showcase"))
