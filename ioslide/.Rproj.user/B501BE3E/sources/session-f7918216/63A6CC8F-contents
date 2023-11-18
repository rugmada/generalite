library(shiny)
library(leaflet)
library(dplyr)

library(reactlog)

library(svglite) # for shinyapps.io to install


library(leaflet)

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
# Database on
## [RSTUDIODB].[dbo].[MBI_TAG_IP_FORMS_VISIT_FORM]

df_visit <- read.csv2("./visitPm.csv")
df_visit$attributes_position_latitude<-as.numeric(df_visit$attributes_position_latitude)
df_visit$attributes_position_longitude<-as.numeric(df_visit$attributes_position_longitude)

df_visit$date_intervention <- as.Date(df_visit$attributes_intervention_on)

df_visit$number_fictif<-df_visit$attributes_visit_data_cash_point_number%>%as.character()%>%substrRight(1)%>%as.integer()
df_visit["number_fictif"][is.na(df_visit["number_fictif"])] <- 0

df_visit<-df_visit%>%mutate(nd = ifelse(nchar(as.character(attributes_visit_data_cash_point_number))<9, paste0("34",as.character(attributes_visit_data_cash_point_number)),as.character(attributes_visit_data_cash_point_number)))


df_user <- unique(df_visit$attributes_user)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%} #species_list_text { padding: 20px; border: 1px solid #ededed; border-radius: 10px; margin: 10px; display: flex; flex-direction: row;flex-wrap: wrap;} .species_item{ flex-basis: 50%; } .species_list_header{font-weight:700;color: green; margin-bottom: 20px;}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(style = "max-width: 30%;background-color: rgba(255,255,255,0.7);padding: 0px 10px 0px 10px;border-radius: 10px",top = 10, right = 10,
                selectizeInput("animateur_nd",
                               label = "Entrez animateur",
                               choices = NULL,
                               multiple = FALSE,
                               width = "100%",
                               options = list(
                                 create = FALSE,
                                 placeholder = "jessicah",
                                 maxItems = '1'
                                 #,onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}")
                                 #,onType = I("function (str) {if (str === \"\") {this.close();}}"))
                               )
                                 )
                ,gt::gt_output("visit_type_in_area")
                ,htmlOutput("pm_list_text")
                )
   ,absolutePanel(bottom = 10,left = 10,
                 style="background-color: rgba(255,255,255,0.7);padding: 10px 30px 10px 30px;border-radius: 20px;",
                 sliderInput(
                   "day_month",
                   "Select Day of year",
                   min = min(df_visit$date_intervention,na.rm = T),
                   max = max(df_visit$date_intervention,na.rm = T),
                   value=c(min(df_visit$date_intervention,na.rm = T),
                           max(df_visit$date_intervention,na.rm = T)),
                   timeFormat="%Y-%m-%d"))
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # data to use to generate the gt table
  df_bounds <- reactive({
    if (is.null(input$map_bounds))
      return(df_visit[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(df_visit,
           attributes_position_latitude >= latRng[1] & attributes_position_latitude <= latRng[2] &
             attributes_position_longitude >= lngRng[1] & attributes_position_longitude <= lngRng[2])
  })
  
  # init leaflet map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      fitBounds(lng1 = 3,lat1 = 4,lng2 = 14,lat2 = 14)
  })
  
  animateur_nd_choices <- reactive({
    base <- df_user
    names(base) <- base
    
    if(is.null(input$animateur_nd) | input$animateur_nd == ""){
      return(base)
    }
    base[str_detect(base,input$animateur_nd)]
  })
  
  isolate({
    updateSelectizeInput(session,"animateur_nd",server = TRUE,choices = animateur_nd_choices())
  })
  
  # df to render the map
  df_react <- reactive({
    # req(input$sci_name)
    
    base <- df_visit%>%dplyr::filter(date_intervention >= input$day_month[1],date_intervention <= input$day_month[2])
    
    
    if(!is.null(input$animateur_nd) & input$animateur_nd != ""){
      print(input$animateur_nd)
      return(base %>%
               filter(str_detect(attributes_user,input$animateur_nd)))
    }else{
      return(base)
    }
  })
  
  
  output$visit_type_in_area <- gt::render_gt({
    df_bounds() %>%
      select(attributes_visit_data_visit_purpose) %>%
      separate_rows(attributes_visit_data_visit_purpose,sep = ",") %>%
      count(attributes_visit_data_visit_purpose,sort=T,name = "Count") %>%
      slice_max(Count,n=5) %>%
      rename("Visit_Purpose" = "attributes_visit_data_visit_purpose") %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = "12pt",heading.title.font.size = "14pt") %>%
      gt::tab_header(title = "Visit Purpose in area") #%>%
    #gtExtras::gt_plt_bar(column = Count,color = "darkgreen",scale_type = "number")
  })
  
  
  count_palet <- colorBin(palette = "Dark2",bins = 3,pretty=TRUE,
                          domain = range(df_visit$number_fictif))
  
  # reactive map update
  observe({
    leafletProxy("map", data = df_react()) %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addMarkers(lng = ~attributes_position_longitude,
                 lat = ~attributes_position_latitude,
                 clusterOptions = markerClusterOptions(),layerId = ~attributes_visit_data_cash_point_number) %>%
      addCircles(lng = ~attributes_position_longitude,
                 lat = ~attributes_position_latitude,
                 color = ~count_palet(number_fictif),
                 radius = ~number_fictif) %>%
      addLegend("bottomright", pal = count_palet, values = ~number_fictif,
                title = "No. of Observations",
                opacity = 1
      )
  })
  
  output$pm_list_text <- renderUI({
    if(!is.null(input$map_marker_click)){
      marker_data <- input$map_marker_click
      nd_pm <- marker_data[1]
      pm_info <- df_visit%>%filter(attributes_visit_data_cash_point_number==nd_pm)
      
      
      # pm_info$%>%
      #   paste(collapse = "</span><span class='species_item'>")
      header <- glue::glue("<span class='species_list_header'> PM {nd_pm}</span>")
      species_list <- paste0(header,"<br/><span class='species_item'> Nom contact : ",pm_info$attributes_visit_data_contact_name
                             ,"</span>"
                             ,"<br/><span class='species_item'> Adresse : ",pm_info$attributes_visit_data_contact_address,"</span>"
                             ,"<br/><span class='species_item'> Remarques : ",pm_info$attributes_visit_data_remarks,"</span>"
                             )
      return(HTML(species_list))
    }else{
      HTML("<span> Cliquez sur le marqueur pour les détails</span>")
    }
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server,options = list(port = 5000))
