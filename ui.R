library(leaflet)
library(shinyjs)
library(highcharter)


#navbarPage(title=div(img(src="https://prioriactions.github.io/prioriactions/reference/figures/logo.png", width = '40px'),"prioriactions"), id="nav",
navbarPage(title="prioriactions", id="nav",
           
    div(class="outer",

      tags$head(
        
        tags$script(HTML(
          '
          window.LeafletWidget.methods.setStyle = function(category, layerId, style){
            var map = this;
            if (!layerId){
              return;
            } 
            else if (!(typeof(layerId) === "object" && layerId.length)){ 
              // in case a single layerid is given
              layerId = [layerId];
            }
          
            //convert columnstore to row store
            style = HTMLWidgets.dataframeToD3(style);
            
            //console.log(style);
            layerId.forEach(function(d,i){
            var layer = map.layerManager.getLayer(category, d);
            if (layer){ // or should this raise an error?
              layer.setStyle(style[i]);
            }
          });
        };
        ')),
        # Include our custom CSS
        includeCSS("styles.css"),
        #includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      
      absolutePanel(bottom = 10, left = 170,
                    useShinyjs(), 
                    shinyjs::disabled(
                      sliderInput("range", NULL, min = 0, max = 1,
                                  value = 1, step = 0.1
                      )
                    ),
      ),
      
      absolutePanel(top = 80, left = 10,
                    useShinyjs(), 
                    shinyjs::hidden(
                    radioGroupButtons("radio", NULL,
                                   choices = list("Base" = 1, "Solution" = 2),
                                   selected = 1, 
                                   size = "xs",
                                   direction = "vertical",
                                   justified = TRUE)
                    ),
      ),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 500, height = "auto",

        column(12, tabsetPanel(id="plot_tabs", 
          tabPanel("Data explorer", id = "tab_explorer_data",
                 fluidRow(
                   useShinyjs(), 
                   
                   h2("Data explorer"),
                   
                   fileInput(
                     inputId = "filemap",
                     label = "Upload map. Choose shapefile",
                     multiple = TRUE,
                     accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                     width = 600
                   ),
                   textOutput("error_filemap"),
                   
                   shinyjs::disabled(
                     fileInput(
                       inputId = "filecsv",
                       label = "Upload prioriactions input data. Choose .csv",
                       multiple = TRUE,
                       accept = c(".csv"),
                       width = 600
                     )
                   ),
                   textOutput("error_filecsv"),
                   
                   shinyjs::disabled(
                     selectInput("specific_plot", "Filtered map:", width = 600,
                                 choices = list(`Costs` = list(""),
                                                `Features distribution` = list(""),
                                                `Threats distribution` = list(""))
                     )
                   ),
                   
                   div(highchartOutput("pie_features", width = 400, height = 320), align = "center"),
                   div(highchartOutput("pie_threats", width = 400, height = 320),align = "center"),
                   
                   
                 ),
          ),
          tabPanel("Running",
                   fluidRow(
                     useShinyjs(), 
                     
                     shinyjs::disabled(
                       h2("Running"),
                       
                       h3("Step 1: Validate inputs"),
                       
                       actionButton("validate", "Validate"),
                       verbatimTextOutput("step1"),
                       
                       h3("Step 2: Create mathematical model"),
                       
                       actionButton("create", "Create"),
                       verbatimTextOutput("step2"),
                       
                       h3("Step 3: Run the model"),
                       
                       actionButton("run", "Run!"),
                       verbatimTextOutput("step3"),
                       tags$head(tags$style("#step3{overflow-y:scroll; max-height: 500px; background: ghostwhite;}"))
                     )
                   )
          ),
          tabPanel("Explore solutions",
                   fluidRow(
                     
                     useShinyjs(), 
                    
                       h2("Solutions"),
                       shinyjs::disabled(
                       fileInput(inputId = "solutionsImport",
                                 label="Import solution data",
                                 buttonLabel=list(icon("folder"),""),
                                 multiple = TRUE,
                                 accept = ".txt",
                                 width = 600),
                        
                       selectInput("solution_name", "Solution name:", width = 600,
                                     choices = list("")),
                                     
                       selectInput("solution_plot", "Tools for solutions:", width = 600,
                                     choices = list(`Costs` = list(""),
                                                    `Features distribution` = list(""),
                                                    `Threats distribution` = list(""))),
                       
                       #shinyjs::hidden(
                        div(highchartOutput("pie_features_sol", width = 400, height = 320), align = "center"),
                        div(highchartOutput("pie_threats_sol", width = 400, height = 320), align = "center")
                       #)
                      )
                       
                     
                   )
          ),
        ))
      ),
    ),
  conditionalPanel("false", icon("crosshair"))
)