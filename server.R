library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

library(sp)
library(raster) #To plot of shapefiles
library(rgdal)
library(shiny)
library(tigris)
library(reshape2)
library(viridis)
library(rmapshaper)
library(stringr)
library(mapview)
library(tidyr)


function(input, output, session) {

  ## Interactive Map 
  # Create the map -------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -50, lat = 18, zoom = 3)
  })
  
  #Uploading maps --------------------------------------------------------------
  shape <- reactive({

    req(input$filemap)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$filemap

    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
   
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
      ext <- tools::file_ext(shpdf$datapath[i])
      validate(need(ext %in% c("shp", "dbf", "sbn", "sbx", "shx", "prj", "qmd", "qpj","cpg"), "Please upload an valid file"))
    }
    

    
    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp,
    # not only that contain .shp)

    shape <- raster::shapefile(paste(tempdirname,
                                    shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                    sep = "/"))
    
    shape <- rmapshaper::ms_simplify(shape, keep_shapes = TRUE)
    
    shape <- sp::spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    shape
  })
  
  #Updating map ----------------------------------------------------------------
  plot_shape <- reactive({
    
    shape <- shape()
    
    
    if("id" %in% names(shape)){
      
      longlat <- coordinates(shape)
      long_shape <- mean(longlat[,1])
      lat_shape <- mean(longlat[,2])
      
      popup_pu <- sprintf(
        "<strong>id %s</strong><br/>",
        shape$id) %>% 
        lapply(htmltools::HTML)
      
      
      leafletProxy("map", data = shape) %>%
        clearShapes() %>%
        clearControls() %>%
        setView(lng = long_shape, lat = lat_shape, zoom = 8) %>%
        #addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(group = "Base",
                    weight = 0.2, 
                    color = "black",
                    smoothFactor = 0.3,
                    fillOpacity = input$range,
                    popup = popup_pu, 
                    fillColor = "white", 
                    layerId = ~id, 
                    highlightOptions = highlightOptions(
                      color = "black", 
                      weight = 5,
                      #fillColor = "#FFD700",
                      fillOpacity = NULL, 
                      bringToFront = TRUE,
                      sendToBack = TRUE))
    }
  })
  
  #Change shapefile initial----------------------------------------------------
  observeEvent(input$filemap, {
    plot_shape()
    enable("range")
  }) 
  
  #Change opacity---------------------------------------------------------------
  observeEvent(input$range, {
    
    shape <- shape()

    leafletProxy("map", data = shape) %>%
      setShapeStyle(layerId = ~id, fillOpacity = input$range, 
                    options = list(group = "Base"))
    
  })
  

  #error text map---------------------------------------------------------------
  output$error_filemap <- renderText({ 
    shape <- shape()
    
    if(is.null(shape))
      paste("")
    else{
      if("id" %in% names(shape)){
        paste("Correctly uploaded")
        enable("filecsv")
      }
      else{
        paste("Shapefile must have a field named id")
      }
    }
  })

  #csv -------------------------------------------------------------------------
  csv_files <- reactive({
    req(input$filecsv)

    if(!is.null(req(input$filecsv))){
      req(input$filecsv)
      csv_data <- input$filecsv
      tempdirname <- dirname(csv_data$datapath[1])
      
      
      # Rename files
      for (i in 1:nrow(csv_data)) {
        #file.rename(
        #  csv_data$datapath[i],
        #  paste0(tempdirname, "/", csv_data$name[i])
        #)
        
        #NO ESTÁ FUNCIONANDO
        ext <- tools::file_ext(csv_data$datapath[i])
        shiny::validate(need(ext == "csv", "Please upload an csv file"))
        
        validate(need(csv_data$name[i] %in% c("dist_features.csv", 
                                              "dist_threats.csv", 
                                              "features.csv", 
                                              "sensitivity.csv", 
                                              "threats.csv", 
                                              "pu.csv",
                                              "boundary.csv"), "Please upload an valid file"))
        
        
        if(csv_data$name[i] == "pu.csv"){
          pu_data <- data.table::fread(file = csv_data$datapath[i],
                                       data.table = FALSE)
        }
        else if(csv_data$name[i] == "features.csv"){
          features_data <- data.table::fread(file = csv_data$datapath[i],
                                             data.table = FALSE)
          #features_data$name <- chartr(" ", "_", features_data$name)
          features_data$name <- sub(" ", "_", features_data$name)
        }
        else if(csv_data$name[i] == "dist_features.csv"){
          dist_features_data <- data.table::fread(file = csv_data$datapath[i],
                                                  data.table = FALSE)
        }
        else if(csv_data$name[i] == "threats.csv"){
          threats_data <- data.table::fread(file = csv_data$datapath[i],
                                            data.table = FALSE)
          threats_data$name <- chartr(" ", "_", threats_data$name)
        }
        else if(csv_data$name[i] == "dist_threats.csv"){
          dist_threats_data <- data.table::fread(file = csv_data$datapath[i],
                                                 data.table = FALSE)
        }
        else if(csv_data$name[i] == "sensitivity.csv"){
          sensitivity_data <- data.table::fread(file = csv_data$datapath[i],
                                                data.table = FALSE)
        }
        else if(csv_data$name[i] == "boundary.csv"){
          bound_data <- data.table::fread(file = csv_data$datapath[i],
                                          data.table = FALSE)
        }
      }
      
        
      files <- list(pu_data, features_data, 
                    dist_features_data, threats_data, 
                    dist_threats_data, sensitivity_data, 
                    bound_data)
      
      enable("specific_plot")
      enable("validate")

    }
    else{
      return(NULL)
    }
    files
  })
  
  #Change opacity---------------------------------------------------------------
  observeEvent(input$filecsv, {
    
    files <- csv_files()
    shape <- shape_with_csv()
    
    if(!is.null(files) && !is.null(shape)){
      
      popup_map <- sprintf(
          "<strong>id %s</strong><br/>
         Monitoring cost: %g <br/>
         Features: %g <br/>
         Threats: %g <br/>",
          shape[[1]]$id, 
          shape[[1]]$monitoring_cost,
          shape[[1]]$total_distribution_features,
          shape[[1]]$total_distribution_threats) %>% 
          lapply(htmltools::HTML)
      
      leafletProxy("map", data = shape[[1]]) %>%
        clearControls() %>%
        addPolygons(group = "Base",
                    weight = 0.2, 
                    color = "black",
                    smoothFactor = 0.3,
                    fillOpacity = input$range,
                    popup = popup_map, 
                    fillColor = "white", 
                    layerId = ~id, 
                    highlightOptions = highlightOptions(
                      color = "black", 
                      weight = 5,
                      fillOpacity = NULL, 
                      bringToFront = TRUE,
                      sendToBack = TRUE))
      
      updateSelectInput(session = session, "specific_plot",
                        choices = list(`Costs` = shape[[2]],
                                       `Features distribution` = shape[[3]],
                                       `Threats distribution` = shape[[4]]),
                        selected = "total_distribution_features"
      )
      
      
      enable("solutionsImport")
    }
  })
  
  # Change map filtered---------------------------------------------------------
  observeEvent(input$specific_plot, {
    updateRadioGroupButtons(session = session, inputId = "radio", selected = 1)
    plot_maps_explorer()
  }) 

  #error csv -------------------------------------------------------------------
  output$error_filecsv <- renderText({ 
    files <- csv_files()
    
    if(is.null(files))
      paste("")
    else{
      #ERROR 7 INPUTS
      paste("")
    }
  })
  
  
  # Updating maps with data frames----------------------------------------------
  shape_with_csv <- reactive({
    files <- csv_files()
    shape <- shape()
    
    if(!is.null(files) && !is.null(shape)){
      #pu_data
      shapeData <- geo_join(shape, files[[1]], by_sp = "id", by_df = "id")
      shapeData <- shapeData[, !(names(shapeData) %in% c("id.1"))]
      
      
      #features_data
      if(!"name" %in% colnames(files[[2]])){
        files[[2]]$name <- paste0("id_", files[[2]]$id)
      }
      
      df_aux_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
      files[[3]]$feature <- df_aux_features$name
      files[[3]] <- dcast(files[[3]], pu~feature, fill = 0, value.var = "amount")
      
      if(NCOL(files[[3]][,-1]) == 1){
        files[[3]]$total_distribution_features <- as.numeric(files[[3]][,-1]  > 0)
      }
      else{
        files[[3]]$total_distribution_features <- rowSums(files[[3]][,-1] > 0)
      }
      
      shapeData <- geo_join(shapeData, files[[3]], by_sp = "id", by_df = "pu")
      shapeData <- shapeData[, !(names(shapeData) %in% c("pu"))]
      
      #threats_data
      if(!"name" %in% colnames(files[[4]])){
        files[[4]]$name <- paste0("id_", files[[4]]$id)
      }
      df_aux_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
      files[[5]]$threat <- df_aux_threats$name
      aux_costs <- files[[5]]
      files[[5]] <- dcast(files[[5]], pu~threat, fill = 0, value.var = "amount")
      
      if(NCOL(files[[5]][,-1]) == 1){
        files[[5]]$total_distribution_threats <- as.numeric(files[[5]][,-1] > 0)
      }
      else{
        files[[5]]$total_distribution_threats <- rowSums(files[[5]][,-1] > 0)
      }
      
      shapeData <- geo_join(shapeData, files[[5]], by_sp = "id", by_df = "pu")
      shapeData <- shapeData[, !(names(shapeData) %in% c("pu"))]
      
      #actions cost
      aux_costs$threat <- paste0("cost_", aux_costs$threat)
      aux_costs <- dcast(aux_costs, pu~threat, fill = 0, value.var = "action_cost")
      shapeData <- geo_join(shapeData, aux_costs, by_sp = "id", by_df = "pu")
      shapeData <- shapeData[, !(names(shapeData) %in% c("pu"))]

      shape <- list(shapeData,  
                  c(colnames(aux_costs[, -1]), "monitoring_cost"),
                  c(unique(df_aux_features$name), "total_distribution_features"), 
                  c(str_sort(unique(df_aux_threats$name)),"total_distribution_threats"))
      
      shape
    }
  })
  
  #Plotting maps explorer-------------------------------------------------------
  plot_maps_explorer <- reactive({
    if(input$radio  == 1){

      req(input$specific_plot)
      shape <- shape_with_csv()
      index <- which(names(shape[[1]]) == input$specific_plot)

      if(input$specific_plot %in% shape[[3]]){
        if(input$specific_plot == "total_distribution_features"){
          pal <- colorBin(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          title_legend <- "Number of features"
        }
        else{
          factor_feat <- factor(shape[[1]][[index]])
          if(length(levels(factor_feat)) > 5){
            pal <- colorBin(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          }
          else{
            pal <- colorFactor(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          }
          
          #pal <- colorFactor(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          title_legend <- "Feature"
        }
      }
      else if(input$specific_plot %in% shape[[4]]){
        if(input$specific_plot == "total_distribution_threats"){
          pal <- colorBin(palette="inferno", domain=shape[[1]][[index]], na.color="transparent")
          title_legend <- "Number of threats"
        }
        else{
          factor_thr <- factor(shape[[1]][[index]])
          if(length(levels(factor_thr)) > 5){
            pal <- colorNumeric(palette="inferno", domain=shape[[1]][[index]], na.color="transparent")
          }
          else{
            pal <- colorFactor(palette="inferno", domain=shape[[1]][[index]], na.color="transparent")
          }
          title_legend <- "Threats"
        }
      }
      else{
        pal <- colorNumeric( palette="plasma", domain=shape[[1]][[index]], na.color="transparent")
        title_legend <- "Cost"
      }

      popup_map <- sprintf(
        "<strong>id %s</strong><br/>
         Monitoring cost: %g <br/>
         Features: %g <br/>
         Threats: %g <br/>",
        shape[[1]]$id, 
        shape[[1]]$monitoring_cost,
        shape[[1]]$total_distribution_features,
        shape[[1]]$total_distribution_threats) %>% 
        lapply(htmltools::HTML)

      leafletProxy("map", data = shape[[1]]) %>%
          clearControls() %>%
          setShapeStyle(layerId = ~id, 
                        fillColor = ~pal(shape[[1]][[index]]),
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        options = list(group = "Base")) %>%
          addLegend(group = "Base",
                    pal = pal, 
                    title = title_legend,
                    values = shape[[1]][[index]], 
                    position = "bottomleft") 
      
    }
  })
  
  #Plotting maps explorer-------------------------------------------------------
  plot_maps_solutions <- reactive({
    
    if(input$radio  == 2){
      shape <- map_solutions()

      index <- which(names(shape[[1]]) == input$solution_plot)
      #pal <- colorBin(palette="plasma", domain=shape[[1]][[index]], na.color="transparent")
        
      if(input$solution_plot %in% names_solutions_benefits){
        if(grepl( "total", input$solution_plot, fixed = TRUE)){
          pal <- colorBin(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          title_legend <- "Number of features"
        }
        else{
          factor_feat <- factor(shape[[1]][[index]])
          if(length(levels(factor_feat)) > 5){
            pal <- colorBin(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          }
          else{
            pal <- colorFactor(palette="viridis", domain=shape[[1]][[index]], na.color="transparent")
          }
          title_legend <- "Benefit"
        }
      }
      else if(input$solution_plot %in% names_solutions_actions){
        if(input$solution_plot == "total_acts"){
          pal <- colorBin(palette="inferno", domain=shape[[1]][[index]], na.color="transparent")
          title_legend <- "Number of actions"
        }
        else{
          factor_thr <- factor(shape[[1]][[index]])
          if(length(levels(factor_thr)) > 5){
            pal <- colorNumeric(palette="inferno", domain=shape[[1]][[index]], na.color="transparent")
          }
          else{
            pal <- colorFactor(palette="inferno", domain=shape[[1]][[index]], na.color="transparent")
          }
          title_legend <- "Action"
        }
      }
      
      popup_map <- sprintf(
          "<strong>id %s</strong><br/>
           Monitoring cost: %g <br/>
           Features: %g <br/>
           Threats: %g <br/>",
          shape[[1]]$id, 
          shape[[1]]$monitoring_cost,
          shape[[1]]$total_distribution_features,
          shape[[1]]$total_distribution_threats) %>% 
          lapply(htmltools::HTML)
        
        
      leafletProxy("map", data = shape[[1]]) %>%
          clearControls() %>%
          setShapeStyle(layerId = ~id, 
                        fillColor = ~pal(shape[[1]][[index]]),
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        options = list(group = "Base")) %>%
          addLegend(group = "Base",
                    pal = pal, 
                    title = title_legend,
                    values = shape[[1]][[index]], 
                    position = "bottomleft")
      }
  })
  
  
  
  # Observe when change radio-----------------------------------------------------
  observeEvent(input$radio, {
    if(input$radio == 1){
      plot_maps_explorer()
    }
    else if(input$radio == 2){
      plot_maps_solutions()
    }
  })
  
  
  # Updating list of maps filtered----------------------------------------------
  #observe({
  #  files <- csv_files()
  #  shape <- shape_with_csv()
    
  #  if(!is.null(files) && !is.null(shape)){
  #    updateSelectInput(session = session, "specific_plot",
  #                      choices = list(`Costs` = shape[[2]],
  #                                     `Features distribution` = shape[[3]],
  #                                     `Threats distribution` = shape[[4]]),
  #                      selected = "total_distribution_features"
  #    )
      #enable("range")
  #  }
  #})
  

  #PRIORIACTIONS----------------------------------------------------------------
  #Step 1-----------------------------------------------------------------------
  
  step1_data <- reactive({
    files <- csv_files()
    
    if(!is.null(files)){
      
      output$step1 <- renderPrint({
        
        tryCatch({ 
          validated <- prioriactions::inputData(
            files[[1]],
            files[[2]],
            files[[3]],
            files[[4]],
            files[[5]],
            files[[6]],
            files[[7]]);
          print("valided");
          
        }, 
        error = function(e) {
          validated <- prioriactions::inputData(
            files[[1]],
            files[[2]],
            files[[3]],
            files[[4]],
            files[[5]],
            files[[6]],
            files[[7]]);
        })
      })
      validated <- prioriactions::inputData(
        files[[1]],
        files[[2]],
        files[[3]],
        files[[4]],
        files[[5]],
        files[[6]],
        files[[7]])
    }
  })
  
  #Step 2-----------------------------------------------------------------------
  step2_data <- reactive({
    data_step1 <- step1_data()
    
    output$step2 <- renderPrint({
      tryCatch({ 
        validated <- prioriactions::problem(
          x = data_step1, blm = 0, model_type = "minimizeCosts")
        print("valided");
        
      }, 
      error = function(e) {
        validated <- prioriactions::problem(
          x = data_step1, blm = 0, model_type = "minimizeCosts")
      })
    })
    validated <- prioriactions::problem(
      x = data_step1, blm = 0, model_type = "minimizeCosts")
  })
  
  #Step 3-----------------------------------------------------------------------
  step3_data <- reactive({
    data_step2 <- step2_data()
    
    output$step3 <- renderPrint({
      tryCatch({ 
        validated <- prioriactions::solve(
          a = data_step2, time_limit = 10)
        print("valided");
        
      }, 
      error = function(e) {
        validated <- prioriactions::solve(
          a = data_step2, time_limit = 10)
      })
    })
    validated <- prioriactions::solve(
      a = data_step2, time_limit = 10)
  
  })
  
  #Validation step 1------------------------------------------------------------
  observeEvent(input$validate, {
    enable("create")
    step1_data()
  }) 
  
  #Validation step 2------------------------------------------------------------
  observeEvent(input$create, {
    enable("run")
    step2_data()
  })
  
  #Validation step 3------------------------------------------------------------
  observeEvent(input$run, {
    solutions <- step3_data()
    files <- csv_files()
    shape <- shape_with_csv()
    
    actions <- prioriactions::getActions(solutions, format = "large")
    solution_names <- unique(actions$solution_name)
    actions$action <- paste0("action_", actions$action)
    actions <- dcast(actions[, -1], pu~action, fill = 0, value.var = "solution")
  
    enable("solution_name")
    enable("solution_plot")
    
    updateSelectInput(session = session, "solution_name",
                      choices = solution_names,
                      selected = solution_names[1]
    )
    
    shinyjs::show("radio")
    updateRadioGroupButtons(session = session, inputId = "radio", selected = 2)
  
    names_solutions_actions <<- c(colnames(actions[, -1]), "total_acts")
    features_con_and_rec <- str_sort(c(paste0(unique(files[[2]]$name),"_con"), paste0(unique(files[[2]]$name),"_rec")))
    names_solutions_benefits <<- c(features_con_and_rec,
                                   "total_benefit_recovery",
                                   "total_benefit_conservation",
                                   "total_benefit")
    
    updateSelectInput(session = session, "solution_plot",
                      choices = list(`Solutions` = names_solutions_actions,
                                     `Benefits` = names_solutions_benefits),
                      selected = "total_acts"
    )
  })
  
  
  map_solutions <- reactive({
    
    map <- shape_with_csv()
    files <- csv_files()

    if(is.null(input$solutionsImport)){
      solutions <- step3_data()
      actions <- prioriactions::getActions(solutions, format = "large")
      benefit <- prioriactions::getSolutionBenefit(solutions, type = "local")
    }
    else{
      files_outputs <- solution_import_data()
      actions <- files_outputs[[1]]
      benefit <- files_outputs[[2]]
      
    }
    

    actions_filtered <- actions[actions$solution_name == input$solution_name, ]
    actions_filtered$action <- paste0("action_", actions_filtered$action)
    actions_filtered <- dcast(actions_filtered[, -1], pu~action, fill = NA, value.var = "solution")

    if(NCOL(actions_filtered[,-1]) == 1){
      actions_filtered$total_acts <- actions_filtered[,-1]
    }
    else{
      actions_filtered$total_acts <- rowSums(actions_filtered[,-1], na.rm = TRUE)
      actions_filtered$total_acts[actions_filtered$total_acts == 0] <- NA
      
    }
    
    
    
    #actions
    map[[1]] <- geo_join(map[[1]], actions_filtered, by_sp = "id", by_df = "pu")
    map[[1]] <- map[[1]][, !(names(map[[1]]) %in% c("pu"))]
    #print(map[[1]]$action_1)

    #benefit
    benefit_filtered <- benefit[benefit$solution_name == input$solution_name, ]
    benefit_filtered <- right_join(benefit_filtered, files[[2]], by = c("feature" = "id"))
    
    benefit_filtered_con <- benefit_filtered
    benefit_filtered_rec <- benefit_filtered
    
    benefit_filtered_con$benefit.conservation <- round(benefit_filtered_con$benefit.conservation, 3)
    benefit_filtered_rec$benefit.recovery <- round(benefit_filtered_rec$benefit.recovery, 3)
    
    benefit_filtered_con$feature <- paste0(benefit_filtered_con$name, "_con")
    benefit_filtered_rec$feature <- paste0(benefit_filtered_rec$name, "_rec")
    benefit_filtered_con <- dcast(benefit_filtered_con[, -1], pu~feature, fill = 0, value.var = "benefit.conservation")
    benefit_filtered_rec <- dcast(benefit_filtered_rec[, -1], pu~feature, fill = 0, value.var = "benefit.recovery")

    if(NCOL(benefit_filtered_con[,-1]) == 1){
      benefit_filtered_con$total_benefit_conservation <- benefit_filtered_con[,-1]
      benefit_filtered_rec$total_benefit_recovery <- benefit_filtered_rec[,-1]
    }
    else{
      benefit_filtered_con$total_benefit_conservation <- rowSums(benefit_filtered_con[,-1])
      benefit_filtered_rec$total_benefit_recovery <- rowSums(benefit_filtered_rec[,-1])
    }
    
    benefit_filtered_rec$total_benefit <- benefit_filtered_con$total_benefit_conservation +
                                          benefit_filtered_rec$total_benefit_recovery
    
    map[[1]] <- geo_join(map[[1]], benefit_filtered_con, by_sp = "id", by_df = "pu")
    map[[1]] <- map[[1]][, !(names(map[[1]]) %in% c("pu"))]
    
    map[[1]] <- geo_join(map[[1]], benefit_filtered_rec, by_sp = "id", by_df = "pu")
    map[[1]] <- map[[1]][, !(names(map[[1]]) %in% c("pu"))]   
    
    map
  })
  
  
  
  observeEvent(input$solution_plot, {
    updateRadioGroupButtons(session = session, inputId = "radio", selected = 2)
    plot_maps_solutions()
    
    if(grepl( "action", input$solution_plot, fixed = TRUE)){
      shinyjs::show("pie_features_sol")
      shinyjs::show("pie_threats_sol")
    }
    else{
      shinyjs::hide("pie_features_sol")
      shinyjs::hide("pie_threats_sol")
    }
    #shinyjs::hide("pie_features_sol")
    #shinyjs::hide("pie_threats_sol")
  }) 
  

  observeEvent(input$solution_name, {
    plot_maps_solutions()
    
  }) 
  

  # Highchart features ---------------------------------------------------------
  output$pie_features <- renderHighchart({
    
    files <- csv_files()
    map <- shape_with_csv()
    
    if(!is.null(files)){
      if(!"name" %in% colnames(files[[2]])){
        files[[2]]$name <- paste0("id_", files[[2]]$id)
      }
      
      df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
      df_features <- df_features[df_features$pu == 1, ]
      
      
      if(nrow(df_features) == 0){
        df_features[1, ] <- 0
        df_features[1, ]$pu <- 1
        df_features[1, ]$name <- "none"
        df_features[1, ]$amount <- 0
      }
      
      hchart(df_features,"pie", hcaes(x = name, y = amount), size = 200, 
             name = "Amount") %>%
      hc_title(text = paste0("Features in pu #", 1),
               align = "center") %>%
      hc_add_theme(hc_theme_bloom()) %>%
      #hc_colors(viridis(max(map[[1]]$total_distribution_features))) %>%
      hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")
    }
  })
  
  # Highchart threats ----------------------------------------------------------
  output$pie_threats <- renderHighchart({
    
    files <- csv_files()
    map <- shape_with_csv()
    
    
    if(!is.null(files)){
      if(!"name" %in% colnames(files[[4]])){
        files[[4]]$name <- paste0("id_", files[[4]]$id)
      }
      
      df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
      df_threats <- df_threats[df_threats$pu == 1, ]
      
      if(nrow(df_threats) == 0){
        df_threats[1, ] <- 0
        df_threats[1, ]$pu <- 1
        df_threats[1, ]$name <- "none"
        df_threats[1, ]$amount <- 0
      }

      hchart(df_threats,"pie", hcaes(x = name, y = amount), size = 200, 
             name = "Amount") %>%
      hc_title(text = paste0("Threats in pu #", 1),
               align = "center") %>%
      hc_colors(inferno(max(map[[1]]$total_distribution_threats, na.rm = TRUE))) %>%
      hc_add_theme(hc_theme_bloom()) %>%
      hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")
    }
  })

  # Updating pie plot when click shapes-----------------------------------------
  observeEvent(input$map_shape_click, { 
      p <- input$map_shape_click
      files <- csv_files()
      
      if (is.null(p$id))
        return()
      else{
        if(!is.null(files)){
          
          #features
          if(!"name" %in% colnames(files[[2]])){
            files[[2]]$name <- paste0("id_", files[[2]]$id)
          }
          df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
          df_features_filtered <- df_features[df_features$pu == p$id, ]
          
          if(nrow(df_features_filtered) == 0){
            df_features_filtered[1, ] <- 0
            df_features_filtered[1, ]$pu <- p$id
            df_features_filtered[1, ]$name <- "none"
            df_features_filtered[1, ]$amount <- 0
          }
          
          highchartProxy("pie_features") %>%
          hcpxy_set_data(
            type = "pie",
            data = df_features_filtered,
            mapping = hcaes(x = name, y = amount),
            redraw = TRUE
          ) %>%
            hcpxy_update(
              title = list(text = paste0("<b>Features in pu #", p$id, "</b>"), align = "center")
            )
          
          #threats
          if(!"name" %in% colnames(files[[4]])){
            files[[4]]$name <- paste0("id_", files[[4]]$id)
          }
          df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
          df_threats_filtered <- df_threats[df_threats$pu == p$id, ]
          
          if(nrow(df_threats_filtered) == 0){
            df_threats_filtered[1, ] <- 0
            df_threats_filtered[1, ]$pu <- p$id
            df_threats_filtered[1, ]$name <- "none"
            df_threats_filtered[1, ]$amount <- 0
          }
          
          highchartProxy("pie_threats") %>%
          hcpxy_set_data(
            type = "pie",
            data = df_threats_filtered,
            mapping = hcaes(x = name, y = amount),
            redraw = TRUE
          ) %>%
          hcpxy_update(
              title = list(text = paste0("<b>Threats in pu #", p$id, "</b>"), align = "center")
          )
          
      }
    }
  })
  
  
  # Highchart Solutions-----------------------------------------------------------

    # Highchart features ---------------------------------------------------------
  output$pie_features_sol <- renderHighchart({
    
    files <- csv_files()
    map <- shape_with_csv()
    
    if(!is.null(files)){
      #if(!"name" %in% colnames(files[[2]])){
      #  files[[2]]$name <- paste0("id_", files[[2]]$id)
      #}
      
      #df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
      #df_features <- df_features[df_features$pu == 1, ]
      df_features <- data.frame (name  ="none",
                                          amount = 0
      )
      
      hchart(df_features,"pie", hcaes(x = name, y = amount), size = 200, 
             name = "Amount") %>%
        #hc_title(text = paste0("Features beneficiated with this action in pu #", 1),
        #         align = "center") %>%
        hc_add_theme(hc_theme_bloom()) %>%
        #hc_colors(viridis(max(map[[1]]$total_distribution_features))) %>%
        hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")
    }
  })
  
  # Highchart threats ----------------------------------------------------------
  output$pie_threats_sol <- renderHighchart({
    
    files <- csv_files()
    map <- shape_with_csv()
    
    
    if(!is.null(files)){
      #if(!"name" %in% colnames(files[[4]])){
      #  files[[4]]$name <- paste0("id_", files[[4]]$id)
      #}
      
      #df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
      #df_threats <- df_threats[df_threats$pu == 1, ]
      df_threats <- data.frame (name  ="none",
                                 amount = 0
      )
      
      hchart(df_threats,"pie", hcaes(x = name, y = amount), size = 200, 
             name = "Amount") %>%
        #hc_title(text = paste0("Threats in pu #", 1),
        #         align = "center") %>%
        #hc_colors(inferno(max(map[[1]]$total_distribution_threats))) %>%
        hc_add_theme(hc_theme_bloom()) %>%
        hc_tooltip(pointFormat = "<b>Value:</b> {point.y} <br>
                 <b>Percentage</b> {point.percentage:,.2f}%")
    }
  })
  
  # Updating pie plot when click shapes-----------------------------------------
  observeEvent(input$map_shape_click, { 
    p <- input$map_shape_click
    files <- csv_files()
    
    if (is.null(p$id))
      return()
    else{
      if(grepl( "action", input$solution_plot, fixed = TRUE)){
        shape <- map_solutions()
        index <- which(names(shape[[1]]) == input$solution_plot)
        map <- shape[[1]][[index]]
        index_pu <- which(shape[[1]]$id == p$id)

        if(!is.na(map[index_pu])){
          
          shinyjs::show("pie_features_sol")
          shinyjs::show("pie_threats_sol")
          
          #features
          if(!"name" %in% colnames(files[[2]])){
            files[[2]]$name <- paste0("id_", files[[2]]$id)
          }
          df_features <- right_join(files[[3]], files[[2]], by = c("feature" = "id"))
          df_features_filtered <- df_features[df_features$pu == p$id, ]
          
          
          threat_index <- readr::parse_number(input$solution_plot)
          features_sensitivities <- files[[6]][files[[6]]$threat == threat_index,]
          df_features_filtered <- subset(df_features_filtered, feature %in% features_sensitivities$feature)
          
          highchartProxy("pie_features_sol") %>%
            hcpxy_set_data(
              type = "pie",
              data = df_features_filtered,
              mapping = hcaes(x = name, y = amount),
              redraw = TRUE
            ) %>%
            hcpxy_update(
              title = list(text = paste0("<b>Features beneficiated with this action in pu #", p$id, "</b>"), align = "center")
            )
          
          #threats
          if(!"name" %in% colnames(files[[4]])){
            files[[4]]$name <- paste0("id_", files[[4]]$id)
          }
          df_threats <- right_join(files[[5]], files[[4]], by = c("threat" = "id"))
          df_threats_filtered <- df_threats[df_threats$pu == p$id, ]
          df_threats_filtered <- df_threats_filtered[df_threats_filtered$threat == threat_index, ]

          highchartProxy("pie_threats_sol") %>%
            hcpxy_set_data(
              type = "pie",
              data = df_threats_filtered,
              mapping = hcaes(x = name, y = amount),
              redraw = TRUE
            ) %>%
            hcpxy_update(
              title = list(text = paste0("<b>Threats abated by this action in pu #", p$id, "</b>"), align = "center")
            )
        }
        else{
          shinyjs::hide("pie_features_sol")
          shinyjs::hide("pie_threats_sol")
        }
      }
    }
  })
  
  #Solutions import ------------------------------------------------------------
  observeEvent(input$solutionsImport, { 
    solution_import_data()
  })
  
  
  solution_import_data <- reactive({
    #req(input$solutionsImport)
    
    if(!is.null(req(input$solutionsImport))){
      req(input$solutionsImport)
      files <- csv_files()
      sol_data <- input$solutionsImport
      tempdirname <- dirname(sol_data$datapath[1])
      
      
      # Rename files
      for (i in 1:nrow(sol_data)) {
        #NO ESTÁ FUNCIONANDO
        ext <- tools::file_ext(sol_data$datapath[i])
        shiny::validate(need(ext == "txt", "Please upload an txt file"))
        
        validate(need(sol_data$name[i] %in% c("output_actions.txt", 
                                              "output_benefits.txt"), "Please upload an valid file"))
        
        
        if(sol_data$name[i] == "output_actions.txt"){
          action_data <- data.table::fread(file = sol_data$datapath[i],
                                       data.table = FALSE)
          
          action_data$solution[action_data$solution == 0] <- NA
          
          solution_names <- unique(action_data$solution_name)
          action_names <-  unique(paste0("action_", action_data$action))
        }
        else if(sol_data$name[i] == "output_benefits.txt"){
          benefit_data <- data.table::fread(file = sol_data$datapath[i],
                                             data.table = FALSE)
        }
      }
      
      files_outputs <- list(action_data, benefit_data)
      
      enable("solution_name")
      enable("solution_plot")
      
      updateSelectInput(session = session, "solution_name",
                        choices = solution_names,
                        selected = solution_names[1]
      )

      shinyjs::show("radio")
      updateRadioGroupButtons(session = session, inputId = "radio", selected = 2)
      
      names_solutions_actions <<- c(action_names, "total_acts")
      features_con_and_rec <- str_sort(c(paste0(unique(files[[2]]$name),"_con"), paste0(unique(files[[2]]$name),"_rec")))
      names_solutions_benefits <<- c(features_con_and_rec,
                                     "total_benefit_recovery",
                                     "total_benefit_conservation",
                                     "total_benefit")
      
      updateSelectInput(session = session, "solution_plot",
                        choices = list(`Solutions` = names_solutions_actions,
                                       `Benefits` = names_solutions_benefits),
                        selected = "total_acts"
      )
      files_outputs
    }
    else{
      return(NULL)
    }
  })
}


