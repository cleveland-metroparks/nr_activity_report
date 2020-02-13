library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(htmltools)

function(input, output, session) {

# Not sure this is still needed
    vals <- reactiveValues()
## Interactive Map ###########################################
    
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
                group = "Mapbox"
            ) %>%
            addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI WorldImagery") %>%
            setView(lng = -81.72, lat = 41.45, zoom = 12)
    })

    # A reactive expression that returns a set of activities filtered by the UI choices
    nr_act_filtered <- reactive({
        nr_act_df %>%
            filter(
                end_date2 >= input$dateRange[1],
                end_date2 <= input$dateRange[2],
                is.null(input$user2) | fulcrum_user %in% input$user2,
                is.null(input$locations) | location %in% input$locations,
                is.null(input$activities) | activity %in% input$activities
            )
        })
    
    # A reactive expression that returns the set of activities that are
    # in bounds right now
    nr_act_InBounds <- reactive({
        if (is.null(input$map_bounds))
            return(nr_act_df[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)

        nr_act_filtered() %>%
            subset(latitude >= latRng[1] & latitude <= latRng[2] &
                   longitude >= lngRng[1] & longitude <= lngRng[2])
    })

    observe({
        vals$N = nrow(nr_act_InBounds())
    })
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    
    # The polygon layer needs to be merged with the data from the points layers to allow more data to be displayed on the popups
    observe({
        colorBy <- input$color
        naf=nr_act_filtered()
        napf=nr_act_poly

        if (colorBy == "fulcrum_user") {
            colorData <- naf$fulcrum_user
            pal <- colorFactor("viridis", colorData)
        } else {
            colorData <- naf$activity_category2
            pal <- colorFactor("viridis", colorData)
        }
        
        leafletProxy("map", data = naf) %>%
            clearMarkers() %>% clearShapes() %>%
            setView(lng = -81.65, lat = 41.38, zoom = 10) %>% 
            addCircleMarkers(~longitude, ~latitude, radius=10, layerId=~fulcrum_id,
                       stroke=FALSE, fillOpacity=1.0, fillColor=pal(colorData),
                       label=~lapply(hover_text,HTML),
                       group = "Activities") %>% addTiles() %>%
            addPolygons(data=napf,color="black",weight=1,smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.2,fillColor = "grey",
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                        bringToFront = FALSE),label=paste0(napf$user_name_multiple,', ',
                                                          napf$start_date,' start_date , ',
                                                          napf$activity_category,', ',
                                                          acres,' acres'),
                        group = "Polygons") %>%
            addLayersControl(baseGroups = c("Mapbox","Open Topo Map","ESRI WorldImagery"),
                             overlayGroups = c("Polygons","Activities"),
                             position = "bottomright") %>%
            addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                      layerId="colorLegend")  #%>% 
            # addLegend("bottomright", pal=pal2, values=colorData2, title=paste("Polygon colors:",colorBy),
            #       layerId="colorLegend")
    })
    
    # output$LocationByActivityCount <- DT::renderDataTable({
    #     df3 = nr_act_InBounds() %>%
    #         select(
    #             Activity=activity,
    #             Location=location
    #         ) %>%
    #         table()
    #     
    #     DT::datatable(df3)
    # })

# Removed for now since websockets are not working    
    # Show a popup at the given location
    # showActivityPopup <- function(id, lat, lng) {
    #     selectedActivity <- nr_act_df[nr_act_df$fulcrum_id == id,]
    #     content <- as.character(tagList(
    #         tags$h5("ID:", selectedActivity$fulcrum_id),
    #         tags$strong(HTML(sprintf("%s", selectedActivity$location))),tags$br(),
    #         sprintf("Activity type: %s", selectedActivity$activity), tags$br(),
    #         sprintf("Start date: %s", selectedActivity$start_date), tags$br(),
    #         if(is.na(selectedActivity$activities_performed_all)) {sprintf("")}
    #         else {sprintf("Activity performed: %s", selectedActivity$activities_performed_all)}, tags$br(),
    #         sprintf("Staff: %s", selectedActivity$staff), tags$br(),
    #         sprintf("Duration: %s", ifelse(is.na(selectedActivity$duration_days),
    #                                        paste(selectedActivity$duration_hours,"hours"),
    #                                        paste(selectedActivity$duration_days,"days"))), tags$br(),
    #         if(selectedActivity$grant=="Other:  NA") {sprintf("")}
    #         else {sprintf("Grant: %s", selectedActivity$grant)}, tags$br()
    #     ))
    #     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    # }
# Removed for now since websockets are not working    
    # When map is clicked, show a popup with activity info
    # observe({
    #     leafletProxy("map") %>% clearPopups()
    #     event <- input$map_marker_click
    #     if (is.null(event))
    #         return()
    #     
    #     isolate({
    #         showActivityPopup(event$id, event$lat, event$lng)
    #     })
    # })
    
##    summary_table ##########################################
    observe({
        locations <- if (is.null(input$user2)) character(0) else {
            nr_act_filtered() %>%
                filter(fulcrum_user %in% input$user2) %>%
                `$`('location') %>%
                unique() %>%
                sort()
        }
        stillSelected <- isolate(input$locations[input$locations %in% locations])
        updateSelectInput(session, "locations", choices = locations,
                          selected = stillSelected)
    })

    observe({
        activities <- if (is.null(input$user2)) character(0) else {
            nr_act_filtered() %>%
                filter(fulcrum_user %in% input$user2,
                       is.null(input$locations) | location %in% input$locations) %>%
                `$`('activity') %>%
                unique() %>%
                sort()
        }
        stillSelected <- isolate(input$activities[input$activities %in% activities])
        updateSelectInput(session, "activities", choices = activities,
                          selected = stillSelected)
    })

    observe({
        grants <- if (is.null(input$user2)) character(0) else {
            nr_act_filtered() %>%
                filter(fulcrum_user %in% input$user2,
                       is.null(input$locations) | location %in% input$locations,
                       is.null(input$activities) | location %in% input$activities) %>%
                `$`('grant_id2') %>%
                unique() %>%
                sort()
        }
        stillSelected <- isolate(input$grants[input$grants %in% grants])
        updateSelectInput(session, "grants", choices = grants,
                          selected = stillSelected)
    })
    
    observeEvent(input$reset, {
        reset("form")
    })
    
    observe({
        if (is.null(input$goto))
            return()
        isolate({
            map <- leafletProxy("map")
            map %>% clearPopups()
            dist <- 0.05
            id <- input$goto$id
            lat <- input$goto$lat
            lng <- input$goto$lng
            showActivityPopup(id, lat, lng)
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        })
    })
    
    output$summary_table <- DT::renderDataTable({
        df2 = nr_act_filtered() %>%
# Removed for now since websockets are not working
            # mutate(action_jump_to_map = paste('<a class="go-map" href="" data-lat="',
            #                       latitude,
            #                       '" data-long="',
            #                       longitude,
            #                       '" data-id="',
            #                       fulcrum_id,
            #                       '"><i class="fa fa-crosshairs"></i></a>',
            #                       sep="")
            # ) %>%
            select(
                Entered_by=fulcrum_user2,
                Staff=staff,
                Activity=activity,
                Location=location,
                Start_date=start_date2,
                Incident_type=incident_type2,
                Species=species2,
                Outcome=outcome2,
                Duration_days=duration_days,
                Duration_hours=duration_hours,
                Grant_description=grant #, Removed for now since websockets are not working
 #               Jump_to_map=action_jump_to_map
            )

        action <- DT::dataTableAjax(session, df2)

        DT::datatable(df2, options = list(ajax = list(url = action)), escape = FALSE)
    })

    # Generate report for output on button click
    # For PDF output, change this to ".pdf" from ".doc"
    output$dir_report <- downloadHandler(
        filename = paste0("nr_act_filtered_report_",
                         Sys.Date(),".doc"),
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport_Rmd=file.path(tempdir(),"nr_act_filtered_report.Rmd")
            t_nr_act=nr_act_filtered()
            sink(file=tempReport_Rmd,append=F,type="output")
                cat("---","\n",
                    "title: Directors report update","\n",
                    "author: Pat Lorch","\n",
                    "date: ",as.character(Sys.Date()),"\n",
                    "---","\n","\n",sep="")
                for(i in unique(t_nr_act$dir_report_category)){
                    cat("_______________________________________________\n")
                    cat("##",i,"\n")
                    for(j in unique(t_nr_act$reservations2)) {
                        len_k=length(t_nr_act$report_text3[t_nr_act$dir_report_category==i &
                                                               t_nr_act$reservations2==j])
                        if(len_k<1)
                            break
                        cat("###",j,"\n")
                        for(k in t_nr_act$report_text3[t_nr_act$dir_report_category==i &
                                                       t_nr_act$reservations2==j]) {
                            cat(k,"  \n")
                        }
                        cat("\n")
                    }
                    cat("\n","\n")
                }
            sink()

            # Knit the document, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport_Rmd, output_file = file,
                              envir = new.env(parent = globalenv())
            )
        }
    )

    output$download_filtered_table = downloadHandler(
        filename = paste0("nr_act_filtered_table_",
                         Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_act_filtered(), file, row.names = FALSE)
        }
    )

    output$download_wildlife_table = downloadHandler(
        filename = paste0("nr_act_wildlife_table_",
                          Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_act_filtered() %>%
                          filter(activity_category == "Wildlife Incident") %>% 
                          count(Reservation = reservations2,Species = species2,Outcome = outcome2), 
                      file, row.names = FALSE)
        }
    )
    
    
##    map_table ##########################################################
    output$map_table <- DT::renderDataTable({
        df4 = nr_act_InBounds()
        DT::datatable(df4, escape = FALSE)
    })

    output$num_in_map = renderText({
        paste("Points in map area = ",vals$N)
    })

    output$download_map_table = downloadHandler(
        filename = paste0("nr_act_map_table_",
                         Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_act_InBounds(), file, row.names = FALSE)
        }
    )

    output$planting_table <- DT::renderDataTable({
        planting = nr_plant
        DT::datatable(planting, escape = FALSE)
    })

    output$download_planting_table = downloadHandler(
        filename = paste0("planting_table_",
                          Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_plant, file, row.names = FALSE)
        }
    )
    
    output$download_plantsum_table = downloadHandler(
        filename = paste0("plantsum_table_",
                          Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_plant_sum, file, row.names = FALSE)
        }
    )
    
    output$download_plantsum2_table = downloadHandler(
        filename = paste0("plantsum2_table_",
                          Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_plant_sum_year_res, file, row.names = FALSE)
        }
    )

    output$download_plantsum3_table = downloadHandler(
        filename = paste0("plantsum3_latest_year_table_",
                          Sys.Date(),".csv"),
        content = function(file) {
            write.csv(nr_plant_sum_latest_year, file, row.names = FALSE)
        }
    )
    
##    main_table ##########################################################
    output$full_table <- DT::renderDataTable({
        df = nr_act_filtered()

        DT::datatable(df, escape = FALSE)
    })
    
    output$download_whole_table = downloadHandler(
        filename = paste("nr_act_whole_table_",
                         Sys.Date(),".csv",sep=""),
        content = function(file) {
            write.csv(nr_act_df, file, row.names = FALSE)
        }
    )
    
}