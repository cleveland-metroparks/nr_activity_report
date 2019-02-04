library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

function(input, output, session) {

    vals <- reactiveValues()
## Interactive Map ###########################################
    
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
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
    observe({
        colorBy <- input$color
        naf=nr_act_filtered()

        if (colorBy == "fulcrum_user") {
            colorData <- naf$fulcrum_user
            pal <- colorFactor("viridis", colorData)
        } else {
            colorData <- naf$activity_category2
            pal <- colorFactor("viridis", colorData)
        }
        
        leafletProxy("map", data = naf) %>%
            clearMarkers() %>%
            addCircleMarkers(~longitude, ~latitude, radius=10, layerId=~fulcrum_id,
                       stroke=FALSE, fillOpacity=1.0, fillColor=pal(colorData)) %>%
            addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                      layerId="colorLegend")
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

    # Show a popup at the given location
    showActivityPopup <- function(id, lat, lng) {
        selectedActivity <- nr_act_df[nr_act_df$fulcrum_id == id,]
        content <- as.character(tagList(
            tags$h5("ID:", selectedActivity$fulcrum_id),
            tags$strong(HTML(sprintf("%s", selectedActivity$location))),tags$br(),
            sprintf("Activity type: %s", selectedActivity$activity), tags$br(),
            if(is.na(selectedActivity$activities_performed_all)) {sprintf("")}
            else {sprintf("Activity performed: %s", selectedActivity$activities_performed_all)}, tags$br(),
            sprintf("Staff: %s", selectedActivity$staff), tags$br(),
            sprintf("Duration: %s", ifelse(is.na(selectedActivity$duration_days),
                                           paste(selectedActivity$duration_hours,"hours"),
                                           paste(selectedActivity$duration_days,"days"))), tags$br(),
            if(selectedActivity$grant=="Other:  NA") {sprintf("")}
            else {sprintf("Grant: %s", selectedActivity$grant)}, tags$br()
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    }
    
    # When map is clicked, show a popup with activity info
    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_marker_click
        if (is.null(event))
            return()
        
        isolate({
            showActivityPopup(event$id, event$lat, event$lng)
        })
    })
    
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

            mutate(action_jump_to_map = paste('<a class="go-map" href="" data-lat="',
                                  latitude,
                                  '" data-long="',
                                  longitude,
                                  '" data-id="',
                                  fulcrum_id,
                                  '"><i class="fa fa-crosshairs"></i></a>',
                                  sep="")
            ) %>%
            select(
                Entered_by=fulcrum_user2,
                Staff=staff,
                Activity=activity,
                Location=location,
                Start_date=start_date2,
                Duration_days=duration_days,
                Duration_hours=duration_hours,
                Grant_description=grant,
                Jump_to_map=action_jump_to_map
            )

        action <- DT::dataTableAjax(session, df2)

        DT::datatable(df2, options = list(ajax = list(url = action)), escape = FALSE)
    })
    
##    map_table ##########################################################
    output$map_table <- DT::renderDataTable({
        df4 = nr_act_InBounds()
        DT::datatable(df4, escape = FALSE)
    })

    output$num_in_map = renderText({
        paste("Points in map area = ",vals$N)
    })
        
##    main_table ##########################################################
    output$full_table <- DT::renderDataTable({
        df = nr_act_filtered()

        DT::datatable(df, escape = FALSE)
    })
}