library(shiny)
library(shinydashboard)
library(pool)
library(lubridate)
library(tibble)
library(rlang)
library(dplyr)
library(dbplyr)
library(postGIStools)
library(rgdal)
library(htmltools)
library(sf)

# This is intended to bring in params that are not version controlled and not in repo
source("loginparams.R")

pool <- pool::dbPool(drv = RPostgreSQL::PostgreSQL(),
    dbname = DBname,
    host = Host, user = User, password = Password,port=Port)

nr_act_df = pool %>% 
    st_read(query = paste0("SELECT a.* from ",Schema,".nr_activity_sum_report_view AS a;"),geometry_column = "geometry")

nr_plant = pool %>% 
    st_read(query = paste0("SELECT a.* from ",Schema,".planting_activity_view AS a;"),geometry_column = "activity_geometry")


# It seems I can use htmltools in global (server side?) and not have websockets choke on them
nr_plant$planting_photos_url=paste0("<a href='",nr_plant$planting_photos_url,"'> photos </a>")
nr_plant_sum = nr_plant %>% 
    select(material_type_name_,
           number_planted,
           quantity_of_seed_used_lbs,
           staff,
           grant_funded_project,
           reservations,
           start_date,
           duration_days,duration_hours,
           number_of_volunteers_present,number_of_staff_present) %>% 
    group_by(year=year(as.Date(start_date)),staff,reservations,material_type_name_) %>%
    summarize(total_planted = sum(number_planted),
              total_seed_lbs = sum(quantity_of_seed_used_lbs),
              total_days = sum(duration_days),
              total_hours = sum(duration_hours),
              total_volunteers = sum(number_of_volunteers_present),
              total_staff = sum(number_of_staff_present)) %>%
    mutate(total_volunteer_hours = total_hours*total_volunteers,
           total_staff_hours = total_hours*total_staff,
           total_staff_days = total_days*total_staff)

nr_plant_sum_year_res = nr_plant_sum %>%
    group_by(year,reservations,material_type_name_) %>%
    summarize(total_planted = sum(total_planted),
              total_seed_lbs = sum(total_seed_lbs),
              total_days = sum(total_days),
              total_hours = sum(total_hours),
              total_volunteers = sum(total_volunteers),
              total_staff = sum(total_staff)) %>%
    mutate(total_volunteer_hours = total_hours*total_volunteers,
           total_staff_hours = total_hours*total_staff,
           total_staff_days = total_days*total_staff)

nr_plant_sum_latest_year = nr_plant_sum_year_res %>%
    filter(year == max(year)) %>%
    group_by(material_type_name_) %>%
    summarize(total_planted = sum(total_planted),
              total_seed_lbs = sum(total_seed_lbs),
              total_days = sum(total_days),
              total_hours = sum(total_hours),
              total_volunteers = sum(total_volunteers),
              total_staff = sum(total_staff)) %>%
    mutate(total_volunteer_hours = total_hours*total_volunteers,
           total_staff_hours = total_hours*total_staff,
           total_staff_days = total_days*total_staff)

nr_act_poly = pool %>% st_read(query = "SELECT a.feature_id,
                                                a.fulcrum_id as activity_fulcrum_id,
                                                a.geom,
                                                a.area_acres,
                                                b.*
                                        FROM management.nr_activity_polygons AS a
                                            LEFT JOIN management.nr_activity_tracking AS b
                                        ON a.fulcrum_id = b.fulcrum_id",geometry_column = "geom") %>%
    st_transform("+proj=longlat +datum=WGS84")
# It seems I can use htmltools in global (server side?) and not have websockets choke on them
acres=htmlEscape(as.character(round(nr_act_poly$area_acres,2)))

pool::poolClose(pool)
