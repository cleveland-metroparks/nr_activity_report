library(shiny)
library(shinydashboard)
library(tibble)
library(pool)
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

# This method of importing data ignores geometry collumn and generates a warning:
#  Warning in postgresqlExecStatement(conn, statement, ...) :
#   RS-DBI driver warning: (unrecognized PostgreSQL field type geometry (id:85253) in column 13)
# But see use of st_read below for alternative

nr_act = pool %>% tbl(in_schema(Schema,"nr_activity_tracking"))
nr_act_df = as.data.frame(nr_act)
# Get username from email
nr_act_df$fulcrum_user=sapply(strsplit(nr_act_df$created_by,split="@"), `[`, 1)
user_list=nr_act_df$fulcrum_user %>% unique() %>% sort()
user_list_name=case_when(user_list == "exs" ~ "Shaffer",
                         user_list == "jdc" ~ "Cepek",
                         user_list == "jjp1" ~ "Philipps",
                         user_list == "jmg2" ~ "Grieser",
                         user_list == "pdl" ~ "Lorch",
                         user_list == "tjk" ~ "Krynak",
                         user_list == "vcs" ~ "Carter-Stone",
                         TRUE ~ user_list)
names(user_list)=user_list_name
nr_act_df$fulcrum_user2=case_when(nr_act_df$fulcrum_user == "exs" ~ "Shaffer",
                                  nr_act_df$fulcrum_user == "jdc" ~ "Cepek",
                                  nr_act_df$fulcrum_user == "jjp1" ~ "Philipps",
                                  nr_act_df$fulcrum_user == "jmg2" ~ "Grieser",
                                  nr_act_df$fulcrum_user == "pdl" ~ "Lorch",
                                  nr_act_df$fulcrum_user == "tjk" ~ "Krynak",
                                  nr_act_df$fulcrum_user == "vcs" ~ "Carter-Stone",
                                  TRUE ~ nr_act_df$fulcrum_user)
nr_act_df$staff = ifelse(is.na(nr_act_df$user_name_multiple),
                    ifelse(nr_act_df$user_name=="NR seasonal(s)",
                        ifelse(is.na(nr_act_df$staff_supervisor),
                                paste("NR seasonal(s)",nr_act_df$staff_supervisor_other,sep=","),
                                paste("NR seasonal(s)",nr_act_df$staff_supervisor,sep=",")),
                                    nr_act_df$user_name),nr_act_df$user_name_multiple)
nr_act_df$activity_category2=ifelse(is.na(nr_act_df$activity_category),"Other",nr_act_df$activity_category)
nr_act_df$activity=ifelse(is.na(nr_act_df$activity_category),
                          nr_act_df$activity_category_other,nr_act_df$activity_category)
nr_act_df$activities_performed2=ifelse(is.na(nr_act_df$activities_performed),"Other",nr_act_df$activities_performed)
nr_act_df$activities_performed_all=ifelse(is.na(nr_act_df$activities_performed),
                          nr_act_df$activities_performed_other,nr_act_df$activities_performed)
nr_act_df$reservations2=ifelse(is.na(nr_act_df$reservations),"Other",nr_act_df$reservations)
nr_act_df$location=ifelse(is.na(nr_act_df$reservations),
                          nr_act_df$reservations_other,nr_act_df$reservations)
nr_act_df$start_date_time=as.POSIXct(strptime(paste(nr_act_df$start_date,nr_act_df$start_time),"%Y-%m-%d %H:%M", tz = "EST5EDT"))
nr_act_df$start_date2=as.Date(nr_act_df$start_date_time)
nr_act_df$end_date_time=as.POSIXct(strptime(paste(nr_act_df$end_date,nr_act_df$end_time),"%Y-%m-%d %H:%M", tz = "EST5EDT"))
nr_act_df$end_date2=as.Date(nr_act_df$end_date_time)
end_date_range=range(nr_act_df$end_date2)
#nr_act_df$duration_dates=paste(nr_act_df$start_date,nr_act_df$end_date,sep=" to ")
days=round(as.numeric(difftime(nr_act_df$end_date_time,nr_act_df$start_date_time,units="days")),1)
hours=round(as.numeric(difftime(nr_act_df$end_date_time,nr_act_df$start_date_time,units="hours")),1)
nr_act_df$duration_days=ifelse(days>=1,days,NA)
nr_act_df$duration_hours=ifelse(days<1,hours,NA)
nr_act_df$grant_id2=ifelse(is.na(nr_act_df$grant_id),"Other",nr_act_df$grant_id)
nr_act_df$grant=ifelse(is.na(nr_act_df$grant_id),
                       paste("Other: ",nr_act_df$grant_id_other),nr_act_df$grant_id)
nr_act_df$incident_type2=ifelse(is.na(nr_act_df$incident_type),nr_act_df$incident_type_other,nr_act_df$incident_type)
nr_act_df$species2=ifelse(is.na(nr_act_df$species),nr_act_df$species_other,nr_act_df$species)
nr_act_df$outcome2=ifelse(is.na(nr_act_df$outcome),nr_act_df$outcome_other,nr_act_df$outcome)
comment_fields=names(nr_act_df)[grep("comment",names(nr_act_df))]
# This version will combine all comments
paste_noNA <- function(x,sep=", ") 
    gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) )
nr_act_df$comments_combined=apply(nr_act_df[comment_fields],1,paste_noNA,sep="; ")
# This version will find first comment only
#nr_act_df$comments_combined=do.call(coalesce,nr_act_df[comment_fields])
purpose_fields=names(nr_act_df)[grep("purpose",names(nr_act_df))]
nr_act_df$purpose_combined=apply(nr_act_df[purpose_fields],1,paste_noNA,sep="; ")
description_fields=names(nr_act_df)[grep("description",names(nr_act_df))]
nr_act_df$description_combined=apply(nr_act_df[description_fields],1,paste_noNA,sep="; ")
nr_act_df$meeting_attendance=ifelse(is.na(nr_act_df$head_count),NA,paste("Meeting attendance:",
                                                                         nr_act_df$head_count))
nr_act_df$report_text=apply(nr_act_df[c("activity","activities_performed_all","purpose_combined",
                                        "description_combined","meeting_location","meeting_attendees",
                                        "meeting_attendance","comments_combined")],1,paste_noNA,sep="; ")
nr_act_df$report_text2=apply(nr_act_df[c("fulcrum_user2","report_text","staff")],1,paste_noNA,sep=": ")
# nr_act_df$report_text3=paste("**",nr_act_df$fulcrum_user2,":** ",nr_act_df$report_text," (Staff: ",
#                              nr_act_df$staff,")",sep="")
nr_act_df$report_text3=paste(nr_act_df$fulcrum_user2,": ",nr_act_df$report_text,sep="")

nr_act_df$dir_report_category=case_when(nr_act_df$activity_category2 %in% c("Wildlife Incident",
                                                                            "Deer Management",
                                                                            "Deer Program Preparation") ~ "Fish & Wildlife",
                                        nr_act_df$activity_category2 == "Inventory and Monitoring" ~ "Inventory & Monitoring",
                                        nr_act_df$activity_category2 %in% c("Invasive Plant Assessment",
                                                                            "Invasive Plant Treatment") ~ "Invasive Plants",
                                        nr_act_df$activity_category2 %in% c("Meeting/Conference/Outreach",
                                                                            "Management Planning",
                                                                            "Training") ~ "Meeting",
                                        nr_act_df$activity_category2 %in% c("Planting/Seeding",
                                                                            "General Vegetation Management",
                                                                            "Field Reconnaissance",
                                                                            "Site preparation (discing, plowing, etc)",
                                                                            "Water Level Management",
                                                                            "Green Infrastructure Maintenance",
                                                                            "Planting Maintenance",
                                                                            "Forestry/Tree Stand Improvement",
                                                                            "Fire Prep") ~ "Site Managament",
                                        TRUE ~ "Other" # Currently includes "Other","Interdepartmental Assistance","Equipment Maintenance/Transport"
)
nr_act_df$hover_text = paste(sep="<br/>",paste0("ID: ",nr_act_df$fulcrum_id),
                             paste0("<b>",nr_act_df$location,"</b>"),
                             paste0("Activity type: ",nr_act_df$activity),
                             paste0("Start date: ",nr_act_df$start_date),
                             paste0("Activity performed: ",nr_act_df$activities_performed_all),
                             paste0("Staff: ",nr_act_df$staff),
                             paste0("Duration: ",ifelse(is.na(nr_act_df$duration_days),
                                                        paste(nr_act_df$duration_hours,"hours"),
                                                        paste(nr_act_df$duration_days,"days"))),
                             paste0("Grant: ",nr_act_df$grant)
)
# lapply(seq(nrow(nr_act_df)),function(){
#     paste0("ID: ",nr_act_df[i,fulcrum_id],"<br/>",nr_act_df[i,location])
# }

# nr_act_df=SpatialPointsDataFrame(coords = c(nr_act_df$longitude,nr_act_df$latitude),
#                                  data = nr_act_df, 
#                                  proj4string = CRS("+init=epsg:4326"))
# t_nr_act=nr_act_df[200:300,]
# tlist=as.list(by(t_nr_act[c("fulcrum_user2","activity","activities_performed_all","comments_combined")],
#                  t_nr_act[c("reservations2","dir_report_category")],identity))

# Need to decide whether to do this here or move it to a postgis view or table

#session$onSessionEnded(function() { dbDisconnect(conn) })
library(lubridate)
nr_plant = pool %>% tbl(in_schema(Schema,"planting_activity_view")) %>% as.data.frame() %>% select(-geometry,-activity_geometry)
nr_plant$planting_photos_url=paste0("<a href='",nr_plant$planting_photos_url,"'>",nr_plant$planting_photos_url,"</a>")
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

acres=htmlEscape(as.character(round(nr_act_poly$area_acres,2)))

pool::poolClose(pool)
