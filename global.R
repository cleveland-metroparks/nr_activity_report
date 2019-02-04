library(shiny)
library(shinydashboard)
library(tibble)
library(pool)
library(rlang)
library(dplyr)
library(dbplyr)

Schema = "management"

#    The settings below using localhost and 63333 require you have run the command to tunnel safely into the database like:
#    ssh -i /c/Users/pdl/.ssh/id_rsa -N -L 63333:localhost:5432 plorch@10.0.0.27 &
#    This method is more secure.  However, it is not working on the server.  It works from R shiny both in window and in browser.
#    If user has blessed IP address, then direct access will work and comments on "host =" lines below can be toggled.
#    This is currently the only method that works on the server since the tunnel would need to be on the server and cannot be generic.
#    Need to know if this is a security issue to have shiny server tunnel to PostgreSQL server.

pool <- pool::dbPool(drv = RPostgreSQL::PostgreSQL(),
    dbname = "NR_monitoring",
    host = "10.0.0.27", user = "plorch", password = "aibMum!$",port=5432)
#     host = "localhost",user = "shiny",password = "GeshSa-8",port = 63333)
#    host = "10.0.0.27", user = "shiny", password = "GeshSa-8",port=5432)

# This method of importing data ignores geometry collumn and generates a warning:
#  Warning in postgresqlExecStatement(conn, statement, ...) :
#   RS-DBI driver warning: (unrecognized PostgreSQL field type geometry (id:85253) in column 13)
# Could use rgdal to import instead

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
comment_fields=names(nr_act_df)[grep("comment",names(nr_act_df))]
# This version will combine all comments
paste_noNA <- function(x,sep=", ") 
    gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) )
nr_act_df$comments_combined=apply(nr_act_df[comment_fields],1,paste_noNA,sep="; ")
# This version will find first comment only
#nr_act_df$comments_combined=do.call(coalesce,nr_act_df[comment_fields])

nr_act_df$report_text=apply(nr_act_df[c("activity","activities_performed_all","comments_combined")],1,paste_noNA,sep="; ")
nr_act_df$report_text2=apply(nr_act_df[c("fulcrum_user2","report_text")],1,paste_noNA,sep=": ")
nr_act_df$report_text3=paste("**",nr_act_df$fulcrum_user2,":** ",nr_act_df$report_text,sep="")

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

t_nr_act=nr_act_df[200:300,]
tlist=as.list(by(t_nr_act[c("fulcrum_user2","activity","activities_performed_all","comments_combined")],
                 t_nr_act[c("reservations2","dir_report_category")],identity))

# sink(file="t_nr_act.Rmd")
# cat("#' ---","\n",
#     "#' title: \"Crop Analysis Q3 2013\"","\n",
#     "#' author: \"John Smith\"","\n",
#     "#' date: \"May 3rd, 2014\"","\n",
#     "#' ---","\n","\n",sep="")
# for(i in unique(t_nr_act$dir_report_category)){
#     cat("##",i,"\n")
#     for(j in unique(t_nr_act$reservations2)) {
#         cat("###",j,"\n")
#         for(k in t_nr_act$report_text3[t_nr_act$dir_report_category==i & 
#                                        t_nr_act$reservations2==j]) {
#             cat(k,"\n")
#         }
#         cat("\n")
#     }
#     cat("\n")
# }
# sink()

# Example
# for (i in unique(df$cat1)) {
#     cat("##",i,"\n")
#     for (j in unique(df[df$cat1==i,"cat2"])) {
#         cat("###",j,"\n")
#         for (k in df$line[df$cat1==i & df$cat2==j])
#             cat(k,"\n")
#     }
# }


# Need to decide whether to do this here or move it to a postgis view or table

#session$onSessionEnded(function() { dbDisconnect(conn) })
pool::poolClose(pool)
