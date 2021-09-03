# Shiny app for manipulating NR activity data

This app is meant to help managers and supervisors view, search, filter, and download NR activity data that was entered in the NR Activity Tracking app in Fulcrum. Users  can then either just use the view to summarize for Directors reports or download for other tasks.

We are developing this with a mapview as well for another way to summarize the data.

Here is an example with both table and map views: [SuperZip] (https://shiny.rstudio.com/gallery/superzip-example.html)

Some of the code for this was derived from code for the above [SuperZip example] (https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example) in the R-shiny gallery.

Good reference for [formating reports] (https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/) 

Even better reference for [generating and downloading html reports] (https://shiny.rstudio.com/articles/generating-reports.html)

## To get data from fulcrum into database

* In fulcrum app, choose export
  * Choose PostGIS for file format
  * Once in a while, choose Include Photos to get a backup of them
    * This will fill up your downloads folder fast
  * Do not choose Include GPS Data or Changesets
  * Do include Full History
  * Hit Finish
  * Once finished zipping, click download button
* Move download somewhere or leave in Downloads
* Extract from zip file using windows
* Double click .sql file to open in DBeaver
  * Make sure NR Monitoring database and management schema are selected from dropdowns or insert code to set schema
  * Run this script
* Once changes are in database, run app in R project or on Rshiny server


## To do #- [] 
- [x] Add map view
  - [x] Decide what needs to be in the view
- [x] Try to use pool based on [this code] (https://github.com/bborgesr/useR2017/blob/master/app/app.R)
- [x] Break activities and locations into smaller number of categories
- [x] Use these for point colors
- [x] Get two types of popups working
- [x] Get Action to center on map
- [x] Allow filtering by all three without conditioning?
  - Can be done but activities has 79 unique options, so limiting by something first helps
  - This link has [some suggestions] (https://groups.google.com/forum/#!topic/shiny-discuss/Q1ZvnjDCzUM)
- [x] Make both map and large table depend on filters from sample table?
  - Large table does now
- [x] Table of what is in map view
  - [x] imbedded calculations
- [x] director's report by reservation
  - [x] replace NA for reservation with Other
  - [x] fix example to work on whole table
  - [x] render as word?
- [x] get filtered data reactive working
- [] Grants report
  - [x] Grant_id summary field
  - [] Need to add grant id drop-down to Fulcrum and possibly rework this part of report
- [x] Get polygons working
  - [x] Include them here in map
- [x] Do we want a table download?
  - Yes for some of tabs
- [x] Move some calculations and combined fields into view in database

### postgis_view branch

- [] Move plant summary fields to views
- [] Make filters work on plant table views
- [] Add column to filter view to take you to the polygon (blank if no polygon)

