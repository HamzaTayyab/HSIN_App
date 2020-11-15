###################SERVER######################
library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(dplyr)
library(DT)


# ui=shinyUI(dashboardPage(
#   dashboardHeader(title = "TEST TOBE"),
#   
#   #dashboardSidebar(width = 300),
#   
#   dashboardSidebar(width = 1
#                    
#                    #sidebarMenu(
#                    # selectInput("Model", "Choose  model:",c("Random forest"))
#                    #)
#                    
#   ),
#   dashboardBody(
#     
#     tabsetPanel(
#       tabPanel("Table",
#                
#                column(12,box(height =400,width = 800, solidHeader = FALSE, status = "success",
#                              DT::dataTableOutput("table1")))
#                
#       )))))

cluster_bar_names <- c("product_date",
                       "fc_sin",
                       "sar",
                       "pii",
                       "foreign_release",
                       "private_release",
                       "p_crcl",
                       "priority",
                       "location_name",
                       "author_name",
                       "hsec_sin",
                       "sbu")

ui <- navbarPage("HSIN NavBar Prototype",
                 tabPanel("Search",
                          column(6,
                                 wellPanel(
                                   textInput("searchterms", "Enter search terms here"),
                                   #Filter selection
                                   column(6,
                                          selectInput("filter_fc_sin", "FC Sin",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_SAR", "Suspicious Activity Reporting",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_PII", "Contains PII",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_foreign", "Releasable to Foreign persons",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_private", "Releaseable to Private Sector",
                                                      choices = c("Select One", "TRUE", "FALSE"))
                                   ),
                                   column(6,
                                          dateRangeInput("filter_date_range", "Report publication date",
                                                         start = "1900-01-01", end = Sys.Date()),
                                          selectInput("filter_hsec_sin", "HSEC SIN", multiple = TRUE,
                                                      choices = c("CYBER ATTACKS AND EXPLOITATION (HSEC-1)",
                                                                  "DISASTERS (HSEC-2)",
                                                                  "ILLICIT ALIEN OPERATIONS (HSEC-3)",
                                                                  "ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                                  "TRANSNATIONAL ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                                  "ILLICIT DRUG OPS (HSEC-5)",
                                                                  "TRANSNATIONAL ILLICIT DRUG OPS (HSEC-5)",
                                                                  "PUBLIC HEALTH HAZARDS (HSEC-6)",
                                                                  "STATE-SPONSORED OPERATIONS (HSEC-7)",
                                                                  "FOREIGN NATION OPS (HSEC-7)",
                                                                  "TERRORIST OPS (HSEC-8)",
                                                                  "TRANSNATIONAL ORGANIZED CRIMES (HSEC-9)",
                                                                  "TRANSNATIONAL VIOLENT CRIMES (HSEC-9)",
                                                                  "WEAPONS PROLIFERATION/ILLICIT OPERATIONS (HSEC-10)",
                                                                  "Not Applicable")
                                          ),
                                          selectInput("filter_location", "Report Location", multiple = TRUE,
                                                      choices = c("Alabama",
                                                                  "Alaska",
                                                                  "Arizona",
                                                                  "Arkansas",
                                                                  "California",
                                                                  "Colorado",
                                                                  "Connecticut",
                                                                  "Delaware",
                                                                  "District of Columbia",
                                                                  "Florida",
                                                                  "Georgia",
                                                                  "Hawaii",
                                                                  "Idaho",
                                                                  "Illinois",
                                                                  "Indiana",
                                                                  "Iowa",
                                                                  "Kansas",
                                                                  "Kentucky",
                                                                  "Louisiana",
                                                                  "Maine",
                                                                  "Maryland",
                                                                  "Massachusetts",
                                                                  "Michigan",
                                                                  "Minnesota",
                                                                  "Mississippi",
                                                                  "Missouri",
                                                                  "Montana",
                                                                  "Nebraska",
                                                                  "Nevada",
                                                                  "New Hampshire",
                                                                  "New Jersey",
                                                                  "New Mexico",
                                                                  "New York",
                                                                  "North Carolina",
                                                                  "North Dakota",
                                                                  "Ohio",
                                                                  "Oklahoma",
                                                                  "Oregon",
                                                                  "Pennsylvania",
                                                                  "Puerto Rico",
                                                                  "Rhode Island",
                                                                  "South Carolina",
                                                                  "South Dakota",
                                                                  "Tennessee",
                                                                  "Texas",
                                                                  "Utah",
                                                                  "Vermont",
                                                                  "Virgin Islands",
                                                                  "Virginia",
                                                                  "Washington",
                                                                  "West Virginia",
                                                                  "Wisconsin",
                                                                  "Wyoming")
                                          ),
                                          selectInput("filter_priorities", "Priority", multiple = TRUE,
                                                      choices = c("Aviation Security",
                                                                  "Aviation/Surface Transportation Security",
                                                                  "Border Security",
                                                                  "Border Security/Transnational Organized Crime",
                                                                  "Countering Violent Extremism",
                                                                  "Counterintelligence",
                                                                  "Counterterrorism",
                                                                  "Cyber Security",
                                                                  "Mass Violence",
                                                                  "Trade and Economic Security",
                                                                  "Not Applicable")
                                          ),
                                          selectInput("filter_sbu", "SBU Classification", multiple = TRUE,
                                                      choices = c("CII",
                                                                  "FOUO",
                                                                  "LES",
                                                                  "Open Source",
                                                                  "PCII",
                                                                  "SSI",
                                                                  "Unclassified",
                                                                  "Not Applicable")
                                          )
                                          
                                   ),
                                   #Button to submit search terms
                                   actionButton("submit_search", "Submit"))),
                          column(6,
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Time series", plotOutput("distPlot")),
                                             tabPanel("Compliance", plotOutput("stackbarPlot")),
                                             tabPanel("Map", plotOutput("mapPlot")),
                                             tabPanel("Cluster Bar", plotOutput("clusterbarPlot"),
                                                      column(4, selectInput("cluster_x",
                                                                            "x-axis",
                                                                            cluster_bar_names)),
                                                      column(4,selectInput("cluster_y",
                                                                           "y-axis",
                                                                           cluster_bar_names)),
                                                      column(4,selectInput("cluster_fill",
                                                                           "Subgroup",
                                                                           cluster_bar_names)))
                                 )
                          ),
                          fluidRow(
                            column(12,
                                   div(style= "overflow-x: scroll", dataTableOutput("hsin_table")),
                                   downloadButton("export_hsin_table", "Export Data")
                            )
                          )
                 ),
                 tabPanel("Comparative Search",
                          column(6,
                                 wellPanel(
                                   textInput("comp_search_a", "Enter search terms here"),
                                   actionButton("submit_comp_search_a", "Submit"),
                                   column(6,
                                          
                                          selectInput("filter_fc_sin_comp_a", "FC Sin",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_SAR_comp_a", "Suspicious Activity Reporting",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_PII_comp_a", "Contains PII",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_foreign_comp_a", "Releasable to Foreign persons",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_private_comp_a", "Releaseable to Private Sector",
                                                      choices = c("Select One", "TRUE", "FALSE"))
                                   ),
                                   column(6,
                                          dateRangeInput("filter_date_range_comp_a", "Report publication date"),
                                          selectInput("filter_hsec_sin_comp_a", "HSEC SIN", multiple = TRUE,
                                                      choices = c("CYBER ATTACKS AND EXPLOITATION (HSEC-1)",
                                                                  "DISASTERS (HSEC-2)",
                                                                  "ILLICIT ALIEN OPERATIONS (HSEC-3)",
                                                                  "ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                                  "TRANSNATIONAL ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                                  "ILLICIT DRUG OPS (HSEC-5)",
                                                                  "TRANSNATIONAL ILLICIT DRUG OPS (HSEC-5)",
                                                                  "PUBLIC HEALTH HAZARDS (HSEC-6)",
                                                                  "STATE-SPONSORED OPERATIONS (HSEC-7)",
                                                                  "FOREIGN NATION OPS (HSEC-7)",
                                                                  "TERRORIST OPS (HSEC-8)",
                                                                  "TRANSNATIONAL ORGANIZED CRIMES (HSEC-9)",
                                                                  "TRANSNATIONAL VIOLENT CRIMES (HSEC-9)",
                                                                  "WEAPONS PROLIFERATION/ILLICIT OPERATIONS (HSEC-10)",
                                                                  "Not Applicable")
                                          ),
                                          selectInput("filter_location_comp_a", "Report Location", multiple = TRUE,
                                                      choices = c("Alabama",
                                                                  "Alaska",
                                                                  "Arizona",
                                                                  "Arkansas",
                                                                  "California",
                                                                  "Colorado",
                                                                  "Connecticut",
                                                                  "Delaware",
                                                                  "District of Columbia",
                                                                  "Florida",
                                                                  "Georgia",
                                                                  "Hawaii",
                                                                  "Idaho",
                                                                  "Illinois",
                                                                  "Indiana",
                                                                  "Iowa",
                                                                  "Kansas",
                                                                  "Kentucky",
                                                                  "Louisiana",
                                                                  "Maine",
                                                                  "Maryland",
                                                                  "Massachusetts",
                                                                  "Michigan",
                                                                  "Minnesota",
                                                                  "Mississippi",
                                                                  "Missouri",
                                                                  "Montana",
                                                                  "Nebraska",
                                                                  "Nevada",
                                                                  "New Hampshire",
                                                                  "New Jersey",
                                                                  "New Mexico",
                                                                  "New York",
                                                                  "North Carolina",
                                                                  "North Dakota",
                                                                  "Ohio",
                                                                  "Oklahoma",
                                                                  "Oregon",
                                                                  "Pennsylvania",
                                                                  "Puerto Rico",
                                                                  "Rhode Island",
                                                                  "South Carolina",
                                                                  "South Dakota",
                                                                  "Tennessee",
                                                                  "Texas",
                                                                  "Utah",
                                                                  "Vermont",
                                                                  "Virgin Islands",
                                                                  "Virginia",
                                                                  "Washington",
                                                                  "West Virginia",
                                                                  "Wisconsin",
                                                                  "Wyoming")
                                          ),
                                          selectInput("filter_priorities_comp_a", "Priority", multiple = TRUE,
                                                      choices = c("Aviation Security",
                                                                  "Aviation/Surface Transportation Security",
                                                                  "Border Security",
                                                                  "Border Security/Transnational Organized Crime",
                                                                  "Countering Violent Extremism",
                                                                  "Counterintelligence",
                                                                  "Counterterrorism",
                                                                  "Cyber Security",
                                                                  "Mass Violence",
                                                                  "Trade and Economic Security",
                                                                  "Not Applicable")
                                          ),
                                          selectInput("filter_sbu_comp_a", "SBU Classification", multiple = TRUE,
                                                      choices = c("CII",
                                                                  "FOUO",
                                                                  "LES",
                                                                  "Open Source",
                                                                  "PCII",
                                                                  "SSI",
                                                                  "Unclassified",
                                                                  "Not Applicable")
                                          )
                                          
                                   ),
                                   br(),
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Time series", plotOutput("comp_dist_plot_a")),
                                               tabPanel("Compliance", plotOutput("comp_stackbar_plot_a")),
                                               tabPanel("Map", plotOutput("comp_map_plot_a")),
                                               tabPanel("Cluster Bar", plotOutput("comp_clusterbar_plot_a"),
                                                        column(4, selectInput("comp_cluster_x_a",
                                                                              "x-axis",
                                                                              cluster_bar_names)),
                                                        column(4,selectInput("comp_cluster_y_a",
                                                                             "y-axis",
                                                                             cluster_bar_names)),
                                                        column(4,selectInput("comp_cluster_fill_a",
                                                                             "Subgroup",
                                                                             cluster_bar_names)))
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(style= "overflow-x: scroll", dataTableOutput("comp_hsin_table_a")),
                                          downloadButton("export_comp_hsin_table_a", "Export Data")
                                   )
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   textInput("comp_search_b", "Enter search terms here"),
                                   actionButton("submit_comp_search_b", "Submit"),
                                   column(6,
                                          selectInput("filter_fc_sin_comp_b", "FC Sin",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_SAR_comp_b", "Suspicious Activity Reporting",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_PII_comp_b", "Contains PII",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_foreign_comp_b", "Releasable to Foreign persons",
                                                      choices = c("Select One", "TRUE", "FALSE")),
                                          selectInput("filter_private_comp_b", "Releasable to Private Sector",
                                                      choices = c("Select One", "TRUE", "FALSE"))
                                   ),
                                   column(6,
                                          dateRangeInput("filter_date_range_comp_b", "Report publication date"),
                                          selectInput("filter_hsec_sin_comp_b", "HSEC SIN", multiple = TRUE,
                                                      choices = c("CYBER ATTACKS AND EXPLOITATION (HSEC-1)",
                                                                  "DISASTERS (HSEC-2)",
                                                                  "ILLICIT ALIEN OPERATIONS (HSEC-3)",
                                                                  "ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                                  "TRANSNATIONAL ILLICIT COMMERCIAL OPS (HSEC-4)",
                                                                  "ILLICIT DRUG OPS (HSEC-5)",
                                                                  "TRANSNATIONAL ILLICIT DRUG OPS (HSEC-5)",
                                                                  "PUBLIC HEALTH HAZARDS (HSEC-6)",
                                                                  "STATE-SPONSORED OPERATIONS (HSEC-7)",
                                                                  "FOREIGN NATION OPS (HSEC-7)",
                                                                  "TERRORIST OPS (HSEC-8)",
                                                                  "TRANSNATIONAL ORGANIZED CRIMES (HSEC-9)",
                                                                  "TRANSNATIONAL VIOLENT CRIMES (HSEC-9)",
                                                                  "WEAPONS PROLIFERATION/ILLICIT OPERATIONS (HSEC-10)",
                                                                  "Not Applicable")
                                          ),
                                          selectInput("filter_location_comp_b", "Report Location", multiple = TRUE,
                                                      choices = c("Alabama",
                                                                  "Alaska",
                                                                  "Arizona",
                                                                  "Arkansas",
                                                                  "California",
                                                                  "Colorado",
                                                                  "Connecticut",
                                                                  "Delaware",
                                                                  "District of Columbia",
                                                                  "Florida",
                                                                  "Georgia",
                                                                  "Hawaii",
                                                                  "Idaho",
                                                                  "Illinois",
                                                                  "Indiana",
                                                                  "Iowa",
                                                                  "Kansas",
                                                                  "Kentucky",
                                                                  "Louisiana",
                                                                  "Maine",
                                                                  "Maryland",
                                                                  "Massachusetts",
                                                                  "Michigan",
                                                                  "Minnesota",
                                                                  "Mississippi",
                                                                  "Missouri",
                                                                  "Montana",
                                                                  "Nebraska",
                                                                  "Nevada",
                                                                  "New Hampshire",
                                                                  "New Jersey",
                                                                  "New Mexico",
                                                                  "New York",
                                                                  "North Carolina",
                                                                  "North Dakota",
                                                                  "Ohio",
                                                                  "Oklahoma",
                                                                  "Oregon",
                                                                  "Pennsylvania",
                                                                  "Puerto Rico",
                                                                  "Rhode Island",
                                                                  "South Carolina",
                                                                  "South Dakota",
                                                                  "Tennessee",
                                                                  "Texas",
                                                                  "Utah",
                                                                  "Vermont",
                                                                  "Virgin Islands",
                                                                  "Virginia",
                                                                  "Washington",
                                                                  "West Virginia",
                                                                  "Wisconsin",
                                                                  "Wyoming")
                                          ),
                                          selectInput("filter_priorities_comp_b", "Priority", multiple = TRUE,
                                                      choices = c("Aviation Security",
                                                                  "Aviation/Surface Transportation Security",
                                                                  "Border Security",
                                                                  "Border Security/Transnational Organized Crime",
                                                                  "Countering Violent Extremism",
                                                                  "Counterintelligence",
                                                                  "Counterterrorism",
                                                                  "Cyber Security",
                                                                  "Mass Violence",
                                                                  "Trade and Economic Security",
                                                                  "Not Applicable")
                                          ),
                                          selectInput("filter_sbu_comp_b", "SBU Classification", multiple = TRUE,
                                                      choices = c("CII",
                                                                  "FOUO",
                                                                  "LES",
                                                                  "Open Source",
                                                                  "PCII",
                                                                  "SSI",
                                                                  "Unclassified",
                                                                  "Not Applicable")
                                          )
                                          
                                   ),
                                   br(),
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Time series", plotOutput("comp_dist_plot_b")),
                                               tabPanel("Compliance", plotOutput("comp_stackbar_plot_b")),
                                               tabPanel("Map", plotOutput("comp_map_plot_b")),
                                               tabPanel("Cluster Bar", plotOutput("comp_clusterbar_plot_b"),
                                                        column(4, selectInput("comp_cluster_x_b",
                                                                              "x-axis",
                                                                              cluster_bar_names)),
                                                        column(4,selectInput("comp_cluster_y_b",
                                                                             "y-axis",
                                                                             cluster_bar_names)),
                                                        column(4,selectInput("comp_cluster_fill_b",
                                                                             "Subgroup",
                                                                             cluster_bar_names)))
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(style= "overflow-x: scroll", dataTableOutput("comp_hsin_table_b")),
                                          downloadButton("export_comp_hsin_table_b", "Export Data")
                                   )
                                 )
                          )
                 )
)

server <- function(input, output, session){
  
  db <- 'postgres' 
  host_db <- "database-2.c0gfscutioly.us-east-2.rds.amazonaws.com"
  db_port <- 5432   # or any other port specified by the DBA
  db_user <-  'postgres'
  db_password <- 'lalaina30'
  drv <- dbDriver("PostgreSQL")
  #drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  default_query <- "SELECT * FROM load_from_s3.locations"
  
  output$table1<-DT::renderDataTable({
    map_data = dbGetQuery(con, default_query)%>%
      group_by(location_name) %>%
      summarize(topic_location = n()) %>%
      rename(state = location_name) %>%
      filter(state != "" & state != "All") %>%
      mutate(state = tolower(state))
    map_data
  })
}

shinyApp(ui = ui, server = server)
