install.packages("patentsview")
install.packages("tidyverse")
install.packages("shiny")
install.packages("data.table")
library(patentsview)
library(tidyverse)
library(shiny)
library(data.table)


# query of patent database
project_query <- qry_funs$and(
  qry_funs$gte(patent_date = "2016-01-01"),
  qry_funs$lte(patent_date = "2016-03-31")
)
#original dataframe
project_result = search_pv(
  query = project_query, 
  fields = c("patent_number", 
             "patent_date", 
             "inventor_id",
             "inventor_last_name",
             "inventor_lastknown_city",
             "inventor_lastknown_state",
             "assignee_id",
             "assignee_organization",
             "assignee_lastknown_state"),
  all_pages = TRUE
)
# unnested original data frame
unnested_project_result = project_result$data$patents %>%
  unnest(inventors, .drop = FALSE) %>%
  unnest(assignees)
unnested_project_result[1:5, ]
#--------------------------------------------
#core objective #1
#result1 = search_pv(
  #query = project_query, 
  #fields = c("patent_id", "inventor_id", "assignee_id")
  #, all_pages = TRUE
#)
#result1

#core1 = result1$data$patents %>% 
 # unnest(inventors, .drop=FALSE) %>% 
  #unnest(assignees)

#Print summary in console
core1_df = unnested_project_result %>%  
  summarise(patents = n_distinct(patent_number), 
            inventors = n_distinct(inventor_id), 
            assignees = n_distinct(assignee_id))

summary(core1_df)
summary_stats_dt = as.data.table(core1_df)
str(summary_stats_dt)
#---------------------------------------
# Core objective #2  
#core2_df= project_result$data$patent %>% 
  #unnest(inventors, .drop=FALSE) %>%
  #unnest(assignees)

core2_df = unnested_project_result%>%
  select(patent_number, 
         patent_date, 
         inventor_last_name, 
         inventor_lastknown_city, 
         assignee_organization,
         assignee_lastknown_state)

patents_dt = as.data.table(core2_df)
head(patents_dt)
#patents_table[1:5, ]
#str(patents_table)
#---------------------------------------
# core objective #3 - print top 5 assignees
core3_df = core2_df %>%
  group_by(assignee_organization) %>%
  summarise(count = n())

head(core3_df)
colnames(core3_df) <- c("assignee_org", "num_patents")

newcore3 = core3_df %>%
  select(assignee_org, num_patents) %>%
  na.exclude(assignee_org) %>%
  arrange(desc(num_patents))

result = newcore3[1:5, ] 
result

# core objective #3 - bar plot 5 top assignees
newtable = table(core2_df$assignee_organization, exclude = NA)
table3 = sort(newtable, decreasing = TRUE)
table4 = head(table3, n = 5)
assignees_plot = barplot(table4, xlab = "Assignee Organizations", 
                  ylab = "Number of Patents",
                  main = "Top Assignee Organizations",
                  ylim = c(0, 8000),
                  cex.names = .45,
                  col = "blue")
str(assignees_plot)

#------------------------------------
#core objective 4
core4_df = core2_df
#-----------------------------------
#core objective 5
Inventor = patents_dt$inventor_last_name

#-----------------------------------
# shiny app
ui <- fluidPage(
  # Give the page a title
  titlePanel("CIS 4730 Group Project"),
  #hr(),
  
  # Generate a row with a sidebar
  #sidebarLayout(
  #,
  
    # Define the sidebar with one input
    #sidebarPanel(
     # textInput("inventor", "Inventor's last name contains (e.g., Zeng)")
      #),
   
    # Create a spot for the barplot
    #mainPanel(
      tabsetPanel(
      id = 'dataset',
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Data Table",
              column(4, 
                     selectInput("assignee", "Assignee:",
                                c("All", sort(unique(patents_dt$assignee_organization)))
                     )
              ),
              hr(),
              textInput("inventor", "Inventor's last name contains (e.g., Zeng) Note: case sensitive"),
              hr(),
              dataTableOutput("mytable2")
              ),
      tabPanel("Bar Plot", plotOutput("patentsPlot"))
   )
  #)
 #) 
)


server <- function(input, output) {
  
  
  # choose columns to display
  #summary_stats2 = summary_stats[sample(nrow(summary_stats), 1000), ]
  # Generate a summary of the dataset ----
  output$summary <- renderPrint(summary(core1_df))  
  #output$summary <- renderDataTable(summary_stats_dt, 
                                   # options = list(orderClasses = TRUE))
  
  # output data table & filter data based on selections  
  output$mytable2 <- renderDataTable({
    mydata <- patents_dt
    
    #drop down menu - filter data table by assignee 
    if (input$assignee != "All") {
     mydata <- mydata[mydata$assignee_organization == input$assignee, ]
    }
    # text box filter by inventor
    if (!is.null(input$inventor) && input$inventor != "") {
      inventor <- input$inventor
      mydata <- mydata[Inventor %like% inventor, ] 
      #%>% filter(Inventor %like% inventor)
    }
    #options = list(orderClasses = TRUE)
    mydata
    })  
  
  #output bar plot  
  output$patentsPlot <- renderPlot({
      # Render a barplot
      barplot(table4, xlab = "Assignee Organizations", 
              ylab = "Number of Patents",
              main = "Top Assignee Organizations",
              ylim = c(0, 8000),
              cex.names = .45,
              col = "blue"
              )
      })

}
shinyApp(ui = ui, server = server)

