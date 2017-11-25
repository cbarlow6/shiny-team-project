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
# original dataframe
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
  summarise("Total number of patents" = n_distinct(patent_number),
            "Total number of inventors" = n_distinct(inventor_id),
            "Total number of assignees" = n_distinct(assignee_id))


summary_stats_dt = as.data.table(core1_df)
summary_stats_dt

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
newtable = table(unnested_project_result$assignee_organization, 
                 exclude = NA)
table3 = sort(newtable, decreasing = TRUE)
table4 = head(table3, n = 5)

# horizontal bar plot
par(mar = c(5,9,4,2))
assignees_plot2 = barplot(table4,
                  xlab = "Number of Patents", horiz = TRUE,
                  main = "Top Assignee Organizations",
                  xlim = c(0, 8000),
                  cex.names = .40,
                  las = 2,
                  col = "blue")

# vertical bar plot
par(mar=c(9,4,2,2))
assignees_plot = barplot(table4,
                         ylab = "Number of Patents",
                         main = "Top Assignee Organizations",
                         ylim = c(0, 8000),
                         cex.names = .40,
                         las = 2,
                         col = "blue"
                         
                         )
                        

#------------------------------------
#core objective 4 - drop down menu state of assignee organization
#-----------------------------------
#core objective 5 - text box query investor's last name
Inventor = patents_dt$inventor_last_name
#-----------------------------------
#menu objective 2
inventor_df = unnested_project_result %>%
  group_by (inventor_id) %>%
  summarise(number_patents = n())


head(inventor_df)

inventor_df2 = unnested_project_result %>%
  select(inventor_id,
         inventor_last_name)
 

#inventor_df3 = inventor_df %>%
  #inner_join(inventor_df2)

inventor_3 = inventor_df2 %>%
  left_join(inventor_df) %>%
  arrange(desc(number_patents))

unique(inventor_3)

inventor_joined =
  left_join(inventor_df2, inventor_df, by = "inventor_id") %>%
  arrange(desc(number_patents))
  
head(unique(inventor_joined))
#-----------------------------------
head(newcore3)
head(filter(newcore3, num_patents > 10 ))


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
    tabPanel("DataTable",
      column(4, 
        selectInput("assignee_state", "Assignee State:",
        c("All", sort(unique(patents_dt$assignee_lastknown_state)))
        )
      ),
      hr(),
      
      textInput("inventor", "Inventor's last name contains 
                (e.g., Zeng) Note: case sensitive"),
      hr(),
      dataTableOutput("mytable2")
    ),
    tabPanel("AnalyzeData", 
      # Generate a row with a sidebar
      sidebarLayout(      
                 
      # Define the sidebar 
      sidebarPanel(
        # Input: Slider for the number of top assignees by # of patents
        sliderInput("number",
                    "Number of top assignees requested:",
                    value = 5,
                    min = 1,
                    max = 10),
                   hr(),
                   #helpText("Top Assignee Organizations")
        
        # Input: Slider for the number of top inventors by # of patents
        sliderInput("n",
                    "Number of top inventors requested:",
                    value = 5,
                    min = 1,
                    max = 10),
        hr()
        #helpText("Top Assignee Organizations")
      ),

      mainPanel(
        plotOutput("patentsPlot"),
        tableOutput("view")
        )
        
      )
    )
      
    )
  #)
 #) 
)


server <- function(input, output) {
  
  
  # choose columns to display
  #summary_stats2 = summary_stats[sample(nrow(summary_stats), 1000), ]
  # Generate a summary of the dataset ----
  output$summary <- renderPrint(summary_stats_dt)  
  #output$summary <- renderDataTable(summary_stats_dt, 
                                   # options = list(orderClasses = TRUE))
  
  # output data table & filter data based on selections  
  output$mytable2 <- renderDataTable({
    mydata <- patents_dt
    
    #drop down menu - filter data table by assignee 
    if (input$assignee_state != "All") {
     mydata <- mydata[mydata$assignee_lastknown_state == input$assignee_state, ]
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
    plot_table <- head(table3, input$number)
    # Render a barplot
      barplot(plot_table, 
              ylab = "Number of Patents",
              main = "Top Assignee Organizations",
              ylim = c(0, 8000),
              cex.names = .35,
              col = "blue")
      
      })

  #render top inventor table
  output$view <- renderTable({
    head(unique(inventor_joined), n = input$n)
  })
  
  
}
shinyApp(ui = ui, server = server)

