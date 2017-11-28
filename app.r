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
             "assignee_lastknown_state",
             "assignee_country"),
  all_pages = TRUE
)
# unnested original data frame
unnested_project_result = project_result$data$patents %>%
  unnest(inventors, .drop = FALSE) %>%
  unnest(assignees)
unnested_project_result[1:5, ]
#--------------------------------------------
#core objective #1

#Print summary in console
core1_df = unnested_project_result %>%  
  summarise("Total number of patents:" = n_distinct(patent_number),
            "Total number of inventors:" = n_distinct(inventor_id),
            "Total number of assignees:" = n_distinct(assignee_id))


summary_stats_dt = as.data.table(core1_df)
summary_stats_dt

#---------------------------------------
# Core objective #2  

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
head(table3, n = 5)

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
#-----------------------------------
#menu objective 2
inventor_df = unnested_project_result %>%
  group_by (inventor_id) %>%
  summarise(number_patents = n())


head(inventor_df)

inventor_df2 = unnested_project_result %>%
  select(inventor_id,
         inventor_last_name)
 

inventor_3 = inventor_df2 %>%
  left_join(inventor_df) %>%
  arrange(desc(number_patents))

unique(inventor_3)

inventor_joined =
  left_join(inventor_df2, inventor_df, by = "inventor_id") %>%
  arrange(desc(number_patents))
str(inventor_joined)  
head(unique(inventor_joined))
#-----------------------------------
# menu objective #3
menu3_df = unnested_project_result %>%
  group_by(assignee_country) %>%
  summarise(count = n())

menu3_df
colnames(menu3_df) <- c("Country", "Total")

menu3 = na.omit(menu3_df) %>%
  select(Country, Total) %>%
  #na.exclude(assignee_org) %>%
  arrange(desc(Total))

menu3
#menu3_result = menu3[1:5, ] 
#menu3_result
menu3_dt = as.data.table(menu3)
menu3_dt

#-----------------------------------
# menu objective #4
head(newcore3)
patents_over_10 = filter(newcore3, num_patents > 10 )
patents_over_10_dt = as.data.table(patents_over_10)
head(patents_over_10_dt)


#-----------------------------------
# shiny app
ui <- fluidPage(
  # Give the page a title
  titlePanel("CIS 4730 Group Project"),
  
#--------------------------------    
#tab #1 - Summary
    tabsetPanel(
    id = 'dataset',
    tabPanel("Summary", verbatimTextOutput("summary")),
    
#---------------------------------------------------    
#tab #2 - DataTable
    tabPanel("DataTable",
             
        selectInput("assignee_state", "Assignee State:",
        c("All", sort(unique(patents_dt$assignee_lastknown_state)))
        ),

      hr(),
      
        textInput("inventor", "Inventor's last name contains 
                (e.g., Zeng) Note: case sensitive"),
      
        hr(),
      
      dataTableOutput("mytable2")
      
    ),
#--------------------------------------    
# tab #3 - AnalyzeData
    tabPanel("AnalyzeData", 
      # Generate a row with a sidebar
      sidebarLayout(      
                 
      # Define the sidebar 
      sidebarPanel(
        # Input: Slider for barplot - number of top assignees by # of patents
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
                    hr(),
        
        # Input: Slider for countries are most interested in obtaining patents by assignee country
        sliderInput("total",
                    "Number of Countries interested in obtaining patents:",
                    value = 5,
                    min = 1,
                    max = 10),
                    hr(),
        
        # checkbox - show assignee org with more than 10 patents
        checkboxInput(inputId = "over_10_patents",
                      label = strong("Show assignee organizations
                      with more than 10 patents"),
                      value = FALSE),
                      hr()
                      ),

      mainPanel(
        plotOutput("patentsPlot"),
        dataTableOutput("show"),
        tableOutput("view"),
        tableOutput("country")
        
        )
      )
    )
  )
)


server <- function(input, output) {
#-----------------------------------
# summary tab  
  
   output$summary <- renderPrint(summary_stats_dt)  
  
#-----------------------------------
# data table tab  
  
  # output data table & filter data based on selections  
  output$mytable2 <- renderDataTable({
    mydata <- patents_dt
    state <- patents_dt$assignee_lastknown_state
    
    #drop down menu - filter data table by assignee 
    if (input$assignee_state != "All") {
    mydata <- mydata[state == input$assignee_state, ]
      }
    
    # text box filter by inventor
    if (input$inventor != "") {
      inventor <- input$inventor
      mydata <- mydata[mydata$inventor_last_name %like% inventor]
    }
    
    mydata
    })  
  
#---------------------------------  
# analyze data tab
  
  #output table - assignee organizations with more than 10 patents
  output$show <- renderDataTable({
    if (input$over_10_patents) {
      patents_over_10_dt
    }
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

  #output top inventors table
  output$view <- renderTable({
    head(unique(inventor_joined), n = input$n)
  })
  
  #render top county table
  output$country <- renderTable({
    head(menu3_dt, input$total)
  }, bordered = TRUE)  

}
shinyApp(ui = ui, server = server)

