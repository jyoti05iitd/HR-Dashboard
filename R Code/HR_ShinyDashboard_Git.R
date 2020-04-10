

library(shiny)
library(shinydashboard)
library(tidyverse)


# Loadin data
file = "C:\\Users\\admin\\OneDrive\\2_DataScience\\R_ShinyProject\\HR_Project\\core_dataset.csv"

data <- data.table::fread(file, stringsAsFactors = FALSE)

data <- data %>% janitor::clean_names()

# Change the format of DOB column to Date
data$dob <- as.Date(data$dob, format = "%m/%d/%Y")

# Change the format of Date.of.Hire column to Date
data$date_of_hire <- as.Date(data$date_of_hire, format = "%m/%d/%Y")

# Change the format of Date.of.Termination column to Date
data$date_of_termination <- as.Date(data$date_of_termination, format = "%m/%d/%Y")


data <- data %>%
  select(employee_number, date_of_hire, date_of_termination, department)


header = dashboardHeader(title = "HR Dashboard")

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("EmployeeCount", tabName = "EmployeeCount", icon = icon("dashboard"))
    
   
  )
)

body = dashboardBody(
  ### Adding the CSS style
  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
  # Boxes need to be put in a row (or column)
  tabItems(
    # First tab content
    tabItem(tabName = "EmployeeCount",
            
            fluidRow(
              
              box(width = 6, background = "olive",
                  br(), " ",
                dateInput("start_date", "Select Start Date", value = "2015-01-01", 
                        min = "2006-01-01", max="2016-12-31")),
              
              box(width = 6, background = "olive",
                  br(), " ",
              dateInput("end_date", "Select End Date", value = "2015-12-31", 
                        min = "2006-01-01", max="2016-12-31"))
              
            ),
            
            
            fluidRow(
              valueBoxOutput("start_emp", width =3),
              valueBoxOutput("end_emp", width =3),
              valueBoxOutput("left_emp", width =3),
              valueBoxOutput("emp_joined", width =3)),
            
            fluidRow(
              
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Attrition percentage",
                plotOutput("barplot"))),
            
            fluidRow(
              
              box(
                width = 12, solidHeader = TRUE,
                title = "Attrition Table",
                DT::dataTableOutput("att_table"))
            )
    )
    
    
  )
)




ui <- dashboardPage(skin = "black",
  header = header,
  sidebar = sidebar,
  body = body)

server <- function(input, output) {
  
  
  data_selected <- reactive({
    data %>%
      mutate(
        # Employee working status at start date
        emp_status1 = ifelse(data$date_of_termination > (input$start_date)
                             | is.na(data$date_of_termination), TRUE, FALSE),
        # Employee count at start date
        emp_act1 = ifelse(data$date_of_hire <= (input$start_date) & emp_status1 == TRUE, 1, 0), 
        
        # Employee working status at end date
        emp_status2 = ifelse(data$date_of_termination > (input$end_date)
                             | is.na(data$date_of_termination), TRUE, FALSE),
        # Employee count at end date
        emp_act2 = ifelse(data$date_of_hire <= (input$end_date) & emp_status2 == TRUE, 1, 0),
        # Employee left in the period
        emp_left = ifelse(data$date_of_termination >= (input$start_date) & 
                            data$date_of_termination <= (input$end_date), 1, 0)) %>%
      group_by(department) %>%
      summarise(start_c = sum(emp_act1, na.rm = TRUE),
                end_c = sum(emp_act2, na.rm = TRUE),
                av_emp = mean(sum(emp_act1, na.rm = TRUE), sum(emp_act2, na.rm = TRUE)),
                emp_left = sum(emp_left, na.rm=TRUE),
                emp_joined = end_c + emp_left - start_c,
                att_perc = round(emp_left/av_emp*100, 1)) %>%
      filter(av_emp >0) %>%
      #filter(att_perc > 0) %>% 
      ungroup() %>% 
      arrange(desc(att_perc)) %>% 
      mutate(department = fct_reorder(department, att_perc, .desc = TRUE))
    
  })
  
  
  output$start_emp <- renderValueBox({
    valueBox(
      
      value = sum(data_selected()[, 2, drop = TRUE]),
      subtitle = "Active Employees at Start Date",
      icon = icon("users")
    )
  })
  
  output$end_emp <- renderValueBox({
    valueBox(
      
      value = sum(data_selected()[, 3, drop = TRUE]),
      subtitle = "Active Employees at End Date",
      icon = icon("user-check")
    )
  })
  
  output$left_emp <- renderValueBox({
    valueBox(
      
      value = sum(data_selected()[, 5, drop = TRUE]),
      subtitle = "Employees Left in the Period",
      icon = icon("user-times")
    )
  })
  
  output$emp_joined <- renderValueBox({
    valueBox(
      
      value = sum(data_selected()[, 6, drop = TRUE]),
      subtitle = "Employees Joined in the Period",
      icon = icon("user-check")
    )
  })
  
  output$barplot <- renderPlot({
    data.df2 <- data_selected()
    
    myRed <- "#99000D"
    myPink = "lightcyan"
    
    ggplot(data.df2, aes(data.df2[,1,drop=TRUE], att_perc)) +
      geom_col(fill="#009CE4") +
      #coord_flip() +
      theme(panel.background = element_blank(),
            legend.key = element_blank(),
            legend.background = element_blank(),
            strip.background = element_blank(),
            plot.background = element_rect(fill = myPink, color = "black", size = 3),
            panel.grid = element_blank(),
            axis.line = element_line(color = "red"),
            axis.ticks = element_line(color = "red"),
            strip.text = element_text(size = 16, color = myRed),
            axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
            axis.title.x = element_blank(),
            axis.text = element_text(color = "black", angle = 90),
            legend.position = "none")
    
      
    
  })
  
  output$att_table <- DT::renderDataTable({
    
    data_selected()
    
    
  })
}

shinyApp(ui, server)