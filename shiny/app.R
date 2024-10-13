

data <- read.csv("ds_salaries.csv") 
library(ggplot2)
library(dplyr)
library(scales)
library(shiny)

data_cleaned <- data %>%
  filter(employee_residence == "US") %>%
  mutate(job_title = case_when(
    grepl("Data Scientist", job_title, ignore.case = TRUE) ~ "Data Scientist",
    grepl("Data Engineer", job_title, ignore.case = TRUE) ~ "Data Engineer",
    grepl("Analyst", job_title, ignore.case = TRUE) ~ "Data Analyst",
    grepl("Machine Learning", job_title, ignore.case = TRUE) ~ "Machine Learning Engineer",
    grepl("Manager", job_title, ignore.case = TRUE) ~ "Data Science Manager",
    grepl("Director", job_title, ignore.case = TRUE) ~ "Data Science Manager",
    grepl("Architect", job_title, ignore.case = TRUE) ~ "Data Architect",
    TRUE ~ "Other"  
  ))

data_cleaned <- data_cleaned %>%
  mutate(
    experience_level = case_when(
      experience_level == "EN" ~ "Entry",
      experience_level == "MI" ~ "Mid",
      experience_level == "SE" ~ "Senior",
      experience_level == "EX" ~ "Executive",
      TRUE ~ as.character(experience_level)
    ),
    experience_level = factor(experience_level, levels = c("Entry", "Mid", "Senior", "Executive"))
  )


ui <- fluidPage(
  titlePanel("Data Science Salary Distribution Comparisons"),
  
  
  fluidRow(
    column(4, selectInput("jobTitle1", "Job Title 1:",
                          choices = unique(data_cleaned$job_title),
                          selected = unique(data_cleaned$job_title)[2])),
    column(4, selectInput("experienceLevel1", "Experience Level 1:",
                          choices = unique(data_cleaned$experience_level),
                          selected = unique(data_cleaned$experience_level)[1])),
    column(4, selectInput("companySize1", "Company Size 1:",
                          choices = unique(data_cleaned$company_size),
                          selected = unique(data_cleaned$company_size)[3])), 
    column(4, selectInput("jobTitle2", "Job Title 2:",
                          choices = unique(data_cleaned$job_title),
                          selected = unique(data_cleaned$job_title)[3])),
    column(4, selectInput("experienceLevel2", "Experience Level 2:",
                          choices = unique(data_cleaned$experience_level),
                          selected = unique(data_cleaned$experience_level)[1])),
    column(4, selectInput("companySize2", "Company Size 2:",
                          choices = unique(data_cleaned$company_size),
                          selected = unique(data_cleaned$company_size)[3])) 
  ),
  
  
  mainPanel(
    plotOutput("salaryDistPlot")
  )
)

server <- function(input, output) {
  output$salaryDistPlot <- renderPlot({
    
    filtered_data1 <- data_cleaned %>%
      filter(job_title == input$jobTitle1,
             experience_level == input$experienceLevel1,
             company_size == input$companySize1)
    
    
    filtered_data2 <- data_cleaned %>%
      filter(job_title == input$jobTitle2,
             experience_level == input$experienceLevel2,
             company_size == input$companySize2)
    
    
    ggplot() +
      geom_density(data = filtered_data1, aes(x = salary_in_usd, fill = paste(input$jobTitle1, input$experienceLevel1, input$companySize1)), alpha = 0.5) +
      geom_density(data = filtered_data2, aes(x = salary_in_usd, fill = paste(input$jobTitle2, input$experienceLevel2, input$companySize2)), alpha = 0.5) +
      scale_fill_manual(values = c("skyblue", "orange")) +
      labs(title = "Salary Distribution Comparison",
           x = "Salary in USD", y = "Density", fill = "Job Title") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
}


shinyApp(ui = ui, server = server)

