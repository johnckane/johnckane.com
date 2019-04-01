#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

load("/home/john/johnckane.com/drafts/stackoverflow-2018-survey/developer_survey_2018/us_ds.Rda")
role_choices <- list("Back-end developer", 
                     "Full-stack developer", 
                     "Data or business analyst",
                     "Front-end developer",
                     "Database administrator")
language_choices <- list("Python", 
                     "SQL", 
                     "Bash/Shell",
                     "HTML",
                     "JavaScript",
                     "CSS",
                     "R",
                     "Java",
                     "C++",
                     "C#")


# Relabel education into new buckets.

us_ds <-
  us_ds %>%
  mutate(
    Education = factor(case_when(
      FormalEducation == "Bachelor’s degree (BA, BS, B.Eng., etc.)" ~ "Bachelor's",
      FormalEducation == "Associate degree" ~ "< Bachelor's",
      FormalEducation == "I never completed any formal education" ~ "< Bachelor's",
      FormalEducation == "Master’s degree (MA, MS, M.Eng., MBA, etc.)" ~ "Master's",
      FormalEducation == "Professional degree (JD, MD, etc.)" ~ "Doctoral/Profesional",
      FormalEducation == "Other doctoral degree (Ph.D, Ed.D., etc.)" ~ "Doctoral/Profesional",
      FormalEducation == "Primary/elementary school" ~ "< Bachelor's",
      FormalEducation == "Some college/university study without earning a degree" ~ "< Bachelor's",
      FormalEducation == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)" ~ "< Bachelor's"
      ),
      levels = c(
        "< Bachelor's",
        "Bachelor's",
        "Master's",
        "Doctoral/Profesional")),
    ProfCodingExp = factor(case_when(
      YearsCodingProf == '0-2 years' ~ "0-2",
      YearsCodingProf == '3-5 years' ~ "3-5",
      YearsCodingProf == '6-8 years' ~ "6-8",
      YearsCodingProf == '9-11 years' ~ "9-14",
      YearsCodingProf == '12-14 years' ~ "9-14",
      YearsCodingProf == '15-17 years' ~ "15-20",
      YearsCodingProf == '18-20 years' ~ "15-20",
      TRUE ~ "20+"),
      levels = c(
        "0-2",
        "3-5",
        "6-8",
        "9-14",
        "15-20",
        "20+")))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("United States Data Science Salaries"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("education", 
                    label = h3("Education"), 
                    choices = setNames(as.character(unique(us_ds$Education)),unique(us_ds$Education)), 
                    selected = NULL),
        selectInput("coding", 
                    label = h3("Years Coding Profesionally"), 
                    choices = setNames(as.character(unique(us_ds$ProfCodingExp)),unique(us_ds$ProfCodingExp)), 
                    selected = NULL),
        checkboxGroupInput("other_roles", label = h3("Other Roles"), 
                           choices = role_choices,
                           selected = NULL),
        checkboxGroupInput("languages", label = h3("Coding Languages"),
                           choices = language_choices,
                           selected = NULL),
        actionButton("go", "Go")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         dataTableOutput("data")
      )
   )
)

server <- function(input, output) {
  
 

   #roles_chosen   <- eventReactive(input$go,{paste(input$other_roles,sep=" ",collapse=" ")})
   roles_chosen   <- eventReactive(input$go,{input$other_roles})
   roles_unchosen <- eventReactive(input$go,{unlist(role_choices[-which(role_choices %in% input$other_roles)])})
   
   #languages_chosen   <- eventReactive(input$go,{paste(input$languages,sep=" ",collapse=" ")})
   languages_chosen   <- eventReactive(input$go,{input$languages})
   languages_unchosen <- eventReactive(input$go,{unlist(language_choices[-which(language_choices %in% languages_chosen())])})
  

     plot_data <- eventReactive(input$go,
    {us_ds %>%
       filter(is.na(ConvertedSalary) == FALSE,
              #FormalEducation == input$education,
              #YearsCodingProf == input$coding,
              grepl(paste0("^(?=.*",paste(languages_chosen(), sep = "",collapse = ")(?=.*"),").*$"), LanguageWorkedWith, perl = TRUE),
              #!grepl(paste0("(",paste(languages_unchosen(),sep="",collapse=")|("),")"),LanguageWorkedWith, perl = TRUE),
              grepl(paste0("^(?=.*",paste(roles_chosen(), sep = "",collapse = ")(?=.*"),").*$"), DevType, perl = TRUE)#,
              #!grepl(paste0("(",paste(roles_unchosen(),sep="",collapse=")|("),")"),DevType, perl = TRUE)
              )
   })
  
   output$distPlot <- renderPlot({
    print(dim(plot_data()))
     ggplot(data = plot_data(), aes(x = ConvertedSalary)) +
      #  geom_dotplot(color = 'darkgreen',method = "histodot") +
      geom_dotplot() +
        NULL
  })
  
  output$data <- renderDataTable({
    plot_data() %>%
      group_by(Education,ProfCodingExp) %>%
      summarise(salary = mean(ConvertedSalary),
                sample = n())
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

