library(shiny)
library(tidyverse)

icu_cohort <- readRDS("icu_cohort.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Summary of Variables in ICU Cohort data"),

    # Sidebar with a select input for variables
    sidebarLayout(
        sidebarPanel(
        selectInput("var", 
                    label = "Choose a variable to display",
                    choices = c("Ethnicity", "Language", "Insurance",
                                "Marital Status", "Gender",
                                "Age at Hospital Admission",
                                "Bicarbonate", "Chloride", "Creatinine",
                                "Potassium", "Sodium", "Hematocrit",
                                "White Blood Cell Count", "Calcium",
                                "Magnesium", "Glucose", "Heart Rate",
                                "Systolic Non-invasive Blood Pressure",
                                "Mean Non-invasive Blood Pressure",
                                "Respiratory Rate", 
                                "Body Temperature in Fahrenheit",
                                "First ICU Unit")) 
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("Plot"),
          tableOutput("Table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Plot <- renderPlot({
      cate <- switch(input$var, 
                     "Ethnicity" = icu_cohort$ethnicity,
                     "Language" = icu_cohort$language,
                     "Insurance" = icu_cohort$insurance,
                     "Marital Status" = icu_cohort$marital_status,
                     "Gender" = icu_cohort$gender,
                     "First ICU Unit" = icu_cohort$first_careunit)
      cont <- switch(input$var,
                     "Age at Hospital Admission" = icu_cohort$age_admission,
                     "Bicarbonate" = icu_cohort$bicarbonate,
                     "Chloride" = icu_cohort$chloride,
                     "Creatinine" = icu_cohort$creatinine,
                     "Potassium" = icu_cohort$potassium,
                     "Sodium" = icu_cohort$sodium,
                     "Hematocrit" = icu_cohort$hematocrit,
                     "White Blood Cell Count" = 
                       icu_cohort$white_blood_cell_count,
                     "Calcium" = icu_cohort$calcium,
                     "Magnesium" = icu_cohort$magnesium,
                     "Glucose" = icu_cohort$glucose,
                     "Heart Rate" = icu_cohort$heart_rate,
                     "Systolic Non-invasive Blood Pressure" = 
                       icu_cohort$systolic_non_invasive_blood_pressure,
                     "Mean Non-invasive Blood Pressure" = 
                       icu_cohort$mean_non_invasive_blood_pressure,
                     "Respiratory Rate" = icu_cohort$respiratory_rate,
                     "Body Temperature in Fahrenheit" = 
                       icu_cohort$body_temperature_in_Fahrenheit)
     categorical <- c("Ethnicity", "Language", "Insurance", 
                      "Marital Status", "Gender", "First ICU Unit")
      if (input$var %in% categorical) {
        ggplot(mapping = aes(x = cate)) +
          geom_bar() +
          labs(x = input$var) +
          theme_minimal()
      } else {
        ggplot(mapping = aes(y = cont)) +
          geom_boxplot() +
          labs(x = "", y = input$var) +
          scale_x_discrete(labels = NULL, breaks = NULL) +
          theme_minimal()
      }
    })
    summary_table <- reactive({
      cate <- switch(input$var,
                     "Ethnicity" = icu_cohort$ethnicity,
                     "Language" = icu_cohort$language,
                     "Insurance" = icu_cohort$insurance,
                     "Marital Status" = icu_cohort$marital_status,
                     "Gender" = icu_cohort$gender,
                     "First ICU Unit" = icu_cohort$first_careunit)
      cont <- switch(input$var,
                     "Age at Hospital Admission" = icu_cohort$age_admission,
                     "Bicarbonate" = icu_cohort$bicarbonate,
                     "Chloride" = icu_cohort$chloride,
                     "Creatinine" = icu_cohort$creatinine,
                     "Potassium" = icu_cohort$potassium,
                     "Sodium" = icu_cohort$sodium,
                     "Hematocrit" = icu_cohort$hematocrit,
                     "White Blood Cell Count" =
                       icu_cohort$white_blood_cell_count,
                     "Calcium" = icu_cohort$calcium,
                     "Magnesium" = icu_cohort$magnesium,
                     "Glucose" = icu_cohort$glucose,
                     "Heart Rate" = icu_cohort$heart_rate,
                     "Systolic Non-invasive Blood Pressure" =
                       icu_cohort$systolic_non_invasive_blood_pressure,
                     "Mean Non-invasive Blood Pressure" =
                       icu_cohort$mean_non_invasive_blood_pressure,
                     "Respiratory Rate" = icu_cohort$respiratory_rate,
                     "Body Temperature in Fahrenheit" =
                       icu_cohort$body_temperature_in_Fahrenheit)
      categorical <- c("Ethnicity", "Language", "Insurance",
                       "Marital Status", "Gender", "First ICU Unit")
      if (input$var %in% categorical) {
        icu_cohort %>%
          group_by({{cate}}) %>%
          summarise(n = n()) %>%
          mutate(Proportion = n / sum(n))
      } else {
        icu_cohort %>%
          summarise(n = n(),
                    mean = mean(cont, na.rm = TRUE),
                    standard_deviation = sd(cont, na.rm = TRUE),
                    min = min(cont, na.rm = TRUE),
                    Q1 = quantile(cont, 0.25, na.rm = TRUE),
                    median = median(cont, na.rm = TRUE),
                    Q3 = quantile(cont, 0.75, na.rm = TRUE),
                    max = max(cont, na.rm = TRUE))
      }
    })
    output$Table <- renderTable(summary_table())
}

      
# Run the application 
shinyApp(ui = ui, server = server)
