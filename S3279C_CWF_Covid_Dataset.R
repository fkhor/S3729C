knitr::opts_chunk$set(echo = TRUE)

# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)
library(DT)

# Load data --------------------------------------------------------------------

covid <- read.csv(file = "https://raw.githubusercontent.com/fkhor/S3729C/main/final_dataset_covid.csv", header = TRUE, sep = ",")


# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                navbarPage(title = "S3729C CWF COVID Dataset:",
                           
                           tabPanel("COVID and Diet",
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        
                                        br(), br(),
                                        
                                        selectInput(
                                          inputId = "y_diet",
                                          label = "Y-axis:",
                                          choices = c(
                                            "Total COVID cases" = "total_cases",
                                            "Total COVID cases per million" = "total_cases_per_million",
                                            "Total Deaths" = "total_deaths"
                                          ),
                                          selected = "total_cases_per_million"
                                        ),
                                        
                                        selectInput(
                                          inputId = "x_diet",
                                          label = "X-axis:",
                                          choices = c(
                                            "Alchohol" = "alcoholic_beverages",
                                            "Animal fats" = "animal_fats",
                                            "Animal products" = "animal_products",
                                            "Aquatic products" = "aquatic_products_other",
                                            "Cereals" = "cereals_excluding_beer",
                                            "Eggs" = "eggs",
                                            "Fish and Seafood" = "fish_seafood",
                                            "Fruits" = "fruits_excluding_wine",
                                            "Meat" = "meat",
                                            "Milk" = "milk_excluding_butter",
                                            "Offals" = "offals",
                                            "Starchy roots" = "starchy_roots",
                                            "Sugar (Sweeteners)" = "sugar_sweeteners",
                                            "Sugar (Crops)" = "sugar_crops",
                                            "Vegetable oils" = "vegetable_oils",
                                            "Vegetables" = "vegetable",
                                            "Vegetal products" = "vegetal_products"
                                          ),
                                          selected = "alcoholic_beverages"
                                        ),
                                        
                                        selectInput(
                                          inputId = "z_diet",
                                          label = "Color by:",
                                          choices = c(
                                            "Population density" = "population_density",
                                            "GPD per capita" = "gdp_per_capita",
                                            "Continent" = "continent"
                                          ),
                                          selected = "gdp_per_capita"
                                        ),
                                        
                                        sliderInput(
                                          inputId = "alpha_diet",
                                          label = "Alpha:",
                                          min = 0, max = 1,
                                          value = 0.5
                                        ),
                                        
                                        sliderInput(
                                          inputId = "size_diet",
                                          label = "Size:",
                                          min = 0, max = 5,
                                          value = 2
                                        ),
                                        
                                        textInput(
                                          inputId = "plot_title_diet",
                                          label = "Plot title",
                                          placeholder = "Enter text to be used as plot title"
                                        ),
                                        
                                        actionButton(
                                          inputId = "update_plot_title_diet",
                                          label = "Update plot title"
                                        ),
                                        
                                        br(), br(),
                                        
                                      ),
                                      
                                      mainPanel(
                                        plotOutput(outputId = "scatterplot_diet")
                                      )
                                    )),
                           
                           tabPanel("COVID and Health Indicators",
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        
                                        br(), br(),
                                        
                                        selectInput(
                                          inputId = "y_health",
                                          label = "Y-axis:",
                                          choices = c(
                                            "Total COVID cases" = "total_cases",
                                            "Total COVID cases per million" = "total_cases_per_million",
                                            "Total Deaths" = "total_deaths"
                                          ),
                                          selected = "total_cases_per_million"
                                        ),
                                        
                                        selectInput(
                                          inputId = "x_health",
                                          label = "X-axis:",
                                          choices = c(
                                            "Total vaccinations" = "total_vaccinations",
                                            "Vaccination rates per hundred" = "people_fully_vaccinated_per_hundred",
                                            "Cardiovascular death rates" = "cardiovasc_death_rate",
                                            "Diabetes prevalence" = "diabetes_prevalence",
                                            "Female smokers" = "female_smokers",
                                            "Male smokers" = "male_smokers",
                                            "Obesity percentage" = "obesity_percentage",
                                            "Life expectancy" = "life_expectancy"
                                          ),
                                          selected = "obesity_percentage"
                                        ),
                                        
                                        selectInput(
                                          inputId = "z_health",
                                          label = "Color by:",
                                          choices = c(
                                            "Population density" = "population_density",
                                            "GPD per capita" = "gdp_per_capita",
                                            "Continent" = "continent"
                                          ),
                                          selected = "gdp_per_capita"
                                        ),
                                        
                                        sliderInput(
                                          inputId = "alpha_health",
                                          label = "Alpha:",
                                          min = 0, max = 1,
                                          value = 0.5
                                        ),
                                        
                                        sliderInput(
                                          inputId = "size_health",
                                          label = "Size:",
                                          min = 0, max = 5,
                                          value = 2
                                        ),
                                        
                                        textInput(
                                          inputId = "plot_title_health",
                                          label = "Plot title",
                                          placeholder = "Enter text to be used as plot title"
                                        ),
                                        
                                        actionButton(
                                          inputId = "update_plot_title_health",
                                          label = "Update plot title"
                                        ),
                                        
                                        br(), br(),

                                        
                                      ),
                                      
                                      mainPanel(
                                        plotOutput(outputId = "scatterplot_health")
                                      )
                                    )
                                    
                           ),
                        
                           tabPanel("About",
                                    sidebarLayout(
                                      sidebarPanel(
                                        "Information about this App"
                                      ),
                                      mainPanel(htmlOutput("text1"))
                                    )
                             
                           )
                                    
                           
                           
                           
                        )
                           
             
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  new_plot_title_diet <- eventReactive(
    eventExpr = input$update_plot_title_diet,
    valueExpr = {
      toTitleCase(input$plot_title_diet)
    }
  )
  
  new_plot_title_health <- eventReactive(
    eventExpr = input$update_plot_title_health,
    valueExpr = {
      toTitleCase(input$plot_title_health)
    }
  )
  
  output$scatterplot_health <- renderPlot({
    ggplot(data = covid, aes_string(x = input$x_health, y = input$y_health, color = input$z_health)) +
      geom_point(alpha = input$alpha_health, size = input$size_health) +
      labs(title = new_plot_title_health())
  })
  
  output$scatterplot_diet <- renderPlot({
    ggplot(data = covid, aes_string(x = input$x_diet, y = input$y_diet, color = input$z_diet)) +
      geom_point(alpha = input$alpha_diet, size = input$size_diet) +
      labs(title = new_plot_title_diet())
  })
  
  output$text1 <- renderText({
    str1 <- paste("Created using R Shiny for S3729C CWF") 
    str2 <- paste("<br>") 
    str3 <- paste(
    "The dataset was taken from: https://github.com/owid/covid-19-data/tree/master/public/data,<br>
    https://www.fao.org/faostat/en/#search/obesity,<br>
    https://www.kaggle.com/datasets/mariaren/covid19-healthy-diet-dataset?select=Food_Supply_Quantity_kg_Data.csv")
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)