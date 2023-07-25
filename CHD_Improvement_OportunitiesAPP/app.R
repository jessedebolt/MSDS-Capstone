library(readr)
library(tidyverse)
library(caret)
library(dplyr)
library(shiny)
library(shinythemes)

# Read in dataset and descriptions
cdc <- read_csv("../data/CDC_all.csv")

# Select and rearrange columns
cdc <- cdc %>% select(c("county", "state", "UrbanRural", "CHD", "HighBP", "HighChol", "Stroke", "Diabetes", "Obesity", "PhysInactivity", "Smoker", "Age65Plus","CholScreen", "CholMedNonAdhear", "CholMedElegible", "cruParticipate", "bpmUse", "MedHouseIncome", "Poverty", "Unemploy", "SNAPrecipients", "EdLessColl", "MedHomeValue", "HealthIns", "AirQuality", "Parks", "Broadband","pcp", "CardioPhys", "Hospitals", "Pharmacies", "pop"))

# Convert to 1=Urban, 0=Rural as factor
cdc$UrbanRural <- factor(ifelse(cdc$UrbanRural %in% c("Large_Urban", "LargeFringe_Urban", "MediumSmall_Urban"), 1, 0))

# Full model less other targets and include top 5 features from variable importance shown in (main)
# Remove 'county' and 'state' columns before splitting (otherwise caused issues with predictions)
cdc <- cdc[, !(names(cdc) %in% c('county', 'state'))]

# Set the seed
set.seed(2112)

# Train test split
cdc_index_top5 <- createDataPartition(cdc$CHD, p = 0.8, list = FALSE)
cdc_tr_top5 <- cdc[cdc_index_top5, ]
cdc_te_top5 <- cdc[-cdc_index_top5, ]

# Set control requirements
control_top5 <- trainControl(method="repeatedcv", 
                             number =5,
                             repeats=3)

# Fit the model
model_top5 <- train(CHD ~ . -Stroke -HighChol -HighBP -bpmUse -Unemploy -MedHomeValue -Diabetes -CholMedNonAdhear -PhysInactivity -cruParticipate -AirQuality -Pharmacies -CardioPhys -Hospitals -Obesity -pop -Parks -MedHouseIncome -Poverty -pcp -CholMedElegible -UrbanRural -EdLessColl -CholScreen, 
                    data=cdc_tr_top5, 
                    method="lm", 
                    trControl = control_top5)



ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("CHD Rate Prediction Change"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("smoker",
                  "Percentage of Smokers:",
                  min = 0.1,
                  max = 2,
                  value = 1,
                  step = 0.1),
      helpText("Adjust to simulate changes in the percentage of smokers"),
      
      sliderInput("healthIns",
                  "Health Insurance Accessibility:",
                  min = 0.1,
                  max = 2,
                  value = 1,
                  step = 0.1),
      helpText("Adjust to simulate changes in health insurance accessibility"),
      
      sliderInput("age65Plus",
                  "Age over 65:",
                  min = 0.1,
                  max = 2,
                  value = 1,
                  step = 0.1),
      helpText("Adjust to simulate changes in the percentage of population over 65"),
      
      sliderInput("snapRecipients",
                  "Percentage on SNAP:",
                  min = 0.1,
                  max = 2,
                  value = 1,
                  step = 0.1),
      helpText("Adjust to simulate changes in the percentage of population on SNAP"),
      
      sliderInput("broadband",
                  "Broadband Access Percentage:",
                  min = 0.1,
                  max = 2,
                  value = 1,
                  step = 0.1),
      helpText("Adjust to simulate changes in broadband access")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Text Output", textOutput("percentChange")),
        tabPanel("Plot", plotOutput("plotChange")),
        tabPanel("Table", tableOutput("tableChange"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$percentChange <- renderText({
    
    new_data <- cdc_te_top5
    new_data$Smoker <- new_data$Smoker * input$smoker
    new_data$HealthIns <- new_data$HealthIns * input$healthIns
    new_data$Age65Plus <- new_data$Age65Plus * input$age65Plus
    new_data$SNAPrecipients <- new_data$SNAPrecipients * input$snapRecipients
    new_data$Broadband <- new_data$Broadband * input$broadband

    predictions <- predict(model_top5, newdata = new_data)
    original_predictions <- predict(model_top5, newdata = cdc_te_top5)
    
    mean_diff <- mean(predictions - original_predictions)
    
    percent_change <- mean_diff / mean(original_predictions) * 100
    
    if (percent_change > 0) {
      change_direction <- "Positive"
    } else if (percent_change < 0) {
      change_direction <- "Negative"
    } else {
      change_direction <- "No"
    }
    
    paste("Predicted percent change in CHD rate: ", round(percent_change, 2), "% (", change_direction, " change)", sep = "")
  })
  
  output$plotChange <- renderPlot({
    new_data <- cdc_te_top5
    new_data$Smoker <- new_data$Smoker * input$smoker
    new_data$HealthIns <- new_data$HealthIns * input$healthIns
    new_data$Age65Plus <- new_data$Age65Plus * input$age65Plus
    new_data$SNAPrecipients <- new_data$SNAPrecipients * input$snapRecipients
    new_data$Broadband <- new_data$Broadband * input$broadband
    
    predictions <- predict(model_top5, newdata = new_data)
    original_predictions <- predict(model_top5, newdata = cdc_te_top5)
    
    data.frame(Original = original_predictions, New = predictions) %>%
      gather(key = "Type", value = "Prediction") %>%
      ggplot(aes(x = Type, y = Prediction)) +
      geom_boxplot() +
      labs(title = "Predicted CHD Rate", y = "Prediction")
  })
  
  output$tableChange <- renderTable({
    new_data <- cdc_te_top5
    new_data$Smoker <- new_data$Smoker * input$smoker
    new_data$HealthIns <- new_data$HealthIns * input$healthIns
    new_data$Age65Plus <- new_data$Age65Plus * input$age65Plus
    new_data$SNAPrecipients <- new_data$SNAPrecipients * input$snapRecipients
    new_data$Broadband <- new_data$Broadband * input$broadband
    
    predictions <- predict(model_top5, newdata = new_data)
    original_predictions <- predict(model_top5, newdata = cdc_te_top5)
    
    data.frame(Original = original_predictions, New = predictions)
  })
}

shinyApp(ui = ui, server = server)
