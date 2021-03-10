#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(hearrthstone)
library(tidymodels)
library(tidyverse)
library(here)
library(shinythemes)
library(shinyWidgets)

dat <- read_csv(here("dev", "data", "df_decks_dummy.csv"))
#druid
druid_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
druid_fit <- druid_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Druid"))

#dh
dh_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
dh_fit <- dh_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Demon Hunter"))

#mage
mage_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
mage_fit <- mage_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Mage"))

#hunter
hunter_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
hunter_fit <- hunter_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Hunter"))

#shaman
shaman_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
shaman_fit <- shaman_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Shaman"))

#rogue
rogue_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
rogue_fit <- rogue_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Rogue"))

#warrior
warrior_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
warrior_fit <- warrior_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Warrior"))

#warlock
warlock_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
warlock_fit <- warlock_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Warlock"))

#priest

priest_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
priest_fit <- priest_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Priest"))

#paladin

paladin_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
paladin_fit <- paladin_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Paladin"))

classify <- function(deckcode){
  if(str_detect(deckcode,"AAEC") & is.character(deckcode)) {
      dummy <- get_dummys(deckcode,"deck")
      dummy <- bind_rows(dat %>%
                             filter(Class == dummy$Class),dummy) %>%
          filter(deckname =="deck") %>%
          mutate_all(~replace_na(., 0))
      if(dummy$Class =="Druid"){
          classification <- druid_fit %>%
              predict(new_data = dummy)

      } else if(dummy$Class =="Demon Hunter"){
          classification <- dh_fit %>%
              predict(new_data = dummy)

      } else if(dummy$Class == "Mage"){
          classification <- mage_fit %>%
              predict(new_data = dummy)

      } else if(dummy$Class == "Hunter"){
          classification <- hunter_fit %>%
              predict(new_data = dummy)
      } else if(dummy$Class == "Shaman"){
          classification <- shaman_fit %>%
              predict(new_data = dummy)

      } else if(dummy$Class == "Rogue"){
          classification <- rogue_fit %>%
              predict(new_data = dummy)

      } else if(dummy$Class == "Warrior"){

          classification <- warrior_fit %>%
              predict(new_data = dummy)

      } else if(dummy$Class == "Warlock"){
          classification <- warlock_fit %>%
              predict(new_data = dummy)

      }else if(dummy$Class == "Priest"){
          classification <- priest_fit %>%
              predict(new_data = dummy)

      }else if(dummy$Class == "Paladin"){
          classification <- paladin_fit %>%
              predict(new_data = dummy)
      }
      classification <- as.character(classification$.pred_class)
      return(classification)
  } else{
      classification <- "Not a valid code"
      return(classification)
  }

}

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("superhero"),
    setBackgroundImage(
        src = "https://i.pinimg.com/originals/13/dd/f3/13ddf355dedf14439696c80b3c2c81af.jpg"
    ),
    # Application title
    titlePanel("Hearthstone Classification"),



        # Show a plot of the generated distribution
        mainPanel(
            textInput("DeckCode",
                            "DeckCode:",
                            value = "Enter Code Here"),
            verbatimTextOutput("classify")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {


    output$classify <- renderText({

       classify(input$DeckCode)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
