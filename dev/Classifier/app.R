library(shiny)
library(hearrthstone)
library(tidymodels)
library(tidyverse)
library(here)
library(shinythemes)
library(shinyWidgets)
library(kknn)
cards_matrix <- function(dat,class){
  matrix <- dat %>%
    filter(Class == class) %>%
    select(contains("name_")) %>%
    mutate_all(~replace_na(., 0)) %>%
    as.matrix()
  return(matrix)
}
pc_df <- function(dat,class){
  pcs <- prcomp(cards_matrix(dat,class))
  pc_df <-  pcs$x %>%
    as_tibble() %>%
    mutate(
      deckname = filter(dat, Class == class)$deckname
    )
  return(pc_df)
}
dat <- read_csv("df_decks_dummy.csv")  %>%
  mutate_all(~replace_na(., 0)) %>%
  filter(!str_detect(deckname," Deck"))

lineups <- read_csv("lineupdecks.csv") %>% select(-Deck1, -Deck2, -Deck3)

#dh
dh_model <- decision_tree(tree_depth = 30, min_n = 2, cost_complexity = 0) %>%
    set_engine("rpart") %>%
    set_mode("classification")
dh_fit <- dh_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Demon Hunter"))

#druid
druid_model <- nearest_neighbor(neighbors = 7) %>%
  set_engine("kknn") %>%
  set_mode("classification")
druid_fit <- druid_model %>%
  fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Druid") %>% select(-Class))

#mage
mage_model <- nearest_neighbor(neighbors = 7) %>%
  set_engine("kknn") %>%
  set_mode("classification")
mage_fit <- mage_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Mage") %>% select(-Class))

#hunter
hunter_model <- decision_tree(tree_depth = 30, min_n = 2) %>%
    set_engine("rpart") %>%
    set_mode("classification")
hunter_fit <- hunter_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Hunter"))

#shaman
shaman_model <- nearest_neighbor(neighbors = 3) %>%
  set_engine("kknn") %>%
  set_mode("classification")
shaman_fit <- shaman_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Shaman") %>% select(-Class))

#rogue
rogue_model <- nearest_neighbor(neighbors = 7) %>%
  set_engine("kknn") %>%
  set_mode("classification")
rogue_fit <- rogue_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Rogue")%>% select(-Class))

#warrior
warrior_model <- nearest_neighbor(neighbors = 1) %>%
    set_engine("kknn") %>%
    set_mode("classification")
warrior_fit <- warrior_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Warrior") %>% select(-Class))

#warlock
warlock_model <- nearest_neighbor(neighbors = 7) %>%
  set_engine("kknn") %>%
  set_mode("classification")
warlock_fit <- warlock_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Warlock") %>% select(-Class))

#priest

priest_model <- nearest_neighbor(neighbors = 1) %>%
  set_engine("kknn") %>%
  set_mode("classification")
priest_fit <- priest_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Priest")%>% select(-Class))

#paladin

paladin_model <- decision_tree(tree_depth = 15, min_n = 2, cost_complexity = 01.000000e-10) %>%
    set_engine("rpart") %>%
    set_mode("classification")
paladin_fit <- paladin_model %>%
    fit(as.factor(deckname) ~ ., dat %>% filter(Class == "Paladin"))

classify <- function(deckcode){
  if(str_detect(deckcode,"AAEC")){
  if(is.null(get_decks(deckcode)$error) & is.character(deckcode)) {
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
  } else if (deckcode == ""){
    classification <- "Enter a deck code"
    return(classification)
    }else{
      classification <- "Not a valid code"
      return(classification)
    } }else if (deckcode == ""){
      classification <- "Enter a deck code"
      return(classification)
    }else{
      classification <- "Not a valid code"
      return(classification)}

}

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("superhero"),
    setBackgroundImage(
        src = "https://i.pinimg.com/originals/13/dd/f3/13ddf355dedf14439696c80b3c2c81af.jpg"
    ),
    navbarPage(
      "Hearrthstone Classification Application",


      tabPanel("Deck Classification",
               h4("Enter a deck code to see if what it is classified as!"),
                 mainPanel(textInput("DeckCode",
                                     "Deck Code:",
                                     value = ""),
                           verbatimTextOutput("classify"))),

      tabPanel("Master's Tour Data Classifications",
               h4("Classifying the decks of the Largest Card Game Tournament Series"),

                   br(),

                 mainPanel(
                   DT::dataTableOutput("lineups")
                 ))))








# Define server logic required to draw a histogram
server <- function(input, output) {


    output$classify <- renderText({

       classify(input$DeckCode)
    })

    output$lineups <- DT::renderDataTable({
      DT::datatable(lineups, style = "bootstrap")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
