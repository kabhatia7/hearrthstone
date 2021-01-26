#' Returns a neat data frame of information about all hearthstone cards of a desired filter
#'
#' @param class a string filters the cards that return to only be from a specific class in Hearthstone, default is NULL to return all Classes
#' @param mana a string argument, filters the data frame to only include cards of a certain mana cost. Default is NULL which returns cards of all mana costs.
#' @importFrom httr GET
#' @import jsonlite
#' @importFrom dplyr select left_join rename mutate bind_rows filter
#' @importFrom tibble tibble
#' @import stringr
#'
#' @return returns a data frame with Name of card, Class, Type, Rarity, Attack, Health, what set it was apart of, durability, armor, and length of card text
#' @export
get_all_cards <- function(class = NULL, mana = NULL){
  my_token <- token_auth()


  search_params <- list()

  search_params$class <- class
  search_params$manaCost <- mana

  if(is.null(class)){
  card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token))
  } else {
    card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token),
                    query = list(tolower(strclass)))

  }
  card_data <- fromJSON(rawToChar(card_raw$content))
  if(card_data$pageCount > 1){
  pages <- 2:card_data$pageCount

  cards <- card_data$cards

  for(i in pages){
    search_params$page <- i
    card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token), query = search_params)
    card_data_n <- fromJSON(rawToChar(card_raw$content))$cards
    cards <- bind_rows(cards, card_data_n)
   # sleeps to ensure do not exceed max number of calls per second
     Sys.sleep(.1)
  }
  }else{
    cards <- card_data$cards
  }
  metadata_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/metadata?locale=en_US&access_token=",my_token))

  metadata <- fromJSON(rawToChar((metadata_raw$content)))



  classes <- metadata$classes
  rarity <- metadata$rarities
  sets <- metadata$sets
  tribes <- metadata$minionTypes
  types <- metadata$types



  df_cards <- cards %>%
    left_join(classes, by = c("classId" = "id")) %>%
    rename(hero = name.y) %>%
    left_join(rarity, by = c("rarityId" = "id")) %>%
    rename(Name = name.x) %>%
    rename(Rarity = name) %>%
    left_join(sets, by = c("cardSetId" = "id")) %>%
    rename(CardSet = name) %>%
    left_join(tribes, by = c("minionTypeId" = "id")) %>%
    rename(MinionType = name) %>%
    left_join(types, by = c("cardTypeId" = "id")) %>%
    rename(CardType = name) %>%
    select(Name, MinionType, hero, Rarity,manaCost,CardSet,MinionType,CardType, attack, health, durability,text) %>%
    mutate(Text_length = str_count(text))

  return(df_cards)
}




#' main difference between get_spells and get_all_cards is that it returns a data frame with variables only containing values pertaining to spells
#'
#' @param class a string that filters results to only show specific class in Hearthstone, default is NULL to return all Classes. MUST BE PASSED AS FOLLOWS: "demonhunter" "druid","hunter","mage","paladin","priest","rogue","shaman","warlock", or "warrior"
#' @param mana a string argument, filters the data frame to only include cards of a certain mana cost. Default is NULL which returns cards of all mana costs.
#'
#' @importFrom httr GET
#' @import jsonlite
#' @importFrom dplyr select left_join rename mutate bind_rows filter
#' @importFrom tibble tibble
#' @import stringr
#'
#' @return returns a data frame with Name of card, Class, Type, Rarity, what set it was apart of, and length of card text
#'
#'
#' @export
get_spells <- function(class = NULL, mana = NULL){

  my_token <- token_auth()



  search_params <- list(type = "spell")

  search_params$class <- class
  search_params$manaCost <- mana

  card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token),
                  query = search_params)

  card_data <- fromJSON(rawToChar(card_raw$content))

  if(card_data$pageCount > 1){
    pages <- 2:card_data$pageCount

    cards <- card_data$cards

    for(i in pages){
      search_params$page <- i
      card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token), query = search_params)
      card_data_n <- fromJSON(rawToChar(card_raw$content))$cards
      cards <- bind_rows(cards, card_data_n)
      # sleeps to ensure do not exceed max number of calls per second
      Sys.sleep(.1)
    }
  }else{
    cards <- card_data$cards
  }
metadata_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/metadata?locale=en_US&access_token=",my_token))

metadata <- fromJSON(rawToChar((metadata_raw$content)))




classes <- metadata$classes
rarity <- metadata$rarities
sets <- metadata$sets
types <- metadata$types


df_cards <- cards %>%
  left_join(classes, by = c("classId" = "id")) %>%
  rename(hero = name.y) %>%
  left_join(rarity, by = c("rarityId" = "id")) %>%
  rename(Name = name.x) %>%
  rename(Rarity = name) %>%
  left_join(sets, by = c("cardSetId" = "id")) %>%
  rename(CardSet = name) %>%
  left_join(types, by = c("cardTypeId" = "id")) %>%
  rename(CardType = name) %>%
  select(Name, hero, Rarity, manaCost,CardSet,CardType,text) %>%
  mutate(Text_length = str_count(text))

return(df_cards)
}




#' main difference between get_minions and get_all_cards is that it returns a data frame with variables only containing values pertaining to minions
#'
#' @param class a string that filters results to only show specific class in Hearthstone, default is NULL to return all Classes. MUST BE PASSED AS FOLLOWS: "demonhunter" "druid","hunter","mage","paladin","priest","rogue","shaman","warlock", or "warrior"
#' @param mana a string argument, filters the data frame to only include cards of a certain mana cost. Default is NULL which returns cards of all mana costs.
#' @param attack a string argument, filters the data frame to only include cards with a specific amount of attack. Default is NULL which returns cards of all attack strengths.
#' @param health a string argument, filters the data frame to only include cards with a specific amount of health. Default is NULL which returns cards of all health totals
#'
#'
#' @importFrom httr GET
#' @import jsonlite
#' @importFrom dplyr select left_join rename mutate bind_rows filter
#' @importFrom tibble tibble
#' @import stringr
#'
#' @return returns a data frame with Name of card, Class, Type, Rarity, what set it was apart of, and length of card text
#'
#'
#' @export
get_minions <- function(class = NULL, mana = NULL, attack = NULL, health = NULL){

  my_token <- token_auth()


  search_params <- list(type = "minion")

  search_params$class <- class
  search_params$manaCost <- mana
  search_params$attack <- attack
  search_params$health <- health

    card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token),
                    query = search_params)

  card_data <- fromJSON(rawToChar(card_raw$content))

  if(card_data$pageCount > 1){
    pages <- 2:card_data$pageCount

    cards <- card_data$cards

    for(i in pages){

      search_params$page <- i
      card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token), query = search_params)
      card_data_n <- fromJSON(rawToChar(card_raw$content))$cards
      cards <- bind_rows(cards, card_data_n)
      # sleeps to ensure do not exceed max number of calls per second
      Sys.sleep(.1)
    }
  }else{
    cards <- card_data$cards
  }
  metadata_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/metadata?locale=en_US&access_token=",my_token))

  metadata <- fromJSON(rawToChar((metadata_raw$content)))


  classes <- metadata$classes
  rarity <- metadata$rarities
  sets <- metadata$sets
  tribes <- metadata$minionTypes
  types <- metadata$types


  df_cards <- cards %>%
    left_join(classes, by = c("classId" = "id")) %>%
    rename(hero = name.y) %>%
    left_join(rarity, by = c("rarityId" = "id")) %>%
    rename(Name = name.x) %>%
    rename(Rarity = name) %>%
    left_join(sets, by = c("cardSetId" = "id")) %>%
    rename(CardSet = name) %>%
    left_join(tribes, by = c("minionTypeId" = "id")) %>%
    rename(MinionType = name) %>%
    left_join(types, by = c("cardTypeId" = "id")) %>%
    rename(CardType = name) %>%
    select(Name, MinionType, hero, Rarity,manaCost,CardSet, attack, health,text, CardType) %>%
    mutate(Text_length = str_count(text))
  return(df_cards)
}

#' main difference between get_minions and get_all_cards is that it returns a data frame with variables only containing values pertaining to minions
#'
#' @param class a string that filters results to only show specific class in Hearthstone, default is NULL to return all Classes. MUST BE PASSED AS FOLLOWS: "demonhunter" "druid","hunter","mage","paladin","priest","rogue","shaman","warlock", or "warrior"
#' @param mana a string argument, filters the data frame to only include cards of a certain mana cost. Default is NULL which returns cards of all mana costs.
#' @param attack a string argument, filters the data frame to only include cards with a specific amount of attack. Default is NULL which returns cards of all attack strengths.
#' @param durability a string argument, filters the data frame to only include cards with a specific amount of health. Default is NULL which returns cards of all health totals
#'
#'
#' @importFrom httr GET
#' @import jsonlite
#' @importFrom dplyr select left_join rename mutate bind_rows filter
#' @importFrom tibble tibble
#' @import stringr
#'
#' @return returns a data frame with Name of card, Class, Type, Rarity, what set it was apart of, attack, durabilty, and length of card text
#'
#'
#' @export
get_weapons <- function(class = NULL, mana = NULL, attack = NULL, durability = NULL){


  my_token <- token_auth()


  search_params <- list(type = "weapon")

  search_params$class <- class
  search_params$manaCost <- mana
  search_params$attack <- attack
  search_params$durability <- durability


  card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token),
                  query = search_params)

  card_data <- fromJSON(rawToChar(card_raw$content))

  if(card_data$pageCount > 1){
    pages <- 2:card_data$pageCount

    cards <- card_data$cards

    for(i in pages){
      card_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/cards?locale=en_US&access_token=",my_token), query = list(page = pages[i]))
      card_data_n <- fromJSON(rawToChar(card_raw$content))$cards
      cards <- bind_rows(cards, card_data_n)
      # sleeps to ensure do not exceed max number of calls per second
      Sys.sleep(.1)
    }
  }else{
    cards <- card_data$cards
  }
  metadata_raw <- GET(paste0("https://us.api.blizzard.com/hearthstone/metadata?locale=en_US&access_token=",my_token))

  metadata <- fromJSON(rawToChar((metadata_raw$content)))



  classes <- metadata$classes
  rarity <- metadata$rarities
  sets <- metadata$sets
  tribes <- metadata$minionTypes
  types <- metadata$types

  df_cards <- cards %>%
    left_join(classes, by = c("classId" = "id")) %>%
    rename(hero = name.y) %>%
    left_join(rarity, by = c("rarityId" = "id")) %>%
    rename(Name = name.x) %>%
    rename(Rarity = name) %>%
    left_join(sets, by = c("cardSetId" = "id")) %>%
    rename(CardSet = name) %>%
    left_join(types, by = c("cardTypeId" = "id")) %>%
    rename(CardType = name) %>%
    select(Name, hero, Rarity,manaCost,CardSet, attack, durability,text, CardType) %>%
    mutate(Text_length = str_count(text))
  return(df_cards)
}

#' Title here
#'
#' @param deckcode a string of characters that identifies a deck in the game of hearthstone
#'
#'
#' @importFrom httr GET
#' @import jsonlite
#' @importFrom dplyr select left_join rename mutate bind_rows filter
#' @importFrom tibble tibble
#' @import stringr
#'
#' @return returns a data frame with Name of card, Class, Type, Rarity, what set it was apart of, and length of card text
#'
#'
#' @export
get_decks <- function(deckcode){
  my_token <- token_auth()
  search_params <- list()
  search_params$code <- deckcode

  metadata_deck <- GET(paste0("https://us.api.blizzard.com/hearthstone/deck?locale=en_US&access_token=",my_token),
      query = search_params)
  deck <- fromJSON(rawToChar((metadata_deck$content)))

  return(deck)
}

#' Title here
#'
#' @param deckcode a string of characters that identifies a deck in the game of hearthstone
#'
#'
#' @importFrom httr GET
#' @import jsonlite
#' @importFrom dplyr select left_join rename mutate bind_rows filter
#' @importFrom tibble tibble
#' @import stringr
#'
#' @return returns a data frame with Name of card, Class, Type, Rarity, what set it was apart of, and length of card text
#'
#'
#' @export
get_hsreplay_decks <- function(websitecode){

  hsreplayurl <- read_html(paste0('https://hsreplay.net/decks/',websitecode)) %>%
    html_nodes('head')

  hsreplayinfo <- tibble(name = hsreplayurl %>%
                           html_nodes(xpath = '//meta[@property = "x-hearthstone:deck"]') %>%
                           html_attr('content'),
                         deckcode = hsreplayurl %>%
                           html_nodes(xpath = '//meta[@property = "x-hearthstone:deck:deckstring"]') %>%
                           html_attr('content'))

  df_deck <- get_decks(hsreplayinfo$deckcode)
  df_deck <- hsreplayinfo %>%
    mutate(Class = as.factor(df_deck$class$name), ManaCurve = mean(df_deck$cards$manaCost))



  return(df_deck)
}

#' Regenerate Token
#'
#'
#' The blizzard API is a little funky and requires you to regenerate an access token every 24 hours.
#' This function allows us to regenerate the keys within our functions so we don't have to worry about that again
#' For the purposes of this package we will be using my developer key and client secret.
#'
#' @importFrom RCurl base64
#' @importFrom httr add_headers POST
#'
#' @return an access token to be used for all other functions
token_auth <- function(){
  id_secret<-RCurl::base64(paste("e046764aa4cb44eaba17e02e01e9456d","YBRD5LI4giatx5yj9zwZnY7CyKSJmHx8",sep=':'))[[1]]
  my_headers<-httr::add_headers(c(Authorization=paste('Basic',id_secret,sep=' ')))
  my_body<-list(grant_type='client_credentials')
  my_token<-httr::content(httr::POST('https://us.battle.net/oauth/token',my_headers,body=my_body,encode='form'))
  return(my_token$access_token)
}
