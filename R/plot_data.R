#' Compare class card statistics between your favorite heroes and villains of Azeroth.
#'
#' @param df_cards optional argument. if you already have a data frame generated from any of the `get_*` functions you may pass it so the function does not have to load a new one. default is null and will select best `get_*` function for your query.
#' @param comparison_var string variable argument. Selects a variable to compare the classes by using the get_* functions of the hearrthstone package, get_all_cards for details
#' @param cardType string argument of what data you would like to compare the classes by, minions, spells, weapons, or all types. To compare all types pass NULL. Default set to "Minions".
#' @param animate animate will allow return an animated plot changing the comparison by the various card sets in the game.
#'
#' @import ggplot2
#' @importFrom glue glue
#' @import gganimate
#' @importFrom stringr str_replace str_to_title
#' @importFrom  plotly ggplotly
#' @import ggthemes
#' @importFrom dplyr filter
#'
#' @return Default outputs an interactive plotly object or a gif if the animate argument is passed as TRUE
#' @export
plot_class_stats <- function(df_cards = NULL, comparison_var, cardType = "Minion",animate = FALSE){

  if(is.null(df_cards)){

    if(!is.null(cardType)){
    cardType <- tolower(cardType)
  }

  if(is.null(cardType)){
  df_cards <- get_all_cards()
  }else if(cardType == "minion"){
    df_cards <- get_minions()
  } else if(cardType == "spell"){
    df_cards <- get_minions()
  } else if(cardType == "weapon"){
    df_cards <- get_weapons()
  }
  } else{
  df_cards <-
    df_cards %>%
      filter(CardType == cardType)
}

 y_var_name <-
    df_cards %>%
    select(!!sym(comparison_var)) %>%
    names() %>%
    str_replace("_", " ") %>%
    str_to_title()

 if(cardType == "spell"){
   stopifnot(y_var_name == "Text Length" | y_var_name == "Manacost")

 }

 if(cardType == "Weapon" | is.null(cardType)){
   stopifnot(y_var_name == "Text Length" | y_var_name == "Manacost" | y_var_name == "Attack" | y_var_name == "Durability")

 }

 if(cardType == "minion" | is.null(cardType)){
   stopifnot(y_var_name == "Text Length" | y_var_name == "Manacost" | y_var_name == "Attack" | y_var_name == "Health")
 }
  colours <- c("Priest"  = "gray60", "Hunter" = "forestgreen", "Rogue" = "khaki", "Mage" = "lightskyblue", "Demon Hunter" = "green3", "Druid" = "orange2", "Shaman" = "royalblue4", "Warlock" = "darkorchid", "Warrior" = "firebrick", "Paladin" = "palevioletred")

  plot <- df_cards %>%
    filter(hero != "Neutral") %>%
    mutate(hero = as.factor(hero)) %>%
    ggplot(aes(x = hero, y = !!sym(comparison_var), color = hero))+
    geom_jitter(aes(text = sprintf("Name: %s <br> Class: %s <br> Mana Cost: %s <br> Card Set: %s", Name, hero,manaCost, CardSet)),position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", size = 5)+
    coord_flip() +
    theme_tufte() +
    theme(
      plot.title.position = "plot",
      legend.position = "none",
      plot.title = element_text(color="#cc9900", size=16, face="bold"),
      axis.title.x = element_text(color="#0099cc", size=14, face="bold"),
      axis.title.y = element_text(color="#0099cc", size=14, face="italic"),
      panel.grid = element_blank()
    ) + scale_color_manual(values = colours)


  if(animate){
    plot <- plot + ggtitle(paste(glue("Summary of classes {y_var_name}"),"by {closest_state}")) +
      coord_flip() +
      labs(x = NULL, y = y_var_name) +
      transition_states(CardSet)

  } else{
    plot <- plot +
      geom_line() +
      ggtitle(glue("Summary of classes {y_var_name}")) +
      labs(x = NULL, y = y_var_name)
    plot <- ggplotly(plot, tooltip = "text")
  }
  return(plot)
}


#' Compare different card rarities to see if legendary and epic cards are really worth your hard earned dust
#'
#'
#' note: there are no neutral spells OR weapons
#' @param df_cards optional argument. if you already have a data frame generated from any of the `get_*` functions you may pass it so the function does not have to load a new one. default is null and will select best `get_*` function for your query.
#' @param comparison_var string variable argument. Selects a variable to compare the classes by using the get_* functions of the hearrthstone package, get_all_cards for details
#' @param cardType string argument of what data you would like to compare the classes by, minions, spells, weapons, or all types. To compare all types pass NULL. Default set to minions.
#' @param class a string that filters results to only show a specific class in Hearthstone, default is NULL to return all Classes. MUST BE PASSED AS FOLLOWS: "demonhunter" "druid","hunter","mage","paladin","priest","rogue","shaman","warlock", "warrior", "neutral"
#' @param animate animate will allow return an animated plot changing the comparison by the various card sets in the game.
#'
#' @import ggplot2
#' @importFrom glue glue
#' @import gganimate
#' @importFrom stringr str_replace str_to_title
#' @importFrom plotly ggplotly
#' @import ggthemes
#'
#' @return Default outputs an interactive plotly object or a gif if the animate argument is passed as TRUE
#' @export
plot_rarity_stats  <- function(df_cards = NULL, comparison_var, cardType = "Minion", class = NULL, animate = FALSE){
if(is.null(df_cards)){
  if(!is.null(cardType)){
    cardType <- tolower(cardType)
  }

  if(is.null(cardType)){
    df_cards <- get_all_cards()
  }else if(cardType == "minion"){
    df_cards <- get_minions()
  } else if(cardType == "spell"){
    df_cards <- get_minions()
  } else if(cardType == "weapon"){
    df_cards <- get_weapons()
  }
} else{
  df_cards <-
    df_cards %>%
    filter(CardType == cardType)
}

  y_var_name <-
    df_cards %>%
    select(!!sym(comparison_var)) %>%
    names() %>%
    str_replace("_", " ") %>%
    str_to_title()

  if(cardType == "spell"){
    stopifnot(y_var_name == "Text Length" | y_var_name == "Manacost")
    message("variable not available for selected card type")
  }

  if(cardType == "Weapon" | is.null(cardType)){
    stopifnot(y_var_name == "Text Length" | y_var_name == "Manacost" | y_var_name == "Attack" | y_var_name == "Durability")
    message("variable not available for selected card type")
  }
  colours <- c("Free" = "gray10", "Common" = "gray60", "Rare" = "dodgerblue4", "Epic" = "magenta4", "Legendary" = "goldenrod")

if(!is.null(class)){
  df_cards <-
    df_cards %>%
    filter(hero == class )
}


  plot <- df_cards %>%
    mutate(Rarity = as.factor(Rarity)) %>%
    ggplot(aes(x = Rarity, y = !!sym(comparison_var), color = Rarity))+
    geom_jitter(aes(text = sprintf("Name: %s <br> Class: %s <br> Mana Cost: %s <br> Card Set: %s", Name, hero, manaCost, CardSet)), position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", size = 5)+
    coord_flip() +
    theme_tufte() +
    theme(
      plot.title.position = "plot",
      legend.position = "none",
      plot.title = element_text(color="#cc9900", size=16, face="bold"),
      axis.title.x = element_text(color="#0099cc", size=14, face="bold"),
      axis.title.y = element_text(color="#0099cc", size=14, face="italic"),
      panel.grid = element_blank()
    ) + scale_color_manual(values = colours)

  if(animate){
    plot <- plot + ggtitle(paste(glue("Summary of {y_var_name} by Rarity"),"by {closest_state}")) +
      coord_flip() +
      labs(x = NULL, y = y_var_name) +
      transition_states(CardSet)

  } else{
    plot <- plot +
      geom_line() +
      ggtitle(glue("Summary of classes {y_var_name} by Rarity")) +
      labs(x = NULL, y = y_var_name)
    plot <- ggplotly(plot, tooltip = "text")
  }

  return(plot)

}



#' Outputs an interactive animated plot that displays the count of card for each expansion in H
#'
#' @importFrom stringr str_to_title
#' @importFrom dplyr group_by filter count enquo
#' @importFrom plotly layout add_bars plot_ly hide_legend animation_opts
#'
#' @param df_cards optional argument. if you already have a data frame generated from any of the get_* functions you may pass it so the function does not have to load a new one. Default set to automatically generate data from get_all_cards function
#' @param classifier a string object that determines what the plot will output, must be "MinionType", "Rarity, or "CardType"
#'
#' @return An interactive plotly object that allows the user to view the distribution of the cards released for the selected expansion
#' @export
changes_over_expac <- function(df_cards = get_all_cards(), classifier = "MinionType"){


  classifier_check <- tolower(classifier)
  if(classifier_check == "miniontype"){
    colours <- c("Mech"  = "gray60",
                 "Beast" = "forestgreen",
                 "Dragon" = "indianred3",
                 "Murloc" = "green3",
                 "Elemental" = "royalblue4",
                 "Demon" = "darkorchid",
                 "Pirate" = "firebrick",
                 "Totem" = "lightskyblue",
                 "All" = "gray17")
  } else if (classifier_check == "rarity"){
    colours <- c("Free" = "gray10",
                 "Common" = "gray60",
                 "Rare" = "dodgerblue4",
                 "Epic" = "magenta4",
                 "Legendary" = "goldenrod")

  } else if (classifier_check == "cardtype" ){
    df_cards <- df_cards %>%
      filter(CardType != "Hero")

     colours <- c(
      "Minion" = "slateblue4",
      "Weapon" = "firebrick",
      "Spell" = "plum"
    )
  }


  t <- list(
    family = "sans serif",
    size = 18,
    color = '#cc9900',
    face = "bold")



  plot <- df_cards %>%
    filter(CardSet != "Basic") %>%
    filter(!is.na(!!sym(classifier))) %>%
    group_by(!!sym(classifier), CardSet) %>%
    count() %>%
    plot_ly(y = ~get(classifier), x = ~n, frame = ~CardSet, ids = ~get(classifier), orientation = "h", color = ~get(classifier), colors = colours) %>%
    add_bars() %>%
    hide_legend() %>%
    animation_opts(frame = 1000, transition = 0, redraw = TRUE) %>%
    layout(title =list(text = sprintf("<b>Distribution of %s released by Expansion</b>",str_to_title(classifier)), x= 0, font = t),
           xaxis = list(title = ""),
           yaxis = list(title = ""))
return(plot)
}





