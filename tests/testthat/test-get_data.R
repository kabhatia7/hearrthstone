test_that("specific get methods retrieve only what they need", {



  my_result_min <-
    get_minions() %>%
    select(CardType) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  Sys.sleep(5)

  my_result_spell <-
    get_spells() %>%
    select(CardType) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  Sys.sleep(5)

  my_result_wep <-
    get_weapons() %>%
    select(CardType) %>%
    filter(CardType == "Weapon") %>%
    dplyr::distinct() %>%
    dplyr::pull()



  expect_equal(my_result_min, "Minion")

  expect_equal(my_result_spell, "Spell")

  expect_equal(my_result_wep, "Weapon")

})


test_that("get_all_cards has appropriate dimensions",{

  Sys.sleep(5)

  my_result <- dim(get_all_cards())

  expect_equal(my_result[1], 2845)
  expect_equal(my_result[2], 12)
})
