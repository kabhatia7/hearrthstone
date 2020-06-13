test_that("function returns plotly object", {

  Sys.sleep(10)

  df_cards <- get_all_cards()


  my_result_1 <- plot_rarity_stats(df_cards, comparison_var = "health")


  expect_equal(class(my_result_1)[1], "plotly")

  my_result_2 <- changes_over_expac(df_cards)

  expect_equal(class(my_result_2)[1], "plotly")

  my_result_3 <- plot_class_stats(df_cards, comparison_var = "health" )

  expect_equal(class(my_result_3)[1], "plotly")
})


test_that("animate argument works properly",{
  Sys.sleep(5)

  df_cards <- get_all_cards()

  my_result_1 <- plot_class_stats(df_cards, comparison_var = "health", animate = TRUE)

  expect_equal(class(my_result_1)[1], "gganim")


  my_result_2 <- plot_rarity_stats(df_cards, comparison_var = "health", animate = TRUE)

  expect_equal(class(my_result_2)[1], "gganim")


})
