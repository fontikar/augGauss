test_that("Execution and output", {
  expect_named(aug_model("linear"))
  expect_type(aug_model("linear"), "list")
  expect_visible(aug_model("linear"))

  expect_named(aug_model("constant_single"))
  expect_type(aug_model("constant_single"), "list")
  expect_visible(aug_model("constant_single"))

  lm_test <- aug_model("linear") |>
    aug_assign_data(X = Loblolly$age,
                     Y = Loblolly$height,
                     N = nrow(Loblolly)) |>
    aug_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)

  expect_visible(lm_test)

  expect_s4_class(lm_test, "stanfit")
})
