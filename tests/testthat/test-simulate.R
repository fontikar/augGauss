test_that("Function runs", {
  expect_visible(simulate_data(nSubj = 10, nGroups = 3))
  expect_no_error(simulate_data(nSubj = 10, nGroups = 3))
})

test_that("Check output", {
  sim_data <- simulate_data(nSubj = 10, nGroups = 3)
  expect_named(sim_data)
  expect_s3_class(sim_data, "tbl_df")
  set.seed(375)
  expect_snapshot(simulate_data(nSubj = 10, nGroups = 3))

  vdiffr::expect_doppelganger("simulated_data", plot_simulated_data(sim_data))
})
