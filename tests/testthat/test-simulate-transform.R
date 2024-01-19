test_that("Function runs", {
  expect_visible(simulate_data(nSubj = 10, nGroups = 3) |> transform_data())
  expect_no_error(simulate_data(nSubj = 10, nGroups = 3) |> transform_data())
  expect_named(transform_data(demo_data))
})

test_that("Check output", {
  set.seed(375)
  sim_data <- simulate_data(nSubj = 10, nGroups = 3)
  expect_named(sim_data)
  expect_s3_class(sim_data, "tbl_df")
  set.seed(375)
  expect_snapshot(simulate_data(nSubj = 10, nGroups = 3))
  vdiffr::expect_doppelganger("simulated_data", plot_simulated_data(sim_data))

  expect_equal(length(transform_data(demo_data)), 3)
  expect_type(transform_data(demo_data), "list")
})

test_that("Errors are triggered", {
  expect_error(demo_data |> transform_data(groupNames = letters[1:2]))
  expect_error(demo_data |> transform_data(dimVals = c(0,1,2,3,4,5)))
  expect_error(demo_data |> transform_data(dimVals = c(1,2,3,4,5,0)))

})
