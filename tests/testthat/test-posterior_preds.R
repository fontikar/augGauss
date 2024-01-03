test_that("functions executes", {
  expect_named(augGauss:::Get_Posterior_Preds_by_group(output, "group1"))
  vdiffr::expect_doppelganger("posterior_preds", Plot_Posterior_Preds_by_group(output, "group1"))
})


test_that("throws errors when group name is wrong", {
  expect_error(Plot_Posterior_Preds_by_group(output, "group_wrong"))
})
