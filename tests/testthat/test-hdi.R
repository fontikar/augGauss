test_that("functions executes", {
  expect_named(output |> Get_HDIs())
  expect_type(output |> Get_HDIs(), "list")
  expect_visible(output |> Get_HDIs())

  expect_named(output |> Get_HDIs_diff())
  expect_type(output |> Get_HDIs_diff(), "list")
  expect_visible(output |> Get_HDIs_diff())
})


test_that("output is expected", {
  expect_snapshot(output |> Get_HDIs())
  expect_snapshot(output |> Get_HDIs_diff())
})
