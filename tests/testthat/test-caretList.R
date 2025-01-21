testthat::test_that(".MethodCheck validates a supported model name", {
  expect_null(.MethodCheck("gbm"))
})

test_that(".MethodCheck throws error for unsuported model name", {
  expect_error(.MethodCheck("THIS_IS_NOT_A_VALID_MODEL"))
})

test_that(".MethodCheck validates a supported model name", {
  expect_null(.MethodCheck("gbm"))
})
