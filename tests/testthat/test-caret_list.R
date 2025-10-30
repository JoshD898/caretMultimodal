# Setup ------------------------------------------------------------------------
library(testthat)
set.seed(192L)

# Test data --------------------------------------------------------------------

numeric_vector <- runif(30)
binary_vector <- factor(sample(c("Type1", "Type2"), 30, replace = TRUE))
multiclass_vector <- factor(sample(c("Type1", "Type2", "Type3"), 30, replace = TRUE))

make_df <- function(n = 30) {
  data.table::data.table(
    Var1 = runif(n),
    Var2 = rnorm(n),
    Var3 = runif(n, 10, 50),
    Var4 = rpois(n, lambda = 5),
    Var5 = rnorm(n, mean = 100, sd = 15)
  )
}

df1 <- make_df()
df2 <- make_df()
df3 <- make_df()


# Core Tests -----------------------------------------------------------------------

#' Makes a caret_list with the default trControls and metrics, then makes sure all methods run without error
caret_list_generic_test <- function(target, data_list, method) {

  models <- suppressWarnings(caret_list(target = target, data_list = data_list, method = method))

  test_that("caret_list constructor", {
    expect_s3_class(models, "caret_list")
    expect_true(all(vapply(models, inherits, logical(1), "train")))
    expect_length(models, length(data_list))
  })

  test_that("summary.caret_list", {
    summary <- summary(models)

    expect_s3_class(summary, "data.table")
    expect_true(all(c("model", "method") %in% names(summary)))
    expect_equal(nrow(summary), length(data_list))
  })

  test_that("predict.caret_list", {
    preds <- suppressWarnings(predict(models, data_list))

    expect_s3_class(preds, "data.table")
    expect_equal(nrow(preds), nrow(data_list[[1]]))
    expect_equal(ncol(preds), length(data_list))
  })

  test_that("oof_predictions.caret_list", {
    oof_preds <- oof_predictions(models)

    expect_s3_class(oof_preds, "data.table")
    expect_equal(nrow(oof_preds), length(target))
  })
}

# Realistically this should loop through all the available models that caret has... for now just testing random forest and glmnet

caret_list_generic_test(
  target = numeric_vector,
  data_list = list(df1, df2, df3),
  method = "glmnet")

caret_list_generic_test(
  target = binary_vector,
  data_list = list(df1, df2, df3),
  method = "glmnet")

caret_list_generic_test(
  target = binary_vector,
  data_list = list(df1, df2, df3),
  method = "rf")


# Edge Case Tests ------------------------------------------------------------

test_that("Invalid inputs", {

  indexed_target <- data.frame(value = numeric_vector)
  indexed_target$index <- seq_len(nrow(indexed_target))

  indexed_numeric_table <- df1
  indexed_numeric_table$index <- seq_len(nrow(indexed_numeric_table))

  expect_error(caret_list(target = numeric_vector, data_list = list(head(df1, 29)), method = "glm"),
               "The number of rows of data_list\\[\\[1\\]\\] does not match the length of the target vector.")

  expect_error(caret_list(target = indexed_target, data_list = list(df1), method = "glm"),
               "Target must be a vector when no identifier column name is provided.")

  expect_error(caret_list(target = indexed_target, data_list = list(df1), method = "glm", identifier_column_name = "index"),
               "The identifier column 'index' is missing in data_list\\[\\[1\\]\\].")

  expect_error(caret_list(target = numeric_vector, data_list = list(indexed_numeric_table), method = "glm", identifier_column_name = "index"),
               "The identifier column 'index' is missing in the target dataframe.")

  indexed_target$extra_column <- seq_len(nrow(indexed_target))

  expect_error(caret_list(target = indexed_target, data_list = list(indexed_numeric_table), method = "glm", identifier_column_name = "index"),
               "Target must have exactly two columns: one serving as an identifier and the other containing values for training.")
})

test_that("Parallelization", {

  n_cores <- parallel::detectCores() - 1

  if (n_cores > 1) {
    cluster <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cluster)

    on.exit({
      parallel::stopCluster(cluster)
      foreach::registerDoSEQ()
    }, add = TRUE)

    expect_message(
      caret_list(target = binary_vector, data_list = list(df1, df2, df3), method = "glmnet"),
      sprintf("Using parallel backend with %d workers.", n_cores)
    )
  }
})


test_that(".match_identifiers", {

  target <- data.table::data.table(
    Identifier = c(1,2,3,4,5),
    Targ = c("A", "B", "C", "D", "E")
  )

  data1 <- data.table::data.table(
    Identifier = c(1,2,3)
  )

  data2 <- data.table::data.table(
    Identifier = c(3,2,5)
  )

  expect_equal(.match_identifiers(target, data1, "Identifier"), c("A","B","C"))
  expect_equal(.match_identifiers(target, data2, "Identifier"), c("C","B","E"))

  expect_true(is.character(.match_identifiers(target, data1, "Identifier")))
})




