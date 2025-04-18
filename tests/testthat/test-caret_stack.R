# Setup ------------------------------------------------------------------------
library(testthat)
set.seed(192L)

numeric_vector <- runif(30)
binary_factor_vector <- factor(sample(c("Type1", "Type2"), 30, replace = TRUE))
three_factor_vector <- factor(sample(c("Type1", "Type2", "Type3"), 30, replace = TRUE))

numeric_table <- data.table::data.table(
  Var1 = runif(30),
  Var2 = rnorm(30),
  Var3 = runif(30, 10, 50),
  Var4 = rpois(30, lambda = 5),
  Var5 = rnorm(30, mean = 100, sd = 15)
)

factor_table <- data.table::data.table(
  Var1 = factor(sample(c("A", "B", "C"), 30, replace = TRUE)),
  Var2 = factor(sample(c("X", "Y", "Z"), 30, replace = TRUE)),
  Var3 = factor(sample(c("Red", "Blue", "Green"), 30, replace = TRUE)),
  Var4 = factor(sample(c("Small", "Medium", "Large"), 30, replace = TRUE)),
  Var5 = factor(sample(c("Yes", "No"), 30, replace = TRUE))
)

mixed_table <- data.table::data.table(
  Num1 = runif(30),
  Num2 = rnorm(30),
  Num3 = rpois(30, lambda = 5),
  Factor1 = factor(sample(c("Apple", "Banana", "Cherry"), 30, replace = TRUE)),
  Factor2 = factor(sample(c("Hot", "Cold"), 30, replace = TRUE))
)

unnamed_data_list = list(numeric_table,
                         factor_table,
                         mixed_table)

named_data_list = list(numeric = numeric_table,
                       factor = factor_table,
                       mixed = mixed_table)

numeric_model <- suppressWarnings(caret::train(x = numeric_table, y = numeric_vector, method = "rf", trControl = .default_control(numeric_vector)))
factor_model <- suppressWarnings(caret::train(x = numeric_table, y = binary_factor_vector, method = "glm", trControl = .default_control(binary_factor_vector)))

named_models <- suppressWarnings(caret_list(target = binary_factor_vector, data_list = named_data_list, method = "rf"))

stack <- suppressWarnings(caret_stack(named_models, method = "glm", target = binary_factor_vector, data_list = named_data_list))

# Constructor tests ------------------------------------------------------------

test_that("caret_stack", {
  expect_true(inherits(stack, "caret_stack"))
  expect_silent(stack_with_new_data <- caret_stack(named_models, method = "glm", target = numeric_vector, data_list = named_data_list))
  expect_error(stack_bad_new_data <- caret_stack(named_models, method = "glm", target = numeric_vector, data_list = list(factor_table, factor_table, factor_table)))
})

test_that("caret_stack invalid input", {
  expect_error(caret_stack(named_models, method = "glm", target = rep("Yes", 30), data_list = named_data_list),
                            "Target vector must contain two or more classes.")

  expect_error(caret_stack(named_models, method = "glm", target = binary_factor_vector[10:20], data_list = named_data_list),
               "The number of rows of data_list\\[\\[1\\]\\] does not match the length of the target vector.")
})

# Method tests -----------------------------------------------------------------

test_that("predict.caret_stack", {
  pred <- predict(stack)
  pred_new_data <- predict(stack, new_data_list = list(numeric_table[1:5, ], factor_table[1:5,], mixed_table[1:5,]))

  expect_equal(ncol(pred), 1)
  expect_equal(nrow(pred), 30)

  expect_equal(ncol(pred_new_data), 1)
  expect_equal(nrow(pred_new_data), 5)
})

test_that("summary.caret_stack", {
  summary <- summary(stack)

  expect_equal(names(summary), c("models", "imp", "metric", "results"))
  expect_true(inherits(summary, "summary.caret_stack"))
  expect_equal(sum(summary$imp$Overall), 100)
})

test_that("print, print.summary", {
  expect_output(print(stack))
  expect_output(print(summary(stack)))
})

test_that("extract_metric.caret_stack", {
  metric <- extract_metric(stack)

  expect_equal(nrow(metric), 4)
  expect_equal(colnames(metric), c("model", "method", "metric", "value", "sd"))
})

test_that("plot.caret_stack", {
  three_factor_models <- suppressWarnings(caret_list(target = three_factor_vector, data_list = named_data_list, method = "rf"))
  three_factor_stack <- suppressWarnings(caret_stack(named_models, method = "rf", target = three_factor_vector, data_list = named_data_list))

  suppressWarnings(plt <- plot(stack))
  suppressWarnings(three_factor_plt <- plot(three_factor_stack))
  expect_true(inherits(plt, "ggplot"))
  expect_true(inherits(three_factor_plt, "ggplot"))
})
