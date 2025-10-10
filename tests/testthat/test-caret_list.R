integer_df <- data.frame(
  Var1 = sample(1:100, 20, replace = TRUE),
  Var2 = sample(1:100, 20, replace = TRUE),
  Var3 = sample(1:100, 20, replace = TRUE)
)

binary_df <- data.frame(
  Var1 = sample(c("Yes", "No"), 20, replace = TRUE),
  Var2 = sample(c("Yes", "No"), 20, replace = TRUE),
  Var3 = sample(c("Yes", "No"), 20, replace = TRUE)
)

multiclass_df <- data.frame(
  ID = 1:10,
  Var1 = sample(c("A", "B", "C"), 20, replace = TRUE),
  Var2 = sample(c("A", "B", "C"), 20, replace = TRUE),
  Var3 = sample(c("A", "B", "C"), 20, replace = TRUE)
)

data_list <- c(integer_df, binary_df, multiclass_df)

# regression_models <- caretMultimodal::caret_list(
#   target = integer_df[[1]],
#   data_list = data_list
#
# )

