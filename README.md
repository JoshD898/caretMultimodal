<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/JoshD898/caretMultimodal/graph/badge.svg)](https://app.codecov.io/gh/JoshD898/caretMultimodal)
[![R-CMD-check](https://github.com/JoshD898/caretMultimodal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoshD898/caretMultimodal/actions/workflows/R-CMD-check.yaml)
[![CodeFactor](https://www.codefactor.io/repository/github/joshd898/caretmultimodal/badge)](https://www.codefactor.io/repository/github/joshd898/caretmultimodal)
![Code Size](https://img.shields.io/github/languages/code-size/joshD898/caretMultimodal)

<!-- badges: end -->

# caretMultimodal

CaretMultimodal is a wrapper around the
[caret](https://github.com/topepo/caret) package that allows for
simplified multi-dataset training and ensembling. It is heavily inspired
by Zach Mayer's
[caretEnsemble](https://github.com/zachmayer/caretEnsemble) package.

## Example Usage

For the following examples, we will [these publicly available data
sets](https://amritsingh.shinyapps.io/omicsBioAnalytics/) on heart
failure. The data sets are described
[here](https://pubmed.ncbi.nlm.nih.gov/30935638/).

Let's train models on the **cells, holter, mrna and protein**
data sets to predict patient **hospitalization** using the **GLMNET** method.
A feature of this package is that 5 fold cross validation will be done by default.

### Creating a `caret_list` object

``` r
set.seed(123L)

# Load the heart failure data

caretMultimodal::load_heart_failure()

# Set up tuneGrid

alphas <- seq(0, 1, by = 0.1)
lambdas <- seq(0.001, 0.1, by = 0.01)
tuneGrid <- expand.grid(alpha = alphas, lambda = lambdas)

# Train the base models

base_models <- caretMultimodal::caret_list(
  target = demo$hospitalizations,
  data_list = list(
    cells = cells,
    holter = holter,
    mrna = mrna,
    proteins = proteins
  ),
  method = "glmnet",
  tuneGrid = tuneGrid
)

# oof_predictions returns the out-of-fold predictions from the cross validation process

oof_predictions(base_models)
#>         cells     holter        mrna     proteins
#>         <num>      <num>       <num>        <num>
#> 1: 0.40001341 0.36187558 0.343293728 0.9583191347
#> 2: 0.29711949 0.19240098 0.372329672 0.2918167839
#> 3: 0.31321699 0.20112728 0.253431072 0.9460390426
#> 4: 0.16659585 0.25782410 0.200639977 0.3159580667
#> 5: 0.42837382 0.26038426 0.835305233 0.0400812811
#>           ...        ...         ...          ...

# summary shows the best tuning parameters and corresponding cross validated metrics from training

summary(base_models)
#>       model method alpha lambda       ROC      Sens       Spec      ROCSD     SensSD    SpecSD
#>      <char> <char> <num>  <num>     <num>     <num>      <num>      <num>      <num>     <num>
#> 1:    cells glmnet   0.8  0.091 0.7740741 0.9777778 0.00000000 0.14721931 0.04969040 0.0000000
#> 2:   holter glmnet   0.8  0.091 0.7851852 1.0000000 0.10000000 0.08842471 0.00000000 0.2236068
#> 3:     mrna glmnet   0.0  0.091 0.8407407 0.9555556 0.06666667 0.12803249 0.06085806 0.1490712
#> 4: proteins glmnet   0.3  0.051 0.9111111 0.9555556 0.30000000 0.08425417 0.06085806 0.2981424

```

### Using `caret_stack` to stack models

Now, lets use the `caret_stack` function to train an ensemble model on the out-of-fold predictions from the base models. 

Alternatively, if we wanted to we could use transfer learning to train the ensemble model on entirely new data.

``` r
stack <- caretMultimodal::caret_stack(
    caret_list = models,
    method = "glmnet",
    tuneGrid = tuneGrid
)

# summary now includes the ensemble model

summary(stack)
#>       model method alpha lambda       ROC      Sens       Spec      ROCSD     SensSD    SpecSD
#>      <char> <char> <num>  <num>     <num>     <num>      <num>      <num>      <num>     <num>
#> 1:    cells glmnet   0.8  0.091 0.7740741 0.9777778 0.00000000 0.14721931 0.04969040 0.0000000
#> 2:   holter glmnet   0.8  0.091 0.7851852 1.0000000 0.10000000 0.08842471 0.00000000 0.2236068
#> 3:     mrna glmnet   0.0  0.091 0.8407407 0.9555556 0.06666667 0.12803249 0.06085806 0.1490712
#> 4: proteins glmnet   0.3  0.051 0.9111111 0.9555556 0.30000000 0.08425417 0.06085806 0.2981424
#> 5: ensemble glmnet   0.3  0.091 0.9444444 0.9333333 0.43333333 0.05555556 0.09938080 0.3651484

# oof_predictions returns the out-of-fold predictions for the ensemble model, as well as whatever data was used to train the ensemble model.

# predict is used to make predictions on new datasets

predict(
    stack,
    new_data_list = list(cells = cells, holter = holter, proteins = proteins)
)
#>           Yes
#>         <num>
#> 1: 0.25371960
#> 2: 0.07682839
#> 3: 0.30947239
#> 4: 0.26927079
#>          ...

plot(stack)
```

![image](https://github.com/user-attachments/assets/5e51c9fc-9e83-4d2b-9ab5-1085fff78d88)

## Installation

The package can be installed using devtools

``` r
devtools::install_github("JoshD898/caretMultimodal")
```

## Project Structure

This project generally follows the [Tidyverse style
guide](https://style.tidyverse.org/).

### Naming Conventions

-   Internal (non-exported) functions are prefixed with `.` to hide them
    from the package namespace.

### File Organization

Each object’s definition and its associated methods are contained within
a single file.

-   **`caret_list.R`** – Defines the `caret_list` object and its
    methods.\
-   **`caret_stack.R`** – Defines the `caret_stack` object and its
    methods.\
-   **`helpers.R`** – Contains internal helper functions shared across
    multiple objects.
