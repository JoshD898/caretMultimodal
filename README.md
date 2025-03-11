<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/JoshD898/caretMultimodal/graph/badge.svg)](https://app.codecov.io/gh/JoshD898/caretMultimodal)
<!-- badges: end -->

# caretMultimodal

CaretMultimodal is a wrapper around the [caret](https://github.com/topepo/caret) package that allows for simplified 
multi-dataset training and ensembling. It is heavily inspired by Zach Mayer's 
[caretEnsemble](https://github.com/zachmayer/caretEnsemble) package.

## Example Usage

For the following examples, we will [these publicly available data sets](https://amritsingh.shinyapps.io/omicsBioAnalytics/) on heart failure. 
The data sets are described [here](https://pubmed.ncbi.nlm.nih.gov/30935638/).

Let's train models on the **cells, holter, and protein** data sets to predict patient **hospitalization** using the **generalized linear model (GLM)** method.  

### Creating a `caret_list` object
```r
load(system.file("sample_data", "HeartFailure.RData", package = "caretMultimodal")) # Load the heart failure data

models <- caretMultimodal::caret_list(
    target = demo$hospitalizations, 
    data_list = list(cells = cells, holter = holter, proteins = proteins), 
    method = "glm"
)

print(summary(models))
#> The following models were trained: cells_model, holter_model, proteins_model 
#>
#> Model metrics:
#>             model method metric     value         sd
#>            <char> <char> <char>     <num>      <num>
#> 1:    cells_model    glm    ROC 0.5962963 0.08114408
#> 2:   holter_model    glm    ROC 0.5037037 0.15843889
#> 3: proteins_model    glm    ROC 0.5944444 0.13833222

predict(models)
#>      cells_model holter_model proteins_model
#>            <num>        <num>          <num>
#>  1: 3.082239e-02 1.512935e-06   2.220446e-16
#>  2: 9.971526e-01 2.220446e-16   1.000000e+00
#>  3: 8.656950e-01 2.220446e-16   1.000000e+00
#>              ...          ...            ...

plot(models)
```
![image](https://github.com/user-attachments/assets/746943eb-e12f-4df3-a67e-74e7ca94235b)

### Using `caret_stack` to stack models

The `caret_stack` function trains a new `caret::train` object on the predictions from models in a `caret_list`. Let's use the **Random Forest** method to ensemble the models we just trained.
```r
stack <- caretMultimodal::caret_stack(
    caret_list = models,
    method = "rf"
)

print(summary(stack))
#> The following models were ensembled: cells_model, holter_model, proteins_model  
#> 
#> Relative importance:
#>                 Overall
#> cells_model    21.19387
#> holter_model   20.79166
#> proteins_model 58.01447
#> 
#> Model accuracy:
#>             model method metric     value        sd
#>            <char> <char> <char>     <num>     <num>
#> 1:       ensemble     rf    ROC 0.6314815 0.2226462
#> 2:    cells_model    glm    ROC 0.6203704 0.1178511
#> 3:   holter_model    glm    ROC 0.4925926 0.2711714
#> 4: proteins_model    glm    ROC 0.5314815 0.1758772

predict(
    stack,
    new_data_list = NULL  # When this is null, the prediction is based on the training data for each model. 
)
#> Yes
#>     <num>
#>  1: 0.076
#>  2: 0.186
#>  3: 0.472
#> ...

plot(stack)
```
![image](https://github.com/user-attachments/assets/c4868fe3-d018-4b1c-b492-08ae22a8cd56)
![image](https://github.com/user-attachments/assets/055d13ee-fe36-48b4-ad31-a5c8460ec651)


## Installation
The package can be installed using devtools
```r
devtools::install_github("JoshD898/caretMultimodal")
```

## Project Structure

This project generally follows the [Tidyverse style guide](https://style.tidyverse.org/).  

### Naming Conventions  
- Internal (non-exported) functions are prefixed with `.` to hide them from the package namespace.  

### File Organization  
Each object’s definition and its associated methods are contained within a single file.  

- **`caret_list.R`** – Defines the `caret_list` object and its methods.  
- **`caret_stack.R`** – Defines the `caret_stack` object and its methods.  
- **`helpers.R`** – Contains internal helper functions shared across multiple objects.


