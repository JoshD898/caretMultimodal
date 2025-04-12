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

Let's train models on rows 10 - 20 of the **cells, holter, and protein** data sets to predict patient **hospitalization** using the **random forest (RF)** method.  

### Creating a `caret_list` object
```r
# Load the heart failure data
load(system.file("sample_data", "HeartFailure.RData", package = "caretMultimodal")) 

models <- caretMultimodal::caret_list(
    target = demo$hospitalizations[10:20], 
    data_list = list(cells = cells[10:20,], holter = holter[10:20,], proteins = proteins[10:20,]), 
    method = "rf"
)

summary(models)
#> The following models were trained: cells_model, holter_model, proteins_model 
#>
#> Model metrics:
#>             model method metric value        sd
#>            <char> <char> <char> <num>     <num>
#> 1:    cells_model     rf    ROC   0.5 0.5000000
#> 2:   holter_model     rf    ROC   0.8 0.4472136
#> 3: proteins_model     rf    ROC   1.0 0.0000000

plot(models)
```
![image](https://github.com/user-attachments/assets/6c896c2a-a88f-4263-a0e7-d95e12138b87)


### Using `caret_stack` to stack models

The `caret_stack` function trains a new `caret::train` object on the predictions from models in a `caret_list`. Let's use the **GLMNET** method to train an ensemble model with the remaining rows of the **cells, holter, and protein** data sets.
```r
stack <- caretMultimodal::caret_stack(
    caret_list = models,
    data_list = list(cells = cells[-(10:20),], holter = holter[-(10:20),], proteins = proteins[-(10:20),]),
    target = demo$hospitalizations[-(10:20)], 
    method = "glmnet"
)

summary(stack)
#> The following models were ensembled: cells_model, holter_model, proteins_model  
#> 
#> Relative importance:
#>                 Overall
#> cells_model    36.65609
#> holter_model   15.42738
#> proteins_model 47.91653
#> 
#> Model metrics (based on caret_stack training data):
#>             model method metric     value         sd
#>            <char> <char> <char>     <num>      <num>
#> 1:       ensemble glmnet    ROC 0.7392857 0.16540766
#> 2:    cells_model     rf    ROC 0.5977564 0.11202056
#> 3:   holter_model     rf    ROC 0.6666667 0.08445071
#> 4: proteins_model     rf    ROC 0.6602564 0.09410487

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


