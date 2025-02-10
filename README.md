
# caretMultimodal

CaretMultimodal is a wrapper around the [caret](https://github.com/topepo/caret) package that allows for simplified 
multi-dataset training and ensembling. It is heavily inspired by Zach Mayer's 
[caretEnsemble](https://github.com/zachmayer/caretEnsemble) package.

**NOTE** This is still a work in progress. `caret_list` is  functional, and I am currently working on `caret_stack`. 
## Example Usage

For the following examples, we will [these publicly available data sets](https://amritsingh.shinyapps.io/omicsBioAnalytics/) on heart failure. 
The data sets are described [here](https://pubmed.ncbi.nlm.nih.gov/30935638/).



Lets train models on the **cells, holter, and protein** data sets to predict patient **hospitalization** using the **generalized linear model (GLM)** method.  

### Creating a `caret_list` object
```r
models <- caret_list(
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

TODO

## Project Structure

This project generally follows the [Tidyverse style guide](https://style.tidyverse.org/).  

### Naming Conventions  
- Internal (non-exported) functions are prefixed with `.` to hide them from the package namespace.  

### File Organization  
Each object’s definition and its associated methods are contained within a single file.  

- **`caret_list.R`** – Defines the `caret_list` object and its methods.  
- **`caret_stack.R`** – Defines the `caret_stack` object and its methods.  
- **`helpers.R`** – Contains internal helper functions shared across multiple objects.


