
# caretMultimodal

CaretMultimodal is a wrapper around the [caret] package that allows for simplified 
multi-dataset training and ensembling. It is heavily inspired by Zach Mayer's 
[caretEnsemble](https://github.com/zachmayer/caretEnsemble) package.

To Do list:


## Project Structure






The "Helpers.R" file contains helper functoins that are used in 2 or more other files.


## caret_list example

For the following examples, we will [these publicly available data sets](https://amritsingh.shinyapps.io/omicsBioAnalytics/) on heart failure. 
The data sets are described [here](https://pubmed.ncbi.nlm.nih.gov/30935638/)

caret_list allows for users to train multiple models by only calling one function.


In this example, we will train models on the **cells, holter, and protein** data sets to predict patient **hospitalization** using the **generalized linear model (GLM)** method.  

### Creating a `caret_list` object
```r
models <- caret_list(
    target = demo$hospitalizations, 
    data_list = list(cells = cells, holter = holter, proteins = proteins), 
    method = "glm"
)
```

### `caret_list` methods

**NOTE** extract metric, summary and print summary are very very similar. Copied them over from caretEnsemble, but could cull them down.

##### predict()

Makes a matrix of predictions for each model in a caret list.

```r
predict(models)
```
Produces:

 cells_model holter_model proteins_model
 1: 3.082239e-02 1.512935e-06   2.220446e-16
 2: 9.971526e-01 2.220446e-16   1.000000e+00
 3: 8.656950e-01 2.220446e-16   1.000000e+00
 4: 4.392770e-02 9.993809e-01   1.000000e+00
 5: 7.850696e-02 2.220446e-16   1.000000e+00
 6: 9.969068e-01 1.000000e+00   1.000000e+00

##### extract_metric()

```r
extract_metric(models)
```
Produces:

            model method metric     value         sd
1:    cells_model    glm    ROC 0.5962963 0.08114408
2:   holter_model    glm    ROC 0.5037037 0.15843889
3: proteins_model    glm    ROC 0.5944444 0.13833222


##### summary() / print(summary())

```r
summary <- summary(models)
print(summary)
```
Produces:
The following models were trained: cells_model, holter_model, proteins_model 

Model metrics:
            model method metric     value         sd
           <char> <char> <char>     <num>      <num>
1:    cells_model    glm    ROC 0.5962963 0.08114408
2:   holter_model    glm    ROC 0.5037037 0.15843889
3: proteins_model    glm    ROC 0.5944444 0.13833222

##### plot()

```r
plot(models)
```
Produces:


