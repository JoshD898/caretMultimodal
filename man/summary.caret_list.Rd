% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caret_list.R
\name{summary.caret_list}
\alias{summary.caret_list}
\title{Summarize a caret_list}
\usage{
\method{summary}{caret_list}(caret_list, metric = NULL)
}
\arguments{
\item{caret_list}{a caret_list object}

\item{metric}{The metric to show. If NULL will use the metric used to train each model}
}
\value{
A data.table with metrics from each model.
}
\description{
This function summarizes the performance of each model in a caret_list object.
}
