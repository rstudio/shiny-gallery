---
title: "Title" 
author: "Author: Daniela Cassol (danielac@ucr.edu) and Thomas Girke (thomas.girke@ucr.edu)"
date: "Last update: `r format(Sys.time(), '%d %B, %Y')`" 
output:
  BiocStyle::html_document:
    toc_float: true
    code_folding: show
  BiocStyle::pdf_document: default
package: systemPipeR
vignette: |
  %\VignetteIndexEntry{RIBO-Seq Workflow Template}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
fontsize: 14pt
bibliography: bibtex.bib
---

<!--
Config css and r style
-->

```{css, echo=FALSE}
pre code {
white-space: pre !important;
overflow-x: scroll !important;
word-break: keep-all !important;
word-wrap: initial !important;
}
```

```{r style, echo = FALSE, results = 'asis'}
BiocStyle::markdown()
options(width=60, max.print=1000)
knitr::opts_chunk$set(
    eval=as.logical(Sys.getenv("KNITR_EVAL", "TRUE")),
    cache=as.logical(Sys.getenv("KNITR_CACHE", "TRUE")), 
    tidy.opts=list(width.cutoff=60), tidy=TRUE)
```

```{r setup, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE}
suppressPackageStartupMessages({
    library(systemPipeR)
})
```

# Introduction

## Load packages and sample data

The `systemPipeR` package needs to be loaded to perform the analysis 
steps shown in this report [@Girke2014-oy]. The package allows users
to run the entire analysis workflow interactively or with a single command 
while also generating the corresponding analysis report. For details
see `systemPipeR's` main [vignette](http://www.bioconductor.org/packages/devel/bioc/vignettes/systemPipeR/inst/doc/systemPipeR.html).

```{r load_systempiper, eval=TRUE, message=FALSE}
library(systemPipeR)
```

## Experiment definition provided by `targets` file

The `targets` file defines all FASTQ files and sample comparisons of the analysis workflow. 

```{r load_targets, eval=TRUE}
targetspath <- system.file("extdata", "targets.txt", package="systemPipeR")
targets <- read.delim(targetspath, comment.char = "#")[,1:4]
targets
```

# Version Information

```{r sessionInfo}
sessionInfo()
```

# Funding

# References
