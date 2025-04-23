---
title: "River Closures at different temperature thresholds"
output:
  html_document: 
    keep_md: true
  word_document: default
  pdf_document: default
date: "2025-03-28"
authors: Emilie Geissinger, Curtis Pennell, Brianna Newton, Chelsea Bloom, and Keith Lewis
affiliation: Coastal and Freshwater Ecology
editor_options: 
  chunk_output_type: console
---







``` r
getwd()
```

```
## [1] "C:/Users/lewiske/Documents/CAFE/projects/temperature/river_closures/Rcode/data-summary"
```

``` r
here::here()
```

```
## [1] "C:/Users/lewiske/Documents/CAFE/projects/temperature/river_closures"
```

``` r
# this is just to show that relative paths need to change now that the directory has switched to the document
#partner.data <- read.csv("../../data-working/output/compiled-partner-data-2024.csv") |> 
 #  mutate(Serial = as.character(Serial)) 
```


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


``` r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

<img src="output/figures_closures/BiscayBayRiver.png" width="1737" /><img src="output/figures_closures/CampbelltonRiver.png" width="1889" /><img src="output/figures_closures/ExploitsRiver.png" width="2129" /><img src="output/figures_closures/RenewsRiver.png" width="1845" /><img src="output/figures_closures/SalmonierRiver.png" width="1764" /><img src="output/figures_closures/TerraNovaRiver.png" width="1887" />

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
