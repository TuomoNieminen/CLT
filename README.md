# CLTdemo
Central limit theorem demo (R shiny)

Shiny is an R library for creating interactive web applications. This  app demonstrates the Central Limit Theorem and sampling distributions 
by allowing the user to draw samples from different probability distributions and displaying the distribution of the sample means or sample medians. 

The is hosted [here](https://tuomonieminen.shinyapps.io/CLTdemo/) and can also be accessed locally from R:  

```
if(!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
runGitHub("CLTdemo", "TuomoNieminen")
```
