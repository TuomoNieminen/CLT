# CLT
Central limit theorem demo (R shiny)

Shiny is an R library for creating interactive web applications. This simple app demonstrated the Central Limit Theorem 
by allowing the user to draw samples from different probability distributions and displaying the distribution of the sample means. 

The app can be run from R:  

```
if(!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
runGitHub("CLT", "TuomoNieminen")
```
