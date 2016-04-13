shinyUI(fluidPage(
  h3("Central limit theorem demonstration", align="center"),
  hr(),
  sidebarLayout(
    sidebarPanel(width=3,
                 
                 
                 # Inputs
                 
                 # continuous or discrete?
                 radioButtons(inputId = "cat",label = "Distribution type",
                              choices=list("continuous"="continuous",
                                           "discrete"="discrete",
                                           "my own"="mine"),
                              selected="continuous"),
                 
                 # dstribution choice
                 
                 # continuous distributions
                 conditionalPanel(
                   condition="input.cat=='continuous'",
                   selectInput(inputId="dist_cont", 
                               label="Choose a distribution",
                               choices = list("norm",
                                              "unif",
                                              "chisq"),
                               selected = "unif")),
                 
                 # discrete distributions
                 conditionalPanel(
                   condition="input.cat=='discrete'",
                   selectInput(inputId="dist_disc", 
                               label="Choose a distribution",
                               choices = list("binom"),
                               selected = "binom")),
                 hr(),
                 
                 # parameters for the distributions (dynamic)
                 h5(strong("Parameters of the distribution")),
                 br(),
                 uiOutput("parameters"),
                 
                 hr(),
                 
                 # sample size choice
                 numericInput(inputId = "n",
                              label="sample size",
                              value=20, min=2),
                 br(),
                 
                 # means or medians?
                 radioButtons(inputId = "stat", label="sampling statistic",
                              choices=list(means = "means", medians = "medians"),
                              selected="means")
    ),
    
    
    # Outputs
    
    mainPanel(
      p("Welcome! Start by choosing a distribution and a sample size 
        on the left hand panel. You can go ahead and create your own distribution if you wish. 
        Random samples of the chosen distributions are then drawn. A smooth, histogram-like estimate of the distribution (based on a single sample) is then plotted on the left. 
        On the right, the sampling distribution of the means or the medians are plotted similarily, along with a corresponding normal approximation. 
        You can use the Infinitize slider to select the number of times a single sample is drawn."),
      
      br(),
      
      fluidRow(
        
        column(9,offset=3,
               
               # number of samples
               sliderInput(inputId = "iter",
                           label="Infinitize! (number of samples)",
                           min=2,max=500,value=2,step=2)
        )
      ),
      
      br(),
      
      textOutput("infotext"),
      
      fluidRow(
        column(6,
               
               # plot of a single sample
               plotOutput("singleplot", click = "plot_click"),
               uiOutput("view")),
        column(6,
               
               #plot of the sample statictis
               plotOutput("meanplot"),
               textOutput("stats"))),
      
      fluidRow(
        column(6,
               
               # table of a single sample
               tableOutput("head_sample")),
        column(6,
               
               # means or medians?
               tableOutput("head_stats"))))
    )
  ))