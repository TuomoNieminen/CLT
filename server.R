shinyServer(function(input, output, session) {
  
  # dynamic infotext
  output$infotext <- renderText({
    paste("Right now, you are drawing samples of",input$n,"observations each.",
          "You are drawing these samples", input$iter, "times, 
          for a total of",input$n*input$iter, "observations.")
  })
  
  
  # the chosen distribution
  distr <- reactive({
    switch(input$cat,
           "continuous" = input$dist_cont,
           "discrete" = input$dist_disc,
           "mine" = "mydist")
  })
  
  # update the personal distribution based on user mouseclicks
  # plotOutput() has the argument click = "plot_click"
  my <- reactiveValues(x=1:10, p=c(1:5,5:1)/(2*sum(1:5)))
  update_p <- observe({
    if(input$cat=="mine") {
      x <- input$plot_click$x
      y <- input$plot_click$y
      if(is.numeric(x)) {
        if(y < 1 & x < 10 & x > 0) {
          y <- max(0,y)
          x <- round(x)
          my$p[x] <- y
        }}}
  })
  
  # random sampler and density function of the personal distribution
  
  rmydist <- function(n, values=my$x, p=my$p) {
    p <- p/sum(p)
    sample(x = values, size = n, replace=T, prob = p)
  }
  dmydist <- function(x, values=my$x, p=my$p) {
    p
  }
  
  # the type of parameters of the chosen distribution
  
  paramtype <- reactive({
    if(distr() %in% c("norm")) "mu"
    else if(distr() %in% c("binom")) "p"
    else if(distr() %in% c("chisq","t")) "df"
    else if(distr() %in% c("unif")) "minmax"
    else if(distr() %in% c("mydist")) "mydist"
  })
  
  # dynamic UI parameter choice
  
  output$parameters <- renderUI({
    switch(paramtype(),
           "mu" = div(numericInput("mean","mean",value=5),
                      numericInput("sd","standard deviation",value=2, min=1)),
           "p" = div(sliderInput("p","probability of success",
                                 value=0.5, min=0.1,max=1,step=0.1),
                     numericInput("trials","# of trials", value=5,min=1)),
           "df" = numericInput("df","degrees of freedom",value=5, min=1),
           "minmax" = sliderInput("range","min and max",value=c(-2,5),min=-10,max=10),
           "mydist" = div(p("It is up to you to define the initial distribution!"),br(),
                          p("Click on the plot on the left to adjust the bars and change the shape of the distribution."))
    )
  })
  
  
  # parameters of the distribution (as a list for do.call)
  
  args <- reactive({
    switch(paramtype(),
           "mu" = list(n=input$n, mean=input$mean, sd=input$sd),
           "p" = list(n=input$n, size=input$trials, prob=input$p),
           "df" = list(n=input$n, df=input$df),
           "minmax" = list(n=input$n, 
                           min=as.numeric(input$range[1]),
                           max=as.numeric(input$range[2])),
           "mydist" = list(n=input$n, values=my$x, p = my$p))
  })
  
  
  # check if we are ready to sample
  
  readytoSample <- reactive({
    if(paramtype()=="minmax") {
      params_ready <- !is.null(input$range)
    } else{
      params_ready <- TRUE
    }
    have_samplesize <- !is.na(input$n) & input$n > 1
    args_ready <-  all(sapply(args(), function(a) !is.null(a)))
    params_ready & args_ready & have_samplesize
  })
  
  # draw samples of the chosen distribution
  
  samples <- reactive({
    if(readytoSample()) {
      s$readtoPlot <- F
      sapply(1:input$iter,function(i) {
        do.call(paste0("r",distr()), args=args())})}
  })
  
  # check if we have a sample
  
  haveSample <- reactive({
    if(is.null(samples())) FALSE
    else if(is.na(samples()[1])) FALSE
    else TRUE
  })
  
  # reactive list with values that will be computed from the samples
  
  s <- reactiveValues(samples =NULL, means = NULL, medians = NULL,
                      stats = NULL, sd_stats = NULL,
                      xlim = NULL, x_seq = NULL, 
                      selected =NULL,density=NULL,
                      readytoPlot=FALSE)
  
  
  
  # compute values from the samples and update the 's' list
  
  update_s <- observe({
    if(haveSample()) {
      s$df <- samples()
      s$means <- colMeans(s$df)
      s$medians <- apply(s$df,2,median)
      s$sd_means <- mean(sqrt(apply(s$df,2,var)/input$n))
      if(input$cat=="mine") {
        s$xlim <- c(1,10)
      } else {
        s$xlim <- quantile(s$df,c(0.001,0.999))
      }
      s$cont_seq <- seq(s$xlim[1],s$xlim[2],length=200)
      s$x_seq <- switch(input$cat,
                        "continuous" = s$cont_seq,
                        "discrete" = floor(s$xlim[1]):ceiling(s$xlim[2]),
                        "mine" = 1:10)
      s$selected <- data.frame(sample = input$view, values = s$df[,input$view])
      s$density <- data.frame(d = do.call(paste0("d",distr()), 
                                          c(list(x=s$x_seq),args()[-1])),
                              x=s$x_seq,
                              type="density")
      if(input$stat=="means") {
          s$stats <- s$means
          s$sd_stats <- s$sd_means
        } else {
         s$stats <-  s$medians
         s$sd_stats <- sqrt(var(s$medians))
        }
      s$readytoPlot <- TRUE
    }
  })
  
  
  
  # UI: which single sample to plot
  
  output$view <- renderUI({ # dynamic UI component
    numericInput(inputId = "view",
                 label="View a specific sample",
                 value=1,
                 min=1,max=input$iter)
  })
  
  
  
  # density estimate for the plot of the single sample
  
  sdens <- reactive({
    if(s$readytoPlot) {
      density(s$selected$values, kernel="gaussian", 
              from = s$xlim[1], to = s$xlim[2], 
              n=2^6)
    }
  })
  
  # plot a single sample
  
  output$singleplot <- renderPlot({
    if(s$readytoPlot) {
      
      # plot the kernel density
      plot(sdens(),
           ylim = c(0,1), xlim=s$xlim,
           xlab = paste("values from",distr()),
           main= paste("sample no.",input$view))
      
      # color the area inside
      denx <- sdens()$x; deny <- sdens()$y
      polygon(c(min(denx),denx,max(denx)),c(0,deny,0), 
              col = "grey92", border="cornflowerblue")
      
      #add lines describing the theoretical distr
      type <- ifelse(input$cat=="continuous", "l", "h")
      lwd <- ifelse(input$cat=="continuous", 1, 3)
      lines(s$x_seq, s$density$d, type=type, lwd=lwd, col="darkgreen")
      
      # add points corresponding to the sample values
      zeros = rep(0, input$n)
      points(x=s$selected$values, y=zeros, pch="|",col="red")
      
      # add legend
      legend("topright",legend=c("sampled values","distribution estimate","actual distribution"),
             col=c("red","cornflowerblue","darkgreen"),lwd=1)
    }
  })
  
  
  
  # density estimate for the plot of the sample means/medians
  
  mdens <- reactive({
    if(s$readytoPlot) {
      
      density(s$stats, kernel="rectangular", adjust=1.2)
    }
  })
  
  # plot the sample means
  
  output$meanplot <- renderPlot({
    if(s$readytoPlot) {
      
      # plot the kernel density
      plot(mdens(),
           ylim=c(0,2.5), xlim=s$xlim, lwd=1,
           xlab = paste("sample",input$stat),
           main=paste("Distribution of",input$iter,"sample",input$stat))
      
      # color the area inside
      polygon(mdens(), col="grey92",border="cornflowerblue")
      
      # add lines describing the theoretical distr (normal)
      y_fit <- dnorm(s$cont_seq, mean(s$stats),s$sd_stats)
      lines(s$cont_seq, y_fit,col="darkgreen", lwd=1)
      
      zeros <- rep(0, input$iter)
      # add points corresponding to the sample statistics
      points(x= s$stats, y=zeros,pch = "|",col = "red")
      
      # add legend
      legend("topright",legend=c(paste(input$stat),"distribution estimate","normal\napproximation"),
             col=c("red","cornflowerblue","darkgreen"),lwd=1)
    }
  })
  
  # stats of the sampling distribution
  
  output$stats <- renderText({
    if(s$readytoPlot) {
    text <- paste0("The mean of the sample ",input$stat," is ",
                   round(mean(s$stats),2),".")
    text <- paste0(text," The (estimated) standard deviation of the sample ",input$stat," is ",
                   round(s$sd_stats,2),".")
    text
    }
  })
  
  output$head_sample <- renderTable({
    if(s$readytoPlot) {
      n <- min(input$n, 6)
      head(s$selected,n=n)
    }
  })
  output$head_stats <- renderTable({
    n <- min(input$iter,6)
    X <- data.frame(sample=1:n)
    X[input$stat] <- s$stats[1:n]
    head(X, n =n)
  })
  
})