library(shiny)
library(AZAT)
library(PerformanceAnalytics)

# Define UI for application
ui <- fluidPage(
  
  # theme = "cosmo.css",
  # 
  # includeCSS("www/cosmo.css"),
  
  # Application title
  titlePanel("Portfolio Optimisation"),
  
  # build 3 columns, 2 for sliders and one for pie      
  
  fluidRow(
    
    column(width = 12,
    sliderInput(inputId = "RiskProf",
                label = "Risk Profile",
                min = 1,
                max = 10,
                value = 5,
                step = 0.5)
    )
    ),
  h4("Original Portfolio Weights"),
  fluidRow(
    column(width = 12,
    splitLayout(cellWidths = rep("12%",NumOfAssets),
                sliderInput(inputId = "W1",
                            label = "Asia Equities (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                # selectInput(inputId = "W1",
                #             label = "Asia Equities (%)",
                #             choices = seq(from = 0,to = 1,by=0.005),
                #             selected = 0.125,
                #             multiple = FALSE
                sliderInput(inputId = "W2",
                            label = "EM Equities (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                sliderInput(inputId = "W3",
                            label = "US Equities (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                sliderInput(inputId = "W4",
                            label = "EU Equities (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                sliderInput(inputId = "W5",
                            label = "IG FI (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                sliderInput(inputId = "W6",
                            label = "HY FI (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                sliderInput(inputId = "W7",
                            label = "Commodities (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005),
                sliderInput(inputId = "W8",
                            label = "Alternatives (%)",
                            min = 0,
                            max = 1,
                            value = 0.125,
                            step = 0.005)
      
    ))
  ),
  h4("Asset Class Expected Returns"),
  fluidRow(
    column(width = 12,
    splitLayout(cellWidths = rep("12%",NumOfAssets),
        sliderInput(inputId = "EV1",
                     label = "Asia Equities (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV2",
                     label = "EM Equities (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV3",
                     label = "US Equities (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV4",
                     label = "EU Equities (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV5",
                     label = "IG FI (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV6",
                     label = "HY FI (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV7",
                     label = "Commodities (%)",
                     min = -10,
                     max = 10,
                     value = 0),
         sliderInput(inputId = "EV8",
                     label = "Alternatives (%)",
                     min = -10,
                     max = 10,
                     value = 0)
  ))
  ),
  h4("Asset Class Expected Returns Confidence"),
  fluidRow(
    column(width = 12,
    splitLayout(cellWidths = rep("12%",NumOfAssets),
         
         sliderInput(inputId = "Conf1",
                     label = "Asia Equities (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf2",
                     label = "EM Equities (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf3",
                     label = "US Equities (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf4",
                     label = "EU Equities (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf5",
                     label = "IG FI (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf6",
                     label = "HY FI (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf7",
                     label = "Commodities (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
         sliderInput(inputId = "Conf8",
                     label = "Alternatives (%)",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05)
  ))
  ),
  
  br(),
  fluidRow(
    column(width = 12,
    splitLayout(cellWidths = c("25%","25%","50%"),
           plotOutput("distPlotOriginal"),
           plotOutput("distPlotPort"),
           column(width = 4,
                  fluidRow(
                    tableOutput("table1")
                  ),
                  fluidRow(
                    tableOutput("table.ret")
                  )
           )
         )
    )
    ),
  
  fluidRow(
   column(width = 12,
           plotOutput("simulation",
                      height = "600px"
                      ))
  ),
  
  fluidRow(
    column(width = 12,
           splitLayout(cellWidths = c("25%","25%","50%"),
                       plotOutput("distPlotFactOriginal"),
                       plotOutput("distPlotFact"),
                       plotOutput("histo",height = "400px")
                       
           )
    )
  ),
  
  fluidRow(
    column(width=6,
           fluidRow(tableOutput("table.DD.original")), #note the error in format is from this
           fluidRow(tableOutput("table.DD.optimal")),
           tableOutput("table.CAPM")
    )
  )
)
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  BL.P2 <- cbind(BL.P,diag(rep(1,NumOfAssets)))
  BL.conf3 <- reactive({
    c(input$Conf1, # 1. Asia Equity
      input$Conf2, # 2. EM Equity
      input$Conf3, # 3. US Equity
      input$Conf4, # 4. EU Equity
      input$Conf5, # 5. Global Agg
      input$Conf6, # 6. HY
      input$Conf7, # 7. Commodities
      input$Conf8) #8. Alts
  })
  BL.Q2 <- reactive({
    rbind(BL.Q,
          matrix(c(input$EV1/100, # 1. Asia Equity
                   input$EV2/100, # 2. EM Equity
                   input$EV3/100, # 3. US Equity
                   input$EV4/100, # 4. EU Equity
                   input$EV5/100, # 5. Global Agg
                   input$EV6/100, # 6. HY
                   input$EV7/100, # 7. Commodities
                   input$EV8/100),ncol=1)) #10. Alts
  })
  BL.Omega.SAA2 <- reactive({
    diag(c(diag(Covmat.SAA)^0.5*BL.tau*(1/(BL.conf+0.00000001)-1),
           diag(Covmat.SAA)^0.5*BL.tau*(1/(BL.conf2+0.00000001)-1),
           BL.tau*0.0551,BL.tau*0.0825,BL.tau*0.217,
           diag(Covmat.SAA)^0.5*BL.tau*(1/(BL.conf3()+0.00000001)-1))
    )
  })
  bvec.Shiny <- c(0.7,-1, #min and max allocation for overall allocation
                  rep(0.00,NumOfAssets-2),0,0, #max allocation for single assets
                  rep(-0.35,NumOfAssets-4),-0.3,-0.01,-0.35,-0.35,
                  0.00, -0.4,
                  0.00,-0.3,
                  0.00,-0.85,
                  0.19,-0.2) 
  RiskAversion.Optim2 <- reactive({input$RiskProf})
  Portfolio.BL.Constr.SAA.Shiny <- reactive({
    AZAT_BL_Optimisation(Covmat.SAA,
                         MeucciPlaceholder.SAA[[2]],
                         BL.P2, BL.Omega.SAA2(), BL.Q2(),
                         Amat,bvec.Shiny,
                         BL.tau=BL.tau,
                         Securities.names=Securities.names,
                         RiskAversion.EV = RiskAversion.SAA,
                         RiskAversion.Optim = RiskAversion.Optim2())
  })
  Portfolio.BL.Constr.Factors <- reactive({
    (solve(MeucciPlaceholder.SAA[[1]]) %*% Portfolio.BL.Constr.SAA.Shiny()[[1]])^2 * diag(t(MeucciPlaceholder.SAA[[1]]) %*% Covmat.SAA %*% MeucciPlaceholder.SAA[[1]]) / sum((solve(MeucciPlaceholder.SAA[[1]]) %*% Portfolio.BL.Constr.SAA.Shiny()[[1]])^2 * diag(t(MeucciPlaceholder.SAA[[1]]) %*% Covmat.SAA %*% MeucciPlaceholder.SAA[[1]]))
  })
  
  Portfolio.BL.Constr.FactorsOriginals <- reactive({
    vartoplot2 <- matrix(c(input$W1,input$W2,input$W3,input$W4,input$W5,input$W6,input$W7,input$W8),
                         ncol=1)
    (solve(MeucciPlaceholder.SAA[[1]]) %*% vartoplot2)^2 * diag(t(MeucciPlaceholder.SAA[[1]]) %*% Covmat.SAA %*% MeucciPlaceholder.SAA[[1]]) / sum((solve(MeucciPlaceholder.SAA[[1]]) %*% vartoplot2)^2 * diag(t(MeucciPlaceholder.SAA[[1]]) %*% Covmat.SAA %*% MeucciPlaceholder.SAA[[1]]))
  })
  
  output$distPlotPort <- renderPlot({
    
    pie(c(Portfolio.BL.Constr.SAA.Shiny()[[1]],max(1-sum(Portfolio.BL.Constr.SAA.Shiny()[[1]]),0)), 
        labels = c(rownames(Portfolio.BL.Constr.SAA.Shiny()[[1]]),"Cash"),
        main="Optimal Portfolio")
  })
  
  output$distPlotFact <- renderPlot({
    
    pie(Portfolio.BL.Constr.Factors(), 
        labels = paste("Fact_",seq(1:NumOfAssets),sep=""),
        main="Original Factors Risks")
  })
  
  output$distPlotFactOriginal <- renderPlot({
    
    pie(Portfolio.BL.Constr.FactorsOriginals(), 
        labels = paste("Fact_",seq(1:NumOfAssets),sep=""),
        main="Optimal Factors Risks")
  })
  
  output$distPlotOriginal <- renderPlot({

    # vartoplot <- c(get(paste("input$w",seq(1:NumOfAssets),sep="")))

    vartoplot <- c(input$W1,input$W2,input$W3,input$W4,input$W5,input$W6,input$W7,input$W8)
    vartoplot <- c(vartoplot, 1-sum(vartoplot))
    
    pie(vartoplot,
        labels = c(rownames(Portfolio.BL.Constr.SAA.Shiny()[[1]]),"Cash"),
        main="Original Portfolio")
  })
  
  output$table1 <- renderTable({
    
    vartoplot <- matrix(c(input$W1,input$W2,input$W3,input$W4,input$W5,input$W6,input$W7,input$W8),ncol=1)
    # vartoplot <- c(vartoplot, 1-sum(vartoplot))
    
    tab.to.print <- matrix(
      cbind(
      Portfolio.BL.Constr.SAA.Div[[4]],
      Portfolio.BL.Constr.SAA.Shiny()[[4]],
      vartoplot,
      Portfolio.BL.Constr.SAA.Shiny()[[1]]
    ),
    ncol = 4
    )
    colnames(tab.to.print) <- c("Prior EV", "Post EV", "Orig Pfolio", "Opt Pfolio")
    rownames(tab.to.print) <- rownames(Portfolio.BL.Constr.SAA.Div[[1]])
    round(tab.to.print*100,1)
  },
  rownames = TRUE,
  colnames = TRUE,
  hover = TRUE,
  bordered = FALSE)
  
  output$table.ret <- renderTable({
    table.AnnualizedReturns(ReturnSimulation())
  },
  rownames = TRUE,
  colnames = TRUE,
  hover = TRUE,
  bordered = FALSE)
  
  output$table.CAPM <- renderTable({
    table.CAPM(ReturnSimulation()[,2],ReturnSimulation()[,1])
  },
  rownames = TRUE,
  colnames = TRUE,
  hover = TRUE,
  bordered = FALSE)
  
  output$table.DD.original <- renderTable({
    table.Drawdowns(ReturnSimulation()[,1])
  },
  rownames = TRUE,
  colnames = TRUE,
  hover = TRUE,
  bordered = FALSE)  

  output$table.DD.optimal <- renderTable({
    table.Drawdowns(ReturnSimulation()[,2])
  },
  rownames = TRUE,
  colnames = TRUE,
  hover = TRUE,
  bordered = FALSE)
  
  ReturnSimulation <- reactive({
    vartoplot <- c(input$W1,input$W2,input$W3,input$W4,input$W5,input$W6,input$W7,input$W8)
    vartoplot <- c(vartoplot, 1-sum(vartoplot))
    
    Port.ret.PortOriginal <- Return.portfolio(PriceLst.ret, 
                                              weights = c(vartoplot), 
                                              wealth.index = FALSE,
                                              contribution = FALSE, 
                                              geometric = TRUE, 
                                              rebalance_on = c("quarters"), 
                                              value = 1, 
                                              verbose = FALSE)
    
    Port.ret.Port <- Return.portfolio(PriceLst.ret, 
                                      weights = c(Portfolio.BL.Constr.SAA.Shiny()[[1]],1-sum(Portfolio.BL.Constr.SAA.Shiny()[[1]])), 
                                      wealth.index = FALSE,
                                      contribution = FALSE, 
                                      geometric = TRUE, 
                                      rebalance_on = c("quarters"), 
                                      value = 1, 
                                      verbose = FALSE)
    
    stufftoprint2 <- cbind(
      Port.ret.PortOriginal,
      Port.ret.Port,
      Port.ret.Port*max(min(sd(Port.ret.PortOriginal)/sd(Port.ret.Port),2),1)-max((min(sd(Port.ret.PortOriginal)/sd(Port.ret.Port),2)-1),0)*0.015/52
    )
    
    colnames(stufftoprint2) <- c("Original Portfolio","Optimal Portfolio","Leveraged Portfolio")
    
    stufftoprint <- stufftoprint2
  })
  
  output$simulation <- renderPlot({
    charts.PerformanceSummary(ReturnSimulation(),
                              wealth.index = TRUE,
                              main = "Portfolios Performance - Leveraged portfolio assumed @ 1.5% cost and maxed at 50% LTV",
                              cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=2)
  })
  
  output$histo <- renderPlot({
    p_min <- min(ReturnSimulation())
    p_max <- max(ReturnSimulation())
    p1 <- hist(ReturnSimulation()[,1], breaks = seq(from = p_min-0.01, to = p_max+0.01, by=0.0025))
    p2 <- hist(ReturnSimulation()[,2], breaks = seq(from = p_min-0.01, to = p_max+0.01, by=0.0025))
    p3 <- hist(ReturnSimulation()[,3], breaks = seq(from = p_min-0.01, to = p_max+0.01, by=0.0025))
    # d_min <- min(c(p1$counts,p2$counts))
    d_max <- max(c(p1$density,p2$density),p3$density)
    plot(p1, col=rgb(0,0,0,1/4), xlim=c(p_min-0.01,p_max+0.01), ylim = c(0,d_max*1.1), freq=FALSE, main="Histogram of Returns")  
    plot(p2, col=rgb(1,0,0,1/4), xlim=c(p_min-0.01,p_max+0.01), ylim = c(0,d_max*1.1), freq=FALSE, add=T)
    plot(p3, col=rgb(0,1,0,1/4), xlim=c(p_min-0.01,p_max+0.01), ylim = c(0,d_max*1.1), freq=FALSE, add=T)
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)

