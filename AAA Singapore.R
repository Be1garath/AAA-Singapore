#Load Libraries  -------------

library(Rblpapi)
library(AZAT)
library(shiny)

# library(xts)
# library(tawny) #XCovariance Estimations
# library(rrcov) #For Square Root of Matrix in Meucci

#Download from BBG --------------------------------------------


#Load securities tickers
securities <- c("NDUECAXJ Index",  "MSEIXAUN Index", "NDDUUS Index", "NDDUE15 Index", 
                "LEGATRUH Index", "HW00 Index", "BCOMTR Index")

securities.names <- c("Asia Equities", "EM ex Asia Equities",  "US Equities",  "EU Equities" ,  
                      "Global IG Fixed Income" , "Global HY Fixed Income", "Commodities")

NumOfAssets <- length(securities)

#Initialise fields to download
fields <- c("PX_LAST")
fieldsName <- c("Close")

#Initialise start date and end date of download
startdate <- as.Date("2001-06-01")
enddate <- as.Date(Sys.Date())

#Download from BBG
conn <- blpConnect()
BBGData <- bdh(con=conn, 
               securities=paste(securities), 
               fields=fields, 
               start.date=startdate, 
               end.date=enddate, 
               options= c("nonTradingDayFillOption"="NON_TRADING_WEEKDAYS",
                          "nonTradingDayFillMethod"="PREVIOUS_VALUE",
                          "currency"="USD"), 
               verbose = F)
blpDisconnect(conn)

VarLst <- list()

for(iBBG in 1:length(securities)){
  BBGData2 <- BBGData[[paste(securities[iBBG])]]
  BBGData2 <- as.xts(BBGData2[,-1], order.by =as.Date(BBGData2$date))
  names(BBGData2) <- paste(securities[iBBG],".",fieldsName, sep="")
  VarLst[[length(VarLst)+1]] <- BBGData2
  # names(VarLst[[iBBG]]) <- securities[iBBG]
}

for(jBBG in 1:length(fields)){
  
  for(iBBG in 1:length(securities)){
    dummy <- as.xts(VarLst[[iBBG]])
    dummy <- dummy[,jBBG]
    names(dummy) <- paste(securities[iBBG],".",fieldsName[jBBG], sep="")
    if(iBBG==1){
      assign(paste("PriceLst.",fieldsName[jBBG],sep=""),dummy)
    } else {
      dummy2 <- get(paste("PriceLst.",fieldsName[jBBG],sep=""))
      dummy2 <- cbind(dummy2,dummy)
      assign(paste("PriceLst.",fieldsName[jBBG],sep=""),dummy2)
    }
  }
}

#Select Matrix

PriceLst <- PriceLst.Close
dim(PriceLst)
# head(PriceLst, 20)

PriceLst <- na.omit(PriceLst)
dim(PriceLst)/255

PriceLst.lag <- PriceLst[endpoints(PriceLst[],on="weeks", k=1)]

PriceLst.ret <- na.omit((PriceLst.lag/lag(PriceLst.lag))-1)
PriceLst.ret <- cbind(PriceLst.ret,
                      matrix(
                        rep(0.05/52,nrow(PriceLst.ret)), #alternatives
                        ncol=1
                      ),
                      matrix(
                        rep(0.01/52,nrow(PriceLst.ret)), #cash
                        ncol=1
                      )
)

table.AnnualizedReturns(PriceLst.ret)

#### Global Parameters --------
BL.tau <- 0.05 #to be used across all optimisation and construction of BL.P, BL.Q and BL.Omega
RiskAversion.SAA <- 5  #central risk aversion to perform ERFC and for construction of BL.P, BL.Q and BL.Omega
Securities.names <- c("Asia.EQ","EMxAsia.EQ", "US.EQ", "EU.EQ", "G.Agg","HY","Comdty","Alts")

#estimation of Covariances at different frequency
COVS.monthly <- AZAT_COV_get(tail(PriceLst,3500), "monthly")
COVS.weekly <- AZAT_COV_get(tail(PriceLst,200), "weekly") # (to be reviewed) use only last 200 days

#addition of HF asset class with assumption of zero corrrelation with other classes
NumOfAssets <- nrow(COVS.monthly[[1]])+1

Covmat.SAA <- diag(ncol=nrow(COVS.monthly[[1]])+1,nrow=nrow(COVS.monthly[[1]])+1)*0.15^2  #this last one is the var of Hedge Funds
Covmat.SAA[1:nrow(COVS.monthly[[1]]),1:nrow(COVS.monthly[[1]])] <- COVS.monthly [[1]]
Cormat.SAA <- cov2cor(Covmat.SAA)

Covmat.TAA <- diag(ncol=nrow(COVS.weekly[[1]])+1,nrow=nrow(COVS.weekly[[1]])+1)*0.15^2  #this last one is the var of Hedge Funds
Covmat.TAA[1:nrow(COVS.weekly[[1]]),1:nrow(COVS.weekly[[1]])] <- COVS.weekly [[1]]
Cormat.TAA <- cov2cor(Covmat.TAA)

#Meucci Procedure for Equal risk contribution for SAA
MeucciPlaceholder.SAA <- AZAT_MEUCCI_erfc(Covmat.SAA, Cormat.SAA, diag(Covmat.SAA)^0.5)
MeucciPlaceholder.TAA <- AZAT_MEUCCI_erfc(Covmat.TAA, Cormat.TAA, diag(Covmat.TAA)^0.5)

#Mcap Views
BL.P <- diag(rep(1,NumOfAssets))
BL.conf <- rep(0,NumOfAssets)  #NOTE: currenet confidence set to zero

BL.Mcap <- matrix(c(0.221, #1. AUS EQ
                    0.232, #2. AUS FI
                    0.042, #3. ROW EQ
                    0.116, #4. US EQ
                    0.053, #5. EU EQ
                    0.042, #6. EM EQ
                    0.126, #7. Global Agg
                    0.032), #8. Alts
                  ncol=1)
BL.Mcap <- BL.Mcap/sum(BL.Mcap)

BL.Q <- Covmat.SAA %*% BL.Mcap * RiskAversion.SAA

#RA Views
BL.P <- cbind(BL.P,diag(rep(1,NumOfAssets)))
BL.conf2 <- c(0.5, # 1. Asia Equity
              0.5, # 2. EM Equity
              0.5, # 3. US Equity
              0.5, # 4. EU Equity
              0.5, # 5. Global Agg
              0.5, # 6. HY
              0.5, # 7. Commodities
              0.3) #10. Alts

#Views expectations
# updated on 12 jan 2017

FX.differenetial <- 0

BL.Q <- rbind(BL.Q,
              matrix(c(0.075-FX.differenetial, # 1. Asia Equity
                       0.075-FX.differenetial, # 2. EM Equity
                       0.008-FX.differenetial, # 3. US Equity
                       0.058-FX.differenetial, # 4. EU Equity
                       0.006-FX.differenetial, # 5. Global Agg
                       0.022-FX.differenetial, # 6. HY
                       0.020-FX.differenetial, # 7. Commodities
                       0.1-FX.differenetial),ncol=1) #10. Alts
)

#add three views (Shiller PE)
BL.P <- cbind(BL.P,c(1,0,0,0,0,0,0,0)) #Asia Equity
BL.P <- cbind(BL.P,c(0,0,1,0,0,0,0,0)) #US Equities Shiller
BL.P <- cbind(BL.P,c(0,0,0,1,0,0,0,0)) #EU Equities Shiller

BL.Q <- rbind(BL.Q,
              matrix(c(0.0700, # 1. Asia Equity
                       0.024-FX.differenetial, # 4. US Equity Shiller
                       0.071-FX.differenetial),ncol=1)  # 5. EU Equities Shiller
)

#View uncertainty for SAA
BL.Omega.SAA <- diag(c(diag(Covmat.SAA)^0.5*BL.tau*(1/(BL.conf+0.00000001)-1),
                   diag(Covmat.SAA)^0.5*BL.tau*(1/(BL.conf2+0.00000001)-1),
                   BL.tau*0.0551,BL.tau*0.0825,BL.tau*0.217))

BL.Omega.TAA <- diag(c(diag(Covmat.TAA)^0.5*BL.tau*(1/(BL.conf+0.00000001)-1),
                       diag(Covmat.TAA)^0.5*BL.tau*(1/(BL.conf2+0.00000001)-1),
                       BL.tau*0.0551,BL.tau*0.0825,BL.tau*0.217))


#Generic Constraint Matrix (left hand side) - NOTE: in case the constraints differ it is necessary to create one ad hoc per portfolio
Amat <- matrix(rep(1,NumOfAssets),ncol=1)
Amat <- cbind(Amat,matrix(rep(-1,NumOfAssets),ncol=1))
Amat <- cbind(Amat,diag(1,NumOfAssets,NumOfAssets))
Amat <- cbind(Amat,diag(-1,NumOfAssets,NumOfAssets))
Amat <- cbind(Amat,c(0,0,0,0,1,1,0,0)) #min FI
Amat <- cbind(Amat,c(0,0,0,0,-1,-1,0,0)) #max FI
Amat <- cbind(Amat,c(1,1,0,0,0,0,0,0)) #min EM
Amat <- cbind(Amat,c(-1,-1,0,0,0,0,0,0)) #max EM
Amat <- cbind(Amat,c(0,0,1,1,0,0,0,0)) #min DM
Amat <- cbind(Amat,c(0,0,-1,-1,0,0,0,0)) #max DM
Amat <- cbind(Amat,c(0,0,0,0,0,0,1,1)) #min Alts
Amat <- cbind(Amat,c(0,0,0,0,0,0,-1,-1)) #min Alts


### Diversified Fund -----------

#Diversified Risk Aversion
RiskAversion.Div <- 5.5

#Diversified Constraints (right hand side)
bvec.Div <- c(0.7,-1, #min and max allocation for overall allocation
              rep(0.025,NumOfAssets), #max allocation for single assets
              rep(-0.25,NumOfAssets),
              0.25, -0.5,
              0.1,-0.25,
              0.1,-0.5,
              0.1,-0.3) 


#Portfolio Optimisation SAA
Portfolio.BL.Constr.SAA.Div <- AZAT_BL_Optimisation(Covmat.SAA,
                                             MeucciPlaceholder.SAA[[2]],
                                             BL.P, BL.Omega.SAA, BL.Q,
                                             Amat,bvec.Div,
                                             BL.tau=BL.tau,
                                             Securities.names=Securities.names,
                                             RiskAversion.EV = RiskAversion.SAA,
                                             RiskAversion.Optim = RiskAversion.Div)

Portfolio.BL.Constr.TAA.Div <- AZAT_BL_Optimisation(Covmat.TAA,
                                                    MeucciPlaceholder.TAA[[2]], #NOTE: to review if to use the ERFC TAA or SAA
                                                    BL.P, BL.Omega.TAA, BL.Q,
                                                    Amat,bvec.Div,
                                                    BL.tau=BL.tau,
                                                    Securities.names=Securities.names,
                                                    RiskAversion.EV = RiskAversion.SAA,
                                                    RiskAversion.Optim = RiskAversion.Div)

Portfolio.Div <- AZAT_BL_Optimisation_gen_outputs_Singapore(Portfolio.BL.Constr.SAA.Div,
                                 Portfolio.BL.Constr.TAA.Div,
                                 MeucciPlaceholder.SAA,
                                 MeucciPlaceholder.TAA,
                                 "Diversified Fund")

###Conservative ------------

#Conservative Risk Aversion
RiskAversion.Cons <- 8

#Conservative Constraints (right hand side)
bvec.Cons <- c(0.6,-0.95, #min and max allocation for overall allocation
               rep(0.025,NumOfAssets), #max allocation for single assets
               rep(-0.25,NumOfAssets),
               0.25, -0.75,
               0,-0.20,
               0.15,-0.5,
               0,-0.3) 

#Portfolio Optimisation SAA
Portfolio.BL.Constr.SAA.Cons <- AZAT_BL_Optimisation(Covmat.SAA,
                                                    MeucciPlaceholder.SAA[[2]],
                                                    BL.P, BL.Omega.SAA, BL.Q,
                                                    Amat,bvec.Cons,
                                                    BL.tau=BL.tau,
                                                    Securities.names=Securities.names,
                                                    RiskAversion.EV = RiskAversion.SAA,
                                                    RiskAversion.Optim = RiskAversion.Cons)

Portfolio.BL.Constr.TAA.Cons <- AZAT_BL_Optimisation(Covmat.TAA,
                                                    MeucciPlaceholder.TAA[[2]], #NOTE: to review if to use the ERFC TAA or SAA
                                                    BL.P, BL.Omega.TAA, BL.Q,
                                                    Amat,bvec.Cons,
                                                    BL.tau=BL.tau,
                                                    Securities.names=Securities.names,
                                                    RiskAversion.EV = RiskAversion.SAA,
                                                    RiskAversion.Optim = RiskAversion.Cons)

Portfolio.Cons <- AZAT_BL_Optimisation_gen_outputs_Singapore(Portfolio.BL.Constr.SAA.Cons,
                                                  Portfolio.BL.Constr.TAA.Cons,
                                                  MeucciPlaceholder.SAA,
                                                  MeucciPlaceholder.TAA,
                                                  "Conserviative Fund")
### Growth -----------

#Growth Risk Aversion
RiskAversion.Grow <- 3

#Growth Constraints (right hand side)
bvec.Grow <- c(0.85,-1, #min and max allocation for overall allocation
               rep(0.025,NumOfAssets), #max allocation for single assets
               rep(-0.25,NumOfAssets),
               0.1, -0.5,
               0.15,-0.5,
               0.15,-0.5,
               0.1,-0.35) 

#Portfolio Optimisation SAA
Portfolio.BL.Constr.SAA.Grow <- AZAT_BL_Optimisation(Covmat.SAA,
                                                     MeucciPlaceholder.SAA[[2]],
                                                     BL.P, BL.Omega.SAA, BL.Q,
                                                     Amat,bvec.Grow,
                                                     BL.tau=BL.tau,
                                                     Securities.names=Securities.names,
                                                     RiskAversion.EV = RiskAversion.SAA,
                                                     RiskAversion.Optim = RiskAversion.Grow)

Portfolio.BL.Constr.TAA.Grow <- AZAT_BL_Optimisation(Covmat.TAA,
                                                     MeucciPlaceholder.TAA[[2]], #NOTE: to review if to use the ERFC TAA or SAA
                                                     BL.P, BL.Omega.TAA, BL.Q,
                                                     Amat,bvec.Grow,
                                                     BL.tau=BL.tau,
                                                     Securities.names=Securities.names,
                                                     RiskAversion.EV = RiskAversion.SAA,
                                                     RiskAversion.Optim = RiskAversion.Grow)

Portfolio.Grow <- AZAT_BL_Optimisation_gen_outputs_Singapore(Portfolio.BL.Constr.SAA.Grow,
                                                  Portfolio.BL.Constr.TAA.Grow,
                                                  MeucciPlaceholder.SAA,
                                                  MeucciPlaceholder.TAA,
                                                  "Growth Fund")



#### Portafogli ------------

Portfolio.Cons[[5]]
Portfolio.Div[[5]]
Portfolio.Grow[[5]]

list.to.save <- list(Portfolio.Div,Portfolio.Cons,Portfolio.Grow)
name.of.file <- paste("Report/",format(Sys.time(),"%Y%m%d-%H%M%S"), sep="")

saveRDS(list.to.save, 
        file=paste(name.of.file,"-AAA-AZ-Athenaeum.rda",sep=""))
    
# asdkjhaks <- readRDS("Report/20170125-190002 AAA AZ Sestante.rda")


rmarkdown::render("Report.Rmd",
                  output_dir = "Report",
                  output_file = paste(name.of.file,"-AAA-AZ-Athenaeum.html", sep=""))


Portfolio.Cons$`Compact Lists`$EV
Portfolio.Div$`Compact Lists`$EV
Portfolio.Grow$`Compact Lists`$EV





(0.05/seq(2:10))^0.5*(7)^0.5*qnorm(0.95,0,1)-0.05*7


(0.05/seq(2:10))^0.5*(7)^0.5*qnorm(0.95,0,1)+0.05*7

ret.est <- 0.05
risk.prof <- 15

plot(ret.est*seq(1:20)-(ret.est/risk.prof)^0.5*(seq(1:20))^0.5*qnorm(0.95,0,1),
     t="l",
     ylim=c(-0.2,2))
lines(ret.est*seq(1:20)+(ret.est/risk.prof)^0.5*(seq(1:20))^0.5*qnorm(0.95,0,1))
lines(ret.est*seq(1:20))
lines(0.0*seq(1:20))





