myData<-readxl::read_xlsx("C:/Users/SyedFar/Desktop/Response_curve.xlsx",sheet = "Sheet1")

Ufun<-function(x, Spend, Return) {
  predictedReturn = x[2] + (x[1] - x[2])*((Spend^x[3])/(x[4] + (Spend^x[3])))
  errorSq = (predictedReturn - Return)^2
  sumSqError = sum(errorSq)
  return(sumSqError)
}
startValVec = c(500,100,2.54324769178015,1000)
minValVec = c(0,0,1,0)
maxValVec = c(500000, 500000, 5, 10000000)


optim.parms<-nlminb(objective=Ufun,start = startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=100000,eval.max=2000),
                    Spend = Response_curve$Spend,
                    Return = Response_curve$Return)

optim.parms


a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(myData$Spend)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = curveDFx, Return = curveDFy)

maxX = 1.05*max(curveDFx, max(myData$Spend))
maxY = 1.05*max(curveDFy, max(myData$Return))

myPlotDataDF = data.frame(Return = myData$Return, Spend = myData$Spend)
optimLineDF = data.frame(Spend = curveDFx, Return = curveDFy)

scatterPlotPlusFit <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black", shape = 16) +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "darkgreen"))  +
  scale_color_manual(labels = "Optimized ADBUDG Fit",values=c('darkgreen')) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma) + 
  ggtitle(paste("Data & Model Fit", sep = " "))

scatterPlotPlusFit














