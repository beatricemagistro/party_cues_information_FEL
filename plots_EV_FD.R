
### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Create figures 3 to 8 in the paper


## run multiple_imp, multiple_imp_no_matching, multiple_imp matching, and education_robust first

trace1b$sublabels <- ""
trace1bCEM$sublabels <- ""
trace2b$sublabels <- ""
trace2bCEM$sublabels <- ""
trace1bCEM$pch <- 15
trace2bCEM$pch <- 18
trace1b$pch <- 15
trace2b$pch <- 18
trace1bCEM$size <- 1
trace2bCEM$size <- 1.5
trace1b$size <- 1
trace2b$size <- 1.5
trace1b$col <- "black"
trace2b$col <- "black"
trace1bCEM$col <- "grey"
trace2bCEM$col <- "grey"
# figure 4
file <- "plot_EV_policy"
tile(trace1b, trace2b, trace1bCEM, trace2bCEM,
     limits = c(0.35,0.95),
     gridlines = list(type="xt"),
     xaxis=list(at=c(0.4, 0.5, 0.6, 0.7,0.8,0.9)),
     topaxis=list(add=TRUE, at=c(0.4, 0.5, 0.6,0.7,0.8,0.9)),
     xaxistitle=list(labels="probability"),
     topaxistitle=list(labels="probability"),
     plottitle=list(labels=c("Favor price controls")),
     width=list(plot=1),
     height=list(plottitle=2,xaxistitle=2.5,topaxistitle=2.5),
     output=list(outfile=file, width=7)
)


### first differences
trace1c$sublabels <- ""
trace1cCEM$sublabels <- ""
trace2c$sublabels <- ""
trace2cCEM$sublabels <- ""
trace1cCEM$pch <- 15
trace2cCEM$pch <- 18
trace1c$pch <- 15
trace2c$pch <- 18
trace1cCEM$size <- 1
trace2cCEM$size <- 1.5
trace1c$size <- 1
trace2c$size <- 1.5
trace1cCEM$col <- "grey"
trace2cCEM$col <- "grey"
trace1c$col <- "black"
trace2c$col <- "black"

vertmark <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.30, -0.20,-0.10,0,0.10)
xlab <- c("-30%","-20%","-10%", "0%", "+10%")
# figure 5
file <- "plot_FD_policy"
tile(trace1c, trace2c, trace1cCEM, trace2cCEM, vertmark, traceSig1, traceSig2,
     traceSig1CEM, traceSig2CEM,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     gridlines=list(type="xt"),
     #xaxistitle=list(labels="difference in probability approve"),
     topaxistitle=list(labels="difference in probability of favoring price controls"),
     #plottitle=list(labels="Approve of price controls"),
     width=list(plot=1),
     height=list(plottitle=2,xaxistitle=2.5,topaxistitle=2.5),
     output=list(outfile=file, width=6.75)
)

### First differences by literacy 

traceFD$size <- 1
traceFDCEM$size <- 1

traceFDCEM$col <- "grey"

traceFD$pch[1] <- 19
traceFD$pch[2] <- 19
traceFDCEM$pch[1] <- 19
traceFDCEM$pch[2] <- 19

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.30,-0.20,-0.10,0,0.10, 0.20,0.30)
xlab <- c("-30%","-20%","-10%", "0%","+10%", "+20%", "+30%")

# figure 6
# Make plot with tile
file <- "plot_FD_lit"
tile(traceFD,traceFDCEM, FDtraceSig1, FDtraceSig1CEM, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     #plottitle=list(labels="High literacy compared to low literacy"),
     topaxistitle=list(labels="difference in probability of favoring price controls"),
     width=list(null=4),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(file=file, width=7)  
)



#### figures for cb analysis

traceEV1$sublabels <- ""
traceEV1CEM$sublabels <- ""
traceEV2$sublabels <- ""
traceEV2CEM$sublabels <- ""
traceEV1$size <- 1
traceEV1CEM$size <- 1
traceEV2$size <- 1
traceEV2CEM$size <- 1
traceEV1$col <- "black"
traceEV2$col <- "black"
traceEV1CEM$col <- "grey"
traceEV2CEM$col <- "grey"
traceEV1$pch <- 19
traceEV2$pch <- 17
traceEV1CEM$pch <- 19
traceEV2CEM$pch <- 17

# Set up traces with labels and legend
# labelTrace <- textTile(labels=c("Correct CB", "Correct Direction"),
#                        x=c( 0.86,    0.89),
#                        y=c( 0.96,  0.86),
#                        cex=0.7,
#                        col="black",
#                        plot=1)
# pointTrace <- pointsTile(
#                        x=c( 0.77,    0.77),
#                        y=c( 0.96,  0.86),
#                        pch=c(19,17),
#                        size=0.6,
#                        col="black",
#                        plot=1)

# figure 3
file <- "plot_EV_cb"
tile(traceEV1,traceEV2,traceEV1CEM,traceEV2CEM,
     limits = c(0,1),
     gridlines = list(type="xt"),
     xaxis=list(at=c(0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)),
     topaxis=list(add=TRUE, at=c(0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)),
     #xaxistitle=list(labels="probability"),
     topaxistitle=list(labels="probability of doing the cost-benefit exercise correctly"),
     #plottitle=list(labels=c("Answer to cost-benefit exercise")),
     width=list(null=4),
     height=list(plottitle=2,xaxistitle=2.5,topaxistitle=2.5),
     output=list(outfile=file, width=7)
)


# These are the first differences if interested
#vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)
# 
# xat <- c(-0.05, 0, 0.05, 0.10, 0.15, 0.20,0.25, 0.30)
# xlab <- c("-5%","0%", "+5%","+10%","+15%","+20%","+25%", "+30%")
# 
# traceCBFD$size <- 1
# traceCBFDCEM$size <- 1
# 
# traceCBFDCEM$col <- "grey"
# 
# traceCBFD$pch[1] <- 19
# traceCBFD$pch[2] <- 17
# traceCBFDCEM$pch[1] <- 19
# traceCBFDCEM$pch[2] <- 17
# 
# # Make plot with tile
# file <- "plot_FD_cb"
# tile(traceCBFD, traceCBFDCEM, vertmarkFD,
#      xaxis=list(at=xat, labels=xlab),
#      topaxis=list(add=TRUE, at=xat, labels=xlab),
#      #plottitle=list(labels="High literacy compared to low literacy"),
#      topaxistitle=list(labels="difference in probability of doing CB exercise"),
#      width=list(null=4),
#      height=list(xaxistitle=3, plottitle=4),
#      gridlines=list(type="xt"),
#      output=list(file=file, width=7)   
# )


#########education figure 8

### FD
traceFD$sublabels <- "Literacy"
traceFD$sublabelsyoffset <- 0.06
traceFD$size <- 1
traceFDeduc$size <- 1
traceFDeduc$pch <- 17

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.30,-0.20,-0.10,0,0.10, 0.20,0.30)
xlab <- c("-30%","-20%","-10%", "0%","+10%", "+20%", "+30%")

# Make plot with tile
file <- "plot_FD_educ_presentation_full"
tile(traceFD,traceFDeduc, FDtraceSig1,FDtraceSig1Educ, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     #plottitle=list(labels="High literacy compared to low literacy"),
     topaxistitle=list(labels="difference in probability of favoring price controls"),
     width=list(null=4),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(file=file, width=7)  
)


##cb analysis figure 7

traceCBFDeduc$size <- 1
traceCBFDeduc$pch <- 17
traceCBFD$pch <- 19
vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.05, 0, 0.05, 0.10, 0.15, 0.20,0.25, 0.30)
xlab <- c("-5%","0%", "+5%","+10%","+15%","+20%","+25%", "+30%")

traceCBFD$size <- 1

# Make plot with tile
file <- "plot_FD_cb_educ"
tile(traceCBFD, traceCBFDeduc, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     #plottitle=list(labels="High literacy compared to low literacy"),
     topaxistitle=list(labels="difference in probability of doing CB exercise"),
     width=list(null=4),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(file=file, width=7)   
)
