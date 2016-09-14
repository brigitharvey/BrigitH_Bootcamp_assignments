subset(dat, treatment == "Water")
#first argument x is the object for which a mean is calculated
mean(1:4)
mean(1:4, trim = 0)
#second argument is trim = 0. it is a specified fraction of the most extreme observations of x
class(1:10)
class(warpbreaks)
summary(1:10)
summary(warpbreaks)
summary(lm(breaks ~ wool, data = warpbreaks))
#summary provides the six number summary for each numeric or integer column, but provides "tables" of the factors, counting the occurrences of each level of a factor and sorts the levels
#summary on linear modek, we get output of the regression
# B.4.1 Writing your own functions
MyBogusMean <- function(x, cheat= 0.05) {SumOfX <- sum(x) n <-length(x) trueMean <- SumOfX/n (1 + cheat) * trueMean
}
RealSales <- c(100, 200, 300)
MyBogusMean(RealSales)
MyBogusMean <- function(x,cheat=0.05)
{SumOfX <- sum(x)
n <- length(x)
trueMean <- SumOfX/n
(1+cheat) * trueMean
}
MyBogusMean(RealSales)
RealSales <- c(100, 200, 300)
MyBogusMean(RealSales)
#B6 Iterated Actions
#B6/1 Iterations of independent actions
#have a matrix, do same thing to each column or row
#use apply to tell what data want to use and the margin we want to focus on, then tell the function
#margin is side of the matrix
#describe matrices by their number of rows, columns, rows are first margin, columns are second margin
m <- matrix(1:10, nrow = 2)
m
apply(m, MARGIN = 1, mean)
apply(m, MARGIN = 2, sum)
?rowMeans
#for faster and simpler operations
#lapply will apply a function to each element of a list
#sapply is similar but simplifies result
sapply(1:10, function(i) mean(rnorm(5)))
#B6.2 Dependent Iterations
#for loops, repeated actions depending on previous outcomes
gens <- 10
output <- numeric(gens + 1)
output[1] <- 25
for (t in 1:gens) output[t + 1] <- output[t] + round(rnorm(n = 1, mean = 0, sd = 2), 0)
output
#B13 Graphics
#B13.1 plot
data(trees)
attach(trees)
plot(Girth, Height)
#B13.2 Adding points, lines and text to a plot
par(mar = c(5, 4, 3, 2))
plot(Girth, VOlume, type = "n", main = "My Trees")
plot(Girth, Volume, type = "n", main = "My Trees")
points(Girth, Volume, type = "h", col = "lightgrey", pch = 19)
hts <- (Height - min(Height))/max(Height - min(Height))
my.colors <- hcl(h = 30 + 270 * hts, alpha = 0.9)
text(Girth, Volume, Height, col = my.colors, cex = 0.5 + hts)
#B13.3 More than one response variable
#plot more than one response variable on a single axis
trees.sort <- trees[order(trees$Girth, trees$Height)]
trees.sort <- trees[order(trees$Girth, trees$Height),]
matplot(trees.sort$Girth, trees.sort[, 2:3], type = "b")
text(18, 40, "Volume", col = "darkred")
text(10, 58, "Height")
#add a second y axis having a different scale
quartz(, 4, 4)
par(mar = c(5, 4, 2, 4))
plot(Girth, Volume, main = "My Trees")
par(new = TRUE)
plot(Girth, Height, axes = FALSE, bty = "n", xlab = "", ylab = "", pch = 3)
axis(4)
mtext("Height", side = 4, line = 3)
par(mar = c(5, 4, 2, 4))
plot(Girth, Volume, main = "My Trees")
par(new = TRUE)
plot(Girth, Height, axes = FALSE, bty = "n", xlab = "", ylab = "", pch = 3)
axis(4)
mtext("Height", side = 4, line = 3)
#B13.4 Controlling Graphics Devices
#create new graphics devices or graphs in several ways
#to open a graphics device on Mac, that is 5 inch wide and 3 in tall
quartz(width = 5, height = 3)
#to control parameters, use arguments to the "par" function
#many arguments refer to the "sides" of the graph
#numbered 1-4 for the bottom X axis and left side Y axis, top and right side Y axis
#arguments to par are many (see?par)
#mar- controls width of margins on each side, units are number of lines of text, defaults to c(5,4,4,2)+0.1, bottom has most room
#mgp- controls spacing of axis title, labels and actual line itself, units of number of lines of text, and default to c(3, 1, 0) so axis title sites 3 lines away from edge of plotting region, the axis labels, one line away and the axis line sits at the edge of the plotting region
# tcl- tick length as a fraction of the height of a line of text, neg values put the tick marks outside, pos values put tick marks inside, defaults -0.5
#can build each side of the graph separately by initiating a graph but not plotting axes "plot(..., axes = FALSE), then adding axes separately
#can use layout to mke graph with several smaller subgraphs (mfrow and mfcol arguments to par and function split.screen)
#layout takes matrix as argument, containing sequence of numbers to fill regions
#create a 4 x 4 grid, two rows filled in by rows, first graph in upper left, second upper right, third fill third and fourth spots
quartz(, 5, 5)
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))
plot(Girth, Height)
par(mar = c(3, 3, 1, 1), mgp = c(1.6, 0.2, 0), tcl = 0.2)
plot(Girth, Height)
par(mar = c(3, 3, 2, 1), mgp = c(1.6, 0.2, 0), tcl = 0.2)
plot(Girth, Height, axes = FALSE, xlim = c(8,22))
axis(1, tcl = -0.3)
axis(2, tick = F)
rug(Height, side = 2, col = 2)
title("A Third, Very Wide, Plot")
#B13.5 Creating a Graphic file
#create graphics device and save to "dev.print" in format like PDF, postscript, PNG, JPEG
getwd()
quartz(, 4, 4)
plot(Height, Volume, main = "Tree Data")
dev.print(pdf, "MyTree.pdf")
