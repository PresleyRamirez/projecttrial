#Core Element 1: Soil Stability 
control_data <- read.delim("C:/Users/Pichy/Desktop/R stuff/ENCE 105/Final Project/control_data.txt")
View(control_data)
acid_data <- read.delim("C:/Users/Pichy/Desktop/R stuff/ENCE 105/Final Project/acid_data.txt")
View(acid_data)
natural_data <- read.delim("C:/Users/Pichy/Desktop/R stuff/ENCE 105/Final Project/natural_data.txt")
View(natural_data)
#The large gaps is error due to heat convection and bad-handling. The points were taken out of the graph to show more well-rounded graph
boxplot(control_data$Erosion_parallel,ylab="Erosion in grams",  main="Erosion with wind parallel to surface", xlab="inside samples" )
boxplot(control_data$Erosion_above,ylab="Erosion in grams",  main="Erosion with wind from above", xlab="inside samples")
boxplot(control_data$Erosion_Angle,ylab="Erosion in grams",  main="Erosion with wind from an angle", xlab="inside samples")
boxplot(natural_data$Erosion1_parallel,ylab="Erosion in grams",  main="Erosion with wind parallel to surface", xlab="natural environment samples" )
boxplot(natural_data$Erosion2_above,ylab="Erosion in grams",  main="Erosion with wind from above", xlab="natural environment samples")
boxplot(natural_data$Erosion3_angle,ylab="Erosion in grams",  main="Erosion with wind from an angle", xlab="natural environment samples")
boxplot(acid_data$Erosion1_parallel,ylab="Erosion in grams",  main="Erosion with wind parallel to surface", xlab="half inside/natural environment samples with acid layer")
boxplot(acid_data$Erosion2_above,ylab="Erosion in grams",  main="Erosion with wind from above", xlab="half inside/natural environment samples with acid layer")
boxplot(acid_data$Erosion3_angle,ylab="Erosion in grams",  main="Erosion with wind from an angle", xlab="half inside/natural environment samples with acid layer")
#results contd.
barplot(control_data$Erosion_parallel,names.arg = (control_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind parallel to surface", sub="inside samples" )
barplot(control_data$Erosion_above,names.arg = (control_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind from above", sub="inside samples" )
barplot(control_data$Erosion_Angle,names.arg = (control_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind from an angle", sub="inside samples" )
barplot(natural_data$Erosion1_parallel,names.arg = (natural_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind parallel to surface", sub="natural environment samples" )
barplot(natural_data$Erosion2_above,names.arg = (natural_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind from above", sub="natural environment samples" )
barplot(natural_data$Erosion3_angle,names.arg = (natural_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind from an angle", sub="natural environment samples" )
barplot(acid_data$Erosion1_parallel,names.arg = (acid_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind parallel to surface", sub="half inside/natural environment samples with acid layer" )
barplot(acid_data$Erosion2_above,names.arg = (acid_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind from above", sub="half inside/natural environment samples with acid layer" )
barplot(acid_data$Erosion3_angle,names.arg = (acid_data$Sample),ylab="Erosion in grams", xlab="Sample", main="Erosion with wind from an angle", sub="half inside/natural environment samples with acid layer" )
summary(control_data$Erosion_parallel)
summary(control_data$Erosion_above)
summary(control_data$Erosion_Angle)
summary(natural_data$Erosion1_parallel)
summary(natural_data$Erosion2_above)
summary(natural_data$Erosion3_angle)
summary(acid_data$Erosion1_parallel)
summary(acid_data$Erosion2_above)
summary(acid_data$Erosion3_angle)
#Core Element 2: Temperature and Water Potential on Soil   
reading_date = "20140911" #date that step was taken, YYYYMMDD
sample_name  = "KSP10_2WP4" #name written on sample
step_name    = ""   #qualifier for multiple steps done in a day or other info
data = read.table("C:/Users/Pichy/Desktop/R stuff/ENCE 105/Final Project/KSP10_2/20140911KSP10_2WP4.txt", quote="\"")
colnames(data) <- c("Week day","Month","day","Time","Year","Minutes","Temperature","Water_Potential","pF")
data$Potential_bar <- -10*data$Water_Potential
data$Potential_fromPF <- (10^data$pF)/100.*9.81*1000./100000.
data$newtime <- data$Minutes/60
xrange=range(data$newtime)
yrange=range(data$Potential_bar,data$Potential_fromPF)
avg_last = mean(data$Potential_fromPF[(nrow(data)-4):nrow(data)])
sd_last = sd(data$Potential_fromPF[(nrow(data)-4):nrow(data)])
plot(data$newtime,data$Potential_fromPF, type="o", pch=4, xlab="Time (Hours)",xlim=xrange, ylim=yrange, ylab="Water Potential (bar)",col="blue")
points(data$newtime,data$Potential_bar, type="o", col="red")
text(mean(xrange), max(yrange), paste("St.Dev of last 5 = ",sd_last))
title("Effect of Temperature on Water Potential",cex.main = 2, font.main= 3, col.main= "blue")
legend("topright", c("From pF","From MPa"),lty=c(1,1),lwd=c(1,1),pch=c(4, 1), col=c("blue","red"))