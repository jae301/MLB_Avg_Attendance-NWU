install.packages("ggplot2")
library(ggplot2)
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library('corrplot') #package corrplot
install.packages("leaps")
library(leaps)
install.packages("car") #Ability to compute VIF statistic
library(car)
install.packages("formattable")  #Enables creation of tables
library(formattable)
install.packages("MASS")   #Allows Stepwise AIC multivariable selection
library(MASS)

library(raster)   #Allows coefficient of variation calculation
library(gtable)
library(grid)
library(gridExtra)   #Creates Tables
library(plyr)  #Enables ddply

default_par <- par()

## Load MLB Attendance File
MLB <- read.csv("MLB_Clean_DATAv2.csv")
MLB$Payroll_MM <- MLB$Payroll/1000000
MLB$CPI_Pay_MM <- MLB$CPI_Pay/1000000
MLB$PayPlusWins <- MLB$Payroll_MM + MLB$Wins
MLB$CPIPayPlusWins <- MLB$CPI_Pay_MM + MLB$Wins


## Load MLB Ticket Price File
Tickets <- read.csv("MLB_Ticket_Prices.csv")



# MLB Regression Equations
summary(lm(Wins~Avg_Attend, data = MLB))
summary(lm(Wins~Payroll, data = MLB))
summary(lm(Wins~Payroll, data = MLB))

# Average Attendance Range for All 30 Teams
ddply(MLB, .(Team), summarise, Attend_Range = max(range(Avg_Attend, na.rm = TRUE))
      -min(range(Avg_Attend, na.rm = TRUE)))

ggplot(data=MLB, aes(MLB$Team, MLB$Avg_Attend)) + 
  geom_boxplot() + 
  labs(x="Team", y="Avg. Attend.") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="MLB Avg. Attendance by Team")


# MLB Plot of Attendance v. Wins
jpeg(filename="MLB_Attendance_v_Wins.jpg", width=5, height=5, units="in", res=300)
plot(MLB$Wins, MLB$Avg_Attend/1000, xlab = "Wins", 
     ylab = "Annual Avg. Attend. ('000s)", 
     main="Fig. XX \n MLB Teams Avg. Attendance v. Wins", 
     pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend/1000~MLB$Wins), lwd=2)
text(max(MLB$Wins), min(MLB$Avg_Attend/1000), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend/1000~MLB$Wins))$r.squared,2)), 
     cex = .75, pos = 2)
dev.off()

# MLB Plot of Payroll v. Wins
plot(MLB$Wins, MLB$Payroll_MM, xlab = "Wins", ylab = "Payroll (MM)", 
     main="MLB Teams Annual Payroll v. Wins", pch=16, col="blue", cex=.75)
abline(lm(MLB$Payroll_MM~MLB$Wins), lwd=2)
text(max(MLB$Wins), min(MLB$Payroll_MM), 
     paste("R-Sq.=", round(summary(lm(MLB$Payroll_MM~MLB$Wins))$r.squared,2)), 
     cex = .75, pos = 2)

#Inflation Adjusted
plot(MLB$Wins, MLB$CPI_Pay_MM, xlab = "Wins", ylab = "Inflation Adjusted Payroll (MM)", 
     main="MLB Teams Adjusted Annual Payroll v. Wins", pch=16, col="blue", cex=.75)
abline(lm(MLB$CPI_Pay_MM~MLB$Wins), lwd=2)
text(max(MLB$Wins), min(MLB$CPI_Pay_MM), 
     paste("R-Sq.=", round(summary(lm(MLB$CPI_Pay_MM~MLB$Wins))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Wins v. Payroll
plot(MLB$Payroll_MM, MLB$Wins, ylab = "Wins", xlab = "Payroll (MM)", 
     main="MLB Teams Annual Wins v. Payroll", pch=16, col="blue", cex=.75)
abline(lm(MLB$Wins~MLB$Payroll_MM), lwd=2)
text(max(MLB$Payroll_MM), min(MLB$Wins), 
     paste("R-Sq.=", round(summary(lm(MLB$Wins~MLB$Payroll_MM))$r.squared,2)), 
     cex = .75, pos = 2)
vif(lm(MLB$Avg_Attend~MLB$Wins+MLB$Payroll_MM))  #VIF of Attend v. Wins and Payroll

# Inflation Adjusted
jpeg(filename="MLB_Wins_v_Pay.jpg", width=5, height=5, units="in", res=300)
plot(MLB$CPI_Pay_MM, MLB$Wins, ylab = "Wins", xlab = "Inflation Adjusted Payroll (MM)", 
     main="Fig. XX \n MLB Teams Annual Wins v. Payroll", pch=16, col="blue", cex=.75)
abline(lm(MLB$Wins~MLB$CPI_Pay_MM), lwd=2)
text(max(MLB$CPI_Pay_MM), 
     min(MLB$Wins), 
     paste("R-Sq.=", round(summary(lm(MLB$Wins~MLB$CPI_Pay_MM))$r.squared,2)), 
     cex = .75, pos = 2)
vif(lm(MLB$Avg_Attend~MLB$Wins+MLB$CPI_Pay_MM))  #VIF of Attend v. Wins and Payroll
dev.off()

# MLB Plot of Attendance v. Payroll
plot(MLB$Payroll_MM, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Payroll (MM)", 
     main="MLB Teams Annual Avg. Attend v. Payroll", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$Payroll_MM), lwd=2)
text(max(MLB$Payroll_MM), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$Payroll_MM))$r.squared,2)), 
     cex = .75, pos = 2)

# Inflation Adjusted
jpeg(filename="MLB_Attendance_v_Pay.jpg", width=5, height=5, units="in", res=300)
plot(MLB$CPI_Pay_MM, MLB$Avg_Attend, ylab = "Avg. Attendance", xlab = "Inflation Adjusted Payroll (MM)", 
     main="Fig. XX \n MLB Annual Attend v. Adjusted Payroll", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$CPI_Pay_MM), lwd=2)
text(max(MLB$CPI_Pay_MM), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$CPI_Pay_MM))$r.squared,2)), 
     cex = .75, pos = 2)
dev.off()

# Percent Stadium Capacity
plot(MLB$CPI_Pay_MM, MLB$Avg_Per_Cap, ylab = "Avg. % Capacity", xlab = "Inflation Adjusted Payroll (MM)", 
     main="Fig. XX \n MLB Annual % Capacity v. Adjusted Payroll", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Per_Cap~MLB$CPI_Pay_MM), lwd=2)
text(max(MLB$CPI_Pay_MM), 
     min(MLB$Avg_Per_Cap), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Per_Cap~MLB$CPI_Pay_MM))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Attendance v. Payroll+Wins
plot(MLB$PayPlusWins, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Payroll (MM) + Wins", 
     main="MLB Teams Annual Avg. Attend v. Payroll+Wins", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$PayPlusWins), lwd=2)
text(max(MLB$PayPlusWins), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$PayPlusWins))$r.squared,2)), 
     cex = .75, pos = 2)

#Inflation Adjusted
plot(MLB$CPIPayPlusWins, MLB$Avg_Attend, ylab = "Avg. Attend", 
     xlab = "Inflation Adjusted Payroll (MM) + Wins", 
     main="MLB Teams Annual Avg. Attend v. Adjusted Payroll+Wins", pch=16, 
     col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$CPIPayPlusWins), lwd=2)
text(max(MLB$CPIPayPlusWins), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$CPIPayPlusWins))$r.squared,2)), 
     cex = .75, pos = 2)

summary(lm(MLB$Avg_Attend~MLB$CPIPayPlusWins))
#Coefficient of Variation (as Percent) - TBD
fitted.values(lm(MLB$Avg_Attend~MLB$CPIPayPlusWins))

# MLB Plot of Attendance v. HRs
plot(MLB$HRs, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Annual Team Homeruns", 
     main="MLB Teams Annual Avg. Attend v. Team HRs", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$HRs), lwd=2)
text(max(MLB$HRs), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$HRs))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Attendance v. Stadium Age
plot(MLB$Stadium_Age, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Stadium Age", 
     main="MLB Teams Annual Avg. Attend v. Stadium Age", pch=16, col="blue", cex=.75, xlim=(c(0,120)))
abline(lm(MLB$Avg_Attend~MLB$Stadium_Age), lwd=2)
text(max(MLB$Stadium_Age), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$Stadium_Age))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Attendance v. Runs Scored
plot(MLB$Runs_Scored, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Runs Scored", 
     main="MLB Teams Annual Avg. Attend v. Runs Scored", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$Runs_Scored), lwd=2)
text(max(MLB$Runs_Scored), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$Runs_Scored))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Attendance v. Runs Against
plot(MLB$Runs_Against, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Runs Against", 
     main="MLB Teams Annual Avg. Attend v. Runs Against", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$Runs_Against), lwd=2)
text(max(MLB$Runs_Against), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$Runs_Against))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Attendance v. Division Finish
plot(MLB$Division_Finish, MLB$Avg_Attend, ylab = "Avg. Attend", xlab = "Division Finish", 
     main="MLB Teams Annual Avg. Attend v. Division Finish", pch=16, col="blue", cex=.75)
abline(lm(MLB$Avg_Attend~MLB$Division_Finish), lwd=2)
text(max(MLB$Division_Finish), min(MLB$Avg_Attend), 
     paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend~MLB$Division_Finish))$r.squared,2)), 
     cex = .75, pos = 2)

# MLB Plot of Wins v. Division Finish
plot(MLB$Division_Finish, MLB$Wins, ylab = "Wins", xlab = "Division Finish", 
     main="MLB Teams Annual Wins v. Division Finish", pch=16, col="blue", cex=.75)
abline(lm(MLB$Wins~MLB$Division_Finish), lwd=2)
text(max(MLB$Division_Finish), min(MLB$Wins), 
     paste("R-Sq.=", round(summary(lm(MLB$Wins~MLB$Division_Finish))$r.squared,2)), 
     cex = .75, pos = 2)


## Summary Table of Simple Linear Regression R-Squared Values
all.teams <- unique(MLB$Team)
rm(out.df)
for (team in all.teams){
  this_team <- subset(MLB, Team==team)
  this_team$Team <- droplevels(this_team$Team)
  this.model.wins <- lm(Avg_Attend ~ Wins, 
                        data = this_team)   # Attendance v. Wins
  this.model.wins.summary.stats <- summary(this.model.wins)
  this.model.pay <- lm(Avg_Attend ~ CPI_Pay_MM, 
                       data = this_team)   # Attendance v. Payroll
  this.model.pay.summary.stats <- summary(this.model.pay)
  this.model.winpay <- lm(Wins ~ CPI_Pay_MM, 
                          data = this_team)   # Wins v. Payroll
  this.model.winpay.summary.stats <- summary(this.model.winpay)
  stats.df <- data.frame(Team = team,
                         Payroll = round(this.model.pay.summary.stats$r.squared, 2),
                         Wins = round(this.model.wins.summary.stats$r.squared, 2),
                         Wins_v_Pay = round(this.model.winpay.summary.stats$r.squared,2))
  if (!exists('out.df')) {out.df <- stats.df}
  else {out.df <- rbind(out.df, stats.df)}
}    

# Sorted by Attendance v. Wins
out.df <- out.df[order(out.df$Wins, decreasing=TRUE),]
out.df$Team <- factor(out.df$Team, levels = unique(out.df$Team))

formattable(out.df, list(Wins=color_tile("hotpink","green"), 
                         Payroll=color_tile("hotpink","green"),
                         Wins_v_Pay=color_text("hotpink","green")))

# Standard Plot
plot(out.df$Team, out.df$Wins, main="R-Squared of Attendance v. Wins", xlab="Team", ylab="RSq - Attend v. Win")
# GGPlot - Green & Red
ggplot(data=out.df) + 
  geom_point(aes(out.df$Team, out.df$Wins, colour=out.df$Wins>0.4)) + 
  labs(x="Team", y="Rsq. - Attendance v. Wins") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="R-Squared of Attendance v. Wins") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="none") + 
  scale_colour_manual(name = 'Team > 0', values = setNames(c('green','red'),c(T, F)))
# GGPlot - Black
ggplot(data=out.df) + 
  geom_point(aes(out.df$Team, out.df$Wins)) + 
  labs(x="Team", y="Rsq. - Attendance v. Wins") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="R-Squared of Attendance v. Wins") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="none")


# Sorted by Attendance v. Payroll
out.df <- out.df[order(out.df$Payroll, decreasing=TRUE),]
out.df$Team <- factor(out.df$Team, levels = unique(out.df$Team))
formattable(out.df, list(Payroll=color_tile("hotpink","green"), 
                         Wins=color_tile("hotpink","green"), 
                         Wins_v_Pay=color_text("hotpink","green")),
            title(main = "Title"))

ggplot(data=out.df) + 
  geom_point(aes(out.df$Team, out.df$Payroll, colour=out.df$Payroll>0.6)) + 
  labs(x="Team", y="Rsq. - Attendance v. Payroll") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="R-Squared of Attendance v. Payroll") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="none") + 
  scale_colour_manual(name = 'Team > 0', values = setNames(c('green','red'),c(T, F)))


## Scatterplot of R-Squared for Wins and Pay for Each Team
p1 <- ggplot(out.df, aes(out.df$Team,out.df$Wins)) + 
  geom_point(colour = "blue", size=3, show.legend = TRUE) + 
  theme_bw() + labs(x="Team", y="R-Squared (Blue=Wins; Green=Pay)") + 
  theme(axis.text.x = element_text(angle = 90))
p2 <- ggplot(out.df, aes(out.df$Team,out.df$Payroll)) + 
  geom_point(colour = "forestgreen", size=3) + 
  theme_bw() + 
  theme(panel.background = element_rect(fill = NA))
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)
grid.draw(g)


## Grid of All P-Values By Variable
all.teams <- unique(MLB$Team)
for (team in all.teams){
  this_team <- subset(MLB, Team==team)
  this_team$Team <- droplevels(this_team$Team)
  this.model <- lm(Avg_Attend ~ 
                     Wins + 
                     Stadium_Age +
                     Runs_Scored + 
                     Runs_Against + 
                     HRs + 
                     CPI_Pay + 
                     Division_Finish
                   ,data = this_team)
  this.model.summary.stats <- summary(this.model)
  stats.df <- data.frame(Team = team,
                         AdjRSq = round(this.model.summary.stats$adj.r.squared, 2),
                         Wins = round(coef(this.model.summary.stats)["Wins", "Pr(>|t|)"], 2),
                         Age = round(coef(this.model.summary.stats)["Stadium_Age", "Pr(>|t|)"], 2), 
                         R.For = round(coef(this.model.summary.stats)["Runs_Scored", "Pr(>|t|)"], 2),
                         R.Agst = round(coef(this.model.summary.stats)["Runs_Against", "Pr(>|t|)"], 2),
                         HRs = round(coef(this.model.summary.stats)["HRs", "Pr(>|t|)"], 2),
                         Pay = round(coef(this.model.summary.stats)["CPI_Pay", "Pr(>|t|)"], 2),
                         Finish = round(coef(this.model.summary.stats)["Division_Finish", "Pr(>|t|)"], 2))
  if (!exists('out.df')) {out.df <- stats.df}
  else {out.df <- rbind(out.df, stats.df)}
}    
out.df <- out.df[order(out.df$Wins, decreasing=FALSE),]
color.format <- formatter("span", style = x ~ style("background-color"=ifelse(x <0.05, "lightgreen", "white")))
formattable(out.df, list(Wins = color.format, 
                         Age = color.format,
                         R.For = color.format,
                         R.Agst = color.format,
                         HRs = color.format,
                         Pay = color.format,
                         Finish = color.format))
rm(out.df)


##
summary(lm(MLB$Avg_Attend[MLB$Team=="HOU"]~MLB$HRs[MLB$Team=="HOU"]))


## Individual Team Analyses
par(mar=c(5,4,4,5)+0.1)

## Minnesota
#Attendance and Wins By Year
plot(MLB$Year[MLB$Team=="MIN"], MLB$Avg_Attend[MLB$Team=="MIN"]/1000, ylim = c(0,100),
     pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="MIN"], MLB$Wins[MLB$Team=="MIN"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col.axis="blue", las=2)
mtext("Wins", side=4, line=3, las=0) 
title("MIN Avg. Attend. & Wins by Year", xlab = "Year", ylab = "Avg. Attend (000s)")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend[MLB$Team=="MIN"]~MLB$Wins[MLB$Team=="MIN"]))$r.squared,2)), cex = .75, pos = 2)

summary(lm(MLB$Avg_Attend[MLB$Team=="MIN"]~MLB$Payroll[MLB$Team=="MIN"]))$r.square

#Attendance and CPI Adjusted Payroll By Year
plot(MLB$Year[MLB$Team=="MIN"], MLB$Avg_Attend[MLB$Team=="MIN"]/1000, ylim = c(0,120),
     pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="MIN"], MLB$CPI_Pay_MM[MLB$Team=="MIN"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), col.axis="blue", las=2)
mtext("CPI Adjusted Payroll ($MM)", side=4, line=3, las=0) 
title("MIN Avg. Attendance & Adjusted Payroll by Year", xlab = "Year", ylab = "Avg. Attend (000s)")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend[MLB$Team=="MIN"]~MLB$CPI_Pay_MM[MLB$Team=="MIN"]))$r.squared,2)), cex = .75, pos = 2)

#Wins and CPI Adjusted Payroll By Year
plot(MLB$Year[MLB$Team=="MIN"], MLB$Wins[MLB$Team=="MIN"], ylim = c(0,120),
     pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="MIN"], MLB$CPI_Pay_MM[MLB$Team=="MIN"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), col.axis="blue", las=2)
mtext("CPI Adjusted Payroll ($MM)", side=4, line=3, las=0) 
title("MIN Wins & Adjusted Payroll by Year", xlab = "Year", ylab = "Wins")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Wins[MLB$Team=="MIN"]~MLB$CPI_Pay_MM[MLB$Team=="MIN"]))$r.squared,2)), cex = .75, pos = 2)


## Philadelphia
# Attendance & Wins by Year
plot(MLB$Year[MLB$Team=="PHI"], MLB$Avg_Attend[MLB$Team=="PHI"]/1000, 
     ylim = c(0,100), pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="PHI"], MLB$Wins[MLB$Team=="PHI"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col.axis="blue", las=2)
mtext("Wins", side=4, line=3, las=0) 
title("PHI Avg. Attend. & Wins by Year", xlab = "Year", ylab = "Avg. Attend (000s)")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend[MLB$Team=="PHI"]~MLB$Wins[MLB$Team=="PHI"]))$r.squared,2)), cex = .75, pos = 2)

# Attendance & Payroll by Year
plot(MLB$Year[MLB$Team=="PHI"], MLB$Avg_Attend[MLB$Team=="PHI"]/1000, 
     ylim = c(0,180), pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="PHI"], MLB$Payroll_MM[MLB$Team=="PHI"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180), col.axis="blue", las=2)
mtext("Payroll (MM)", side=4, line=3, las=0) 
title("PHI Avg. Attend. & Payroll by Year", xlab = "Year", ylab = "Avg. Attend (000s)")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend[MLB$Team=="PHI"]~MLB$Payroll_MM[MLB$Team=="PHI"]))$r.squared,2)), cex = .75, pos = 2)

# Wins & Payroll by Year
plot(MLB$Year[MLB$Team=="PHI"], MLB$Wins[MLB$Team=="PHI"], 
     ylim = c(0,180), pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="PHI"], MLB$Payroll_MM[MLB$Team=="PHI"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180), col.axis="blue", las=2)
mtext("Payroll (MM)", side=4, line=3, las=0) 
title("PHI Wins & Payroll by Year", xlab = "Year", ylab = "Wins")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Wins[MLB$Team=="PHI"]~MLB$Payroll_MM[MLB$Team=="PHI"]))$r.squared,2)), cex = .75, pos = 2)

summary(lm(MLB$Avg_Attend[MLB$Team=="PHI"]~MLB$Payroll[MLB$Team=="PHI"]))$r.square


## Cleveland
plot(MLB$Year[MLB$Team=="CLE"], MLB$Avg_Attend[MLB$Team=="CLE"]/1000, ylim = c(0,100),
     pch=16, col="red", lty=3, type="b", yaxt="n", ann=FALSE)
lines(MLB$Year[MLB$Team=="CLE"], MLB$Wins[MLB$Team=="CLE"], type="b", pch=15, 
      col="blue", lty=2)
axis(2, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col.axis="red", las=2)
axis(4, at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), col.axis="blue", las=2)
mtext("Wins", side=4, line=3, las=0) 
title("CLE Avg. Attend. & Wins by Year", xlab = "Year", ylab = "Avg. Attend (000s)")
text(max(MLB$Year), 0, paste("R-Sq.=", round(summary(lm(MLB$Avg_Attend[MLB$Team=="CLE"]~MLB$Wins[MLB$Team=="CLE"]))$r.squared,2)), cex = .75, pos = 2)

summary(lm(MLB$Avg_Attend[MLB$Team=="CLE"]~MLB$Payroll[MLB$Team=="CLE"]))$r.square


par(default_par)


########## Minnesota ##########
paste("Minnesota Average Ticket Price (2016 Est.): $", Tickets$CPI_Ticket[Tickets$Team=="MIN"])
paste("Minnesota Average Ticket Price Rank: ", Tickets$Rank[Tickets$Team=="MIN"])
paste("Minnesota Fan Cost Index (2012 Actual): $", Tickets$Fan_Cost_Index[Tickets$Team=="MIN"])
paste("Minnesota Fan Cost Index (2016 Est.): $", Tickets$CPI_Index[Tickets$Team=="MIN"])

# Multivariate AIC Variable Selection
stepAIC(lm(MLB$Attendance[MLB$Team=="MIN"] ~ 
             MLB$CPI_Pay_MM[MLB$Team=="MIN"] + 
             MLB$Wins[MLB$Team=="MIN"] + 
             MLB$Runs_Scored[MLB$Team=="MIN"] + 
             MLB$Runs_Against[MLB$Team=="MIN"] + 
             MLB$HRs[MLB$Team=="MIN"] + 
             MLB$Division_Finish[MLB$Team=="MIN"]),
        direction = "both")

summary(lm(MLB$Attendance[MLB$Team == "MIN"] ~ 
             MLB$CPI_Pay_MM[MLB$Team == "MIN"] + 
             MLB$Wins[MLB$Team == "MIN"] + 
             MLB$HRs[MLB$Team == "MIN"]))

####JONH_REST_STEPWISE
#ATLANTA #R^2 = 0.5927
Atlanta<-MLB[1:21,]
ATL_Reg2<-lm(Atlanta$Avg_Attend~ Atlanta$Wins+ Atlanta$Runs_Scored+ 
               Atlanta$Runs_Against+ Atlanta$HRs + Atlanta$CPI_Pay+ 
               Atlanta$Division_Finish)
StepAIC_ATL=stepAIC(ATL_Reg2, direction="both")
StepAIC_ATL$anova
summary(StepAIC_ATL)

#ARIZONA #R^2 = 0.1136
Arizona<-MLB[22:40,]
ARI_Reg2<-lm(Arizona$Avg_Attend~ Arizona$Wins+ Arizona$Runs_Scored+ 
               Arizona$Runs_Against+ Arizona$HRs + Arizona$CPI_Pay+ 
               Arizona$Division_Finish)
StepAIC_ARI=stepAIC(ARI_Reg2, direction="both")
StepAIC_ARI$anova
summary(StepAIC_ARI)



###BAL #R^2 = 0.409

Baltimore<-MLB[41:61,]
BAL_Reg2<-lm(Baltimore$Avg_Attend~ Baltimore$Wins+ Baltimore$Runs_Scored+ 
               Baltimore$Runs_Against+ Baltimore$HRs + Baltimore$CPI_Pay+ 
               Baltimore$Division_Finish)
StepAIC_BAL=stepAIC(BAL_Reg2, direction="both")
StepAIC_BAL$anova
summary(StepAIC_BAL)



##Bos #R^2 = 0.67
Boston<-MLB[62:82,]
BOS_Reg2<-lm(Boston$Avg_Attend~ Boston$Wins+ Boston$Runs_Scored+ 
               Boston$Runs_Against+ Boston$HRs + Boston$CPI_Pay+ 
               Boston$Division_Finish)
StepAIC_BOS=stepAIC(BOS_Reg2, direction="both")
StepAIC_BOS$anova
summary(StepAIC_BOS)

#CHC #R^2 = 0.7544

Cubs<-MLB[83:103,]
CHC_Reg2<-lm(Cubs$Avg_Attend~ Cubs$Wins+ Cubs$Runs_Scored+ 
               Cubs$Runs_Against+ Cubs$HRs + Cubs$CPI_Pay+ 
               Cubs$Division_Finish)
StepAIC_CHC=stepAIC(CHC_Reg2, direction="both")
StepAIC_CHC$anova
summary(StepAIC_CHC)

#CHW #R^2 = 0.6069

WhiteSox<-MLB[104:124,]
CHW_Reg2<-lm(WhiteSox$Avg_Attend~ WhiteSox$Wins+ WhiteSox$Runs_Scored+ 
               WhiteSox$Runs_Against+ WhiteSox$HRs + WhiteSox$CPI_Pay+ 
               WhiteSox$Division_Finish)
StepAIC_CHW=stepAIC(CHW_Reg2, direction="both")
StepAIC_CHW$anova
summary(StepAIC_CHW)

#CIN #R^2 = 0.3237
Reds<-MLB[125:145,]
CIN_Reg2<-lm(Reds$Avg_Attend~ Reds$Wins+ Reds$Runs_Scored+ 
               Reds$Runs_Against+ Reds$HRs + Reds$CPI_Pay+ 
               Reds$Division_Finish)
StepAIC_CIN=stepAIC(CIN_Reg2, direction="both")
StepAIC_CIN$anova
summary(StepAIC_CIN)


#CLE #R^2 = 0.82
Indians<-MLB[146:166,]
CLE_Reg2<-lm(Indians$Avg_Attend~ Indians$Wins+ Indians$Runs_Scored+ 
               Indians$Runs_Against+ Indians$HRs + Indians$CPI_Pay+ 
               Indians$Division_Finish)
StepAIC_CLE=stepAIC(CLE_Reg2, direction="both")
StepAIC_CLE$anova
summary(StepAIC_CLE)




#COL #R^2 = 0.4877
Rockies<-MLB[167:187,]
COL_Reg2<-lm(Rockies$Avg_Attend~ Rockies$Wins+ Rockies$Runs_Scored+ 
               Rockies$Runs_Against+ Rockies$HRs + Rockies$CPI_Pay+ 
               Rockies$Division_Finish)
StepAIC_COL=stepAIC(COL_Reg2, direction="both")
StepAIC_COL$anova
summary(StepAIC_COL)

#DET #R^2 = 0.8562
Tigers<-MLB[188:208,]
DET_Reg2<-lm(Tigers$Avg_Attend~ Tigers$Wins+ Tigers$Runs_Scored+ 
               Tigers$Runs_Against+ Tigers$HRs + Tigers$CPI_Pay+ 
               Tigers$Division_Finish)
StepAIC_DET=stepAIC(DET_Reg2, direction="both")
StepAIC_DET$anova
summary(StepAIC_DET)

#HOU #R^2 = 0.8264
Astros<-MLB[209:229,]
HOU_Reg2<-lm(Astros$Avg_Attend~ Astros$Wins+ Astros$Runs_Scored+ 
               Astros$Runs_Against+ Astros$HRs + Astros$CPI_Pay+ 
               Astros$Division_Finish)
StepAIC_HOU=stepAIC(HOU_Reg2, direction="both")
StepAIC_HOU$anova
summary(StepAIC_HOU)

#KC #R^2 = 0.8686
Royals<-MLB[230:250,]
KC_Reg2<-lm(Royals$Avg_Attend~ Royals$Wins+ Royals$Runs_Scored+ 
               Royals$Runs_Against+ Royals$HRs + Royals$CPI_Pay+ 
               Royals$Division_Finish)
StepAIC_KC=stepAIC(KC_Reg2, direction="both")
StepAIC_KC$anova
summary(StepAIC_KC)

#LAA #R^2 = 0.8643
Angel<-MLB[251:271,]
LAA_Reg2<-lm(Angel$Avg_Attend~ Angel$Wins+ Angel$Runs_Scored+ 
              Angel$Runs_Against+ Angel$HRs + Angel$CPI_Pay +
              Angel$Division_Finish)
StepAIC_LAA=stepAIC(LAA_Reg2, direction="both")
StepAIC_LAA$anova
summary(StepAIC_LAA)



#LAD #R^2 = 0.5157
Dodgers<-MLB[272:292,]
LAD_Reg2<-lm(Dodgers$Avg_Attend~ Dodgers$Wins+ Dodgers$Runs_Scored+ 
               Dodgers$Runs_Against+ Dodgers$HRs + Dodgers$CPI_Pay+ 
               Dodgers$Division_Finish)
StepAIC_LAD=stepAIC(LAD_Reg2, direction="both")
StepAIC_LAD$anova
summary(StepAIC_LAD)


#MIA #R^2 = 0.4561
Marlins<-MLB[293:313,]
MIA_Reg2<-lm(Marlins$Avg_Attend~ Marlins$Wins+ Marlins$Runs_Scored+ 
               Marlins$Runs_Against+ Marlins$HRs + Marlins$CPI_Pay+ 
               Marlins$Division_Finish)
StepAIC_MIA=stepAIC(MIA_Reg2, direction="both")
StepAIC_MIA$anova
summary(StepAIC_MIA)

#MIL #R^2 = 0.8019
Brewers<-MLB[314:334,]
MIL_Reg2<-lm(Brewers$Avg_Attend~ Brewers$Wins+ Brewers$Runs_Scored+ 
               Brewers$Runs_Against+ Brewers$HRs + Brewers$CPI_Pay+ 
               Brewers$Division_Finish)
StepAIC_MIL=stepAIC(MIL_Reg2, direction="both")
StepAIC_MIL$anova
summary(StepAIC_MIL)

#MIN #R^2 = 0.7749
Twins<-MLB[335:355,]
MIN_Reg2<-lm(Twins$Avg_Attend~ Twins$Wins+ Twins$Runs_Scored+ 
               Twins$Runs_Against+ Twins$HRs + Twins$CPI_Pay+ 
               Twins$Division_Finish)
StepAIC_MIN=stepAIC(MIN_Reg2, direction="both")
StepAIC_MIN$anova
summary(StepAIC_MIN)


#NYM #R^2 = 0.7475
Mets<-MLB[356:376,]
NYM_Reg2<-lm(Mets$Avg_Attend~ Mets$Wins+ Mets$Runs_Scored+ 
               Mets$Runs_Against+ Mets$HRs + Mets$CPI_Pay+ 
               Mets$Division_Finish)
StepAIC_NYM=stepAIC(NYM_Reg2, direction="both")
StepAIC_NYM$anova
summary(StepAIC_NYM)



#NYY #R^2 = 0.8097
Yankees<-MLB[377:397,]
NYY_Reg2<-lm(Yankees$Avg_Attend~ Yankees$Wins+ Yankees$Runs_Scored+ 
               Yankees$Runs_Against+ Yankees$HRs + Yankees$CPI_Pay+ 
               Yankees$Division_Finish)
StepAIC_NYY=stepAIC(NYY_Reg2, direction="both")
StepAIC_NYY$anova
summary(StepAIC_NYY)



#OAK #R^2 = 0.7183
Athletics<-MLB[398:418,]
OAK_Reg2<-lm(Athletics$Avg_Attend~ Athletics$Wins+ Athletics$Runs_Scored+ 
               Athletics$Runs_Against+ Athletics$HRs + Athletics$CPI_Pay+ 
               Athletics$Division_Finish)
StepAIC_OAK=stepAIC(OAK_Reg2, direction="both")
StepAIC_OAK$anova
summary(StepAIC_OAK)

#PHI #R^2 = 0.9082
Phillies<-MLB[419:439,]
PHI_Reg2<-lm(Phillies$Avg_Attend~ Phillies$Wins+ Phillies$Runs_Scored+ 
               Phillies$Runs_Against+ Phillies$HRs + Phillies$CPI_Pay+ 
               Phillies$Division_Finish)
StepAIC_PHI=stepAIC(PHI_Reg2, direction="both")
StepAIC_PHI$anova
summary(StepAIC_PHI)

#PIT #R^2 = 0.6737
Pirates<-MLB[440:460,]
PIT_Reg2<-lm(Pirates$Avg_Attend~ Pirates$Wins+ Pirates$Runs_Scored+ 
               Pirates$Runs_Against+ Pirates$HRs + Pirates$CPI_Pay+ 
               Pirates$Division_Finish)
StepAIC_PIT=stepAIC(PIT_Reg2, direction="both")
StepAIC_PIT$anova
summary(StepAIC_PIT)


#SD #R^2 = 0.5405
Padres<-MLB[461:481,]
SD_Reg2<-lm(Padres$Avg_Attend~ Padres$Wins+ Padres$Runs_Scored+ 
               Padres$Runs_Against+ Padres$HRs + Padres$CPI_Pay+ 
               Padres$Division_Finish)
StepAIC_SD=stepAIC(SD_Reg2, direction="both")
StepAIC_SD$anova
summary(StepAIC_SD)

#SF #R^2 = 0.6454
Giants<-MLB[482:502,]
SF_Reg2<-lm(Giants$Avg_Attend~ Giants$Wins+ Giants$Runs_Scored+ 
              Giants$Runs_Against+ Giants$HRs + Giants$CPI_Pay+ 
              Giants$Division_Finish)
StepAIC_SF=stepAIC(SF_Reg2, direction="both")
StepAIC_SF$anova
summary(StepAIC_SF)

#SEA #R^2 = 0.7448
Mariners<-MLB[503:523,]
SEA_Reg2<-lm(Mariners$Avg_Attend~ Mariners$Wins+ Mariners$Runs_Scored+ 
               Mariners$Runs_Against+ Mariners$HRs + Mariners$CPI_Pay+ 
               Mariners$Division_Finish)
StepAIC_SEA=stepAIC(SEA_Reg2, direction="both")
StepAIC_SEA$anova
summary(StepAIC_SEA)

#STL #R^2 = 0.5142
Cardinals<-MLB[524:544,]
STL_Reg2<-lm(Cardinals$Avg_Attend~ Cardinals$Wins+ Cardinals$Runs_Scored+ 
               Cardinals$Runs_Against+ Cardinals$HRs + Cardinals$CPI_Pay+ 
               Cardinals$Division_Finish)
StepAIC_STL=stepAIC(STL_Reg2, direction="both")
StepAIC_STL$anova
summary(StepAIC_STL)

#TB #R^2 = 0.1376
Rays<-MLB[545:563,]
TB_Reg2<-lm(Rays$Avg_Attend~ Rays$Wins+ Rays$Runs_Scored+ 
              Rays$Runs_Against+ Rays$HRs + Rays$CPI_Pay+ 
              Rays$Division_Finish)
StepAIC_TB=stepAIC(TB_Reg2, direction="both")
StepAIC_TB$anova
summary(StepAIC_TB)


#TEX #R^2 = 0.4339
Rangers<-MLB[564:584,]
TEX_Reg2<-lm(Rangers$Avg_Attend~ Rangers$Wins+ Rangers$Runs_Scored+ 
              Rangers$Runs_Against+ Rangers$HRs + Rangers$CPI_Pay+ 
              Rangers$Division_Finish)
StepAIC_TEX=stepAIC(TEX_Reg2, direction="both")
StepAIC_TEX$anova
summary(StepAIC_TEX)

#TOR #R^2 = 0.3113
BlueJays<-MLB[585:605,]
TOR_Reg2<-lm(BlueJays$Avg_Attend~ BlueJays$Wins+ BlueJays$Runs_Scored+ 
               BlueJays$Runs_Against+ BlueJays$HRs + BlueJays$CPI_Pay+ 
               BlueJays$Division_Finish)
StepAIC_TOR=stepAIC(TOR_Reg2, direction="both")
StepAIC_TOR$anova
summary(StepAIC_TOR)


#WAS #R^2 = 0.5407
Nationals<-MLB[606:626,]
WAS_Reg2<-lm(Nationals$Avg_Attend~ Nationals$Wins+ Nationals$Runs_Scored+ 
               Nationals$Runs_Against+ Nationals$HRs + Nationals$CPI_Pay+ 
               Nationals$Division_Finish)
StepAIC_WAS=stepAIC(WAS_Reg2, direction="both")
StepAIC_WAS$anova
summary(StepAIC_WAS)


#SEA #R^2 = 0.7448
#PHI #R^2 = 0.9082
#OAK #R^2 = 0.7183
#NYY #R^2 = 0.8097
#NYM #R^2 = 0.7475
#MIN #R^2 = 0.7749
#MIL #R^2 = 0.8019
#LAA #R^2 = 0.8643
#KC #R^2 = 0.8686
#HOU #R^2 = 0.8264
#DET #R^2 = 0.8562
#CLE #R^2 = 0.82
#CHC #R^2 = 0.7544









## Old Code
Team_Wins_Only <- c("OAK","SEA")
Team_Pay_Only <- c("BOS","CHC","LAA","MIA","MIL","MIN","NYM","NYY","SF","STL","WAS")
Team_Wins_and_Pay <- c("ATL","DET","HOU","KC","PHI","PIT")
Team_Neither <- c("ARI","BAL","CHW","CIN","CLE","COL","LAD","SD","TB","TEX","TOR")

MLB_dataframe<-data.frame(MLB$Wins, MLB$Year, MLB$Capacity, MLB$Attendance, MLB$Avg_Attend, MLB$Runs_Scored, MLB$Runs_Against, MLB$HRs, MLB$Payroll, MLB$Division_Finish)
MLB_dataframe_noCapacity<-data.frame(MLB$Wins, MLB$Year, MLB$Attendance, MLB$Avg_Attend, MLB$Runs.Scored, MLB$Runs.Against, MLB$HRs, MLB$Payroll, MLB$Division.Finish)

MLB_Correlation<-cor(MLB_dataframe)
MLB_Correlation

corrplot(MLB_Correlation, method = "number")

MLB_Reg<-lm(MLB$Avg_Attend ~ MLB$Wins + MLB$Year+ MLB$Capacity+  MLB$Age.of.Stadium+ MLB$Runs.Scored+ MLB$Runs.Against+ MLB$HRs+ MLB$Payroll+ MLB$Division.Finish)

Summary<-summary(MLB_Reg)

test <- as.data.frame(MLB[,c("Avg_Attend", "Wins", "Runs.Scored", "Runs.Against", "HRs")])
leaps <- regsubsets(Avg_Attend ~ Wins + Runs.Scored + Runs.Against + HRs, data=test, nbest = 4)
plot(leaps, scale="adjr2")

#Atlanta
Atlanta<-MLB_dataframe[1:21,]
ATL_Reg<-lm(Atlanta$MLB.Avg_Attend ~ Atlanta$MLB.Wins + Atlanta$MLB.Year+ Atlanta$MLB.Capacity+ Atlanta$MLB.Runs.Scored+ Atlanta$MLB.Runs.Against+ Atlanta$MLB.HRs + Atlanta$MLB.Payroll+ Atlanta$MLB.Division.Finish)
summary(ATL_Reg)
MLB_Correlation_ATL<-cor(Atlanta)
MLB_Correlation_ATL
corrplot(MLB_Correlation_ATL, method = "circle")
corrplot(MLB_Correlation_ATL, method = "number", number.cex=0.75, main="Atlanta")
Atlanta<-MLB_dataframe[1:21,]
ATL_Reg2<-lm(Atlanta$MLB.Avg_Attend~ Atlanta$MLB.Wins+ Atlanta$MLB.Runs_Scored+ Atlanta$MLB.Runs_Against+ Atlanta$MLB.HRs + Atlanta$MLB.Payroll+ Atlanta$MLB.Division_Finish)





#ARI
Arizona<-MLB_dataframe_noCapacity[22:40,]
ARI_Reg<-lm(Arizona$MLB.Avg_Attend ~ Arizona$MLB.Wins + Arizona$MLB.Year+ Arizona$MLB.Runs.Scored+ Arizona$MLB.Runs.Against+ Arizona$MLB.HRs + Arizona$MLB.Payroll+ Arizona$MLB.Division.Finish)#Arizona$MLB.Capacity
summary(ARI_Reg)
MLB_Correlation_ARI<-cor(Arizona)###CAPACITY TAKEN OUT BC ITS ALL THE SAME, THUS STAND DEV IS 0
MLB_Correlation_ARI
corrplot(MLB_Correlation_ARI, method = "circle")
corrplot(MLB_Correlation_ARI, method = "number", number.cex=0.75, main="Arizona")


##BAL

Baltimore<-MLB_dataframe[41:61,]
BAL_Reg<-lm(Baltimore$MLB.Avg_Attend ~ Baltimore$MLB.Wins + Baltimore$MLB.Year+ Baltimore$MLB.Capacity+ Baltimore$MLB.Runs.Scored+ Baltimore$MLB.Runs.Against+ Baltimore$MLB.HRs + Baltimore$MLB.Payroll+ Baltimore$MLB.Division.Finish)
summary(BAL_Reg)
MLB_Correlation_BAL<-cor(Baltimore)
MLB_Correlation_BAL
corrplot(MLB_Correlation_BAL, method = "circle")
corrplot(MLB_Correlation_BAL, method = "number", number.cex=0.75, main="Baltimore")


##Bos
Boston<-MLB_dataframe_noCapacity[62:82,]
BOS_Reg<-lm(Boston$MLB.Avg_Attend ~ Boston$MLB.Wins + Boston$MLB.Year+ Boston$MLB.Runs.Scored+ Boston$MLB.Runs.Against+ Boston$MLB.HRs + Boston$MLB.Payroll+ Boston$MLB.Division.Finish)
summary(BOS_Reg)
MLB_Correlation_BOS<-cor(Boston)
MLB_Correlation_BOS
corrplot(MLB_Correlation_BOS, method = "circle")
corrplot(MLB_Correlation_BOS, method = "number", number.cex=0.75, main="Boston")

#CHC

Cubs<-MLB_dataframe_noCapacity[83:103,]
CHC_Reg<-lm(Cubs$MLB.Avg_Attend ~ Cubs$MLB.Wins + Cubs$MLB.Year+ Cubs$MLB.Runs.Scored+ Cubs$MLB.Runs.Against+ Cubs$MLB.HRs + Cubs$MLB.Payroll+ Cubs$MLB.Division.Finish)
summary(CHC_Reg)
MLB_Correlation_CHC<-cor(Cubs)
MLB_Correlation_CHC
corrplot(MLB_Correlation_CHC, method = "circle")
corrplot(MLB_Correlation_CHC, method = "number", number.cex=0.75, main="Chicago Cubs")

#CHW

WhiteSox<-MLB_dataframe_noCapacity[104:124,]
CWS_Reg<-lm(WhiteSox$MLB.Avg_Attend ~ WhiteSox$MLB.Wins + WhiteSox$MLB.Year+ WhiteSox$MLB.Runs.Scored+ WhiteSox$MLB.Runs.Against+ WhiteSox$MLB.HRs + WhiteSox$MLB.Payroll+ WhiteSox$MLB.Division.Finish)
summary(CWS_Reg)
MLB_Correlation_CWS<-cor(WhiteSox)
MLB_Correlation_CWS
corrplot(MLB_Correlation_CWS, method = "circle")
corrplot(MLB_Correlation_CWS, method = "number", number.cex=0.75, main="Chicago White Sox")

#CIN
Reds<-MLB_dataframe[125:145,]
CIN_Reg<-lm(Reds$MLB.Avg_Attend ~ Reds$MLB.Wins + Reds$MLB.Year+ Reds$MLB.Capacity+ Reds$MLB.Runs.Scored+ Reds$MLB.Runs.Against+ Reds$MLB.HRs + Reds$MLB.Payroll+ Reds$MLB.Division.Finish)
summary(CIN_Reg)
MLB_Correlation_CIN<-cor(Reds)
MLB_Correlation_CIN
corrplot(MLB_Correlation_CIN, method = "circle")
corrplot(MLB_Correlation_CIN, method = "number", number.cex=0.75, main="Cincinnati")


#CLE
Indians<-MLB_dataframe_noCapacity[146:166,]
CLE_Reg<-lm(Indians$MLB.Avg_Attend ~ Indians$MLB.Wins + Indians$MLB.Year+ Indians$MLB.Runs.Scored+ Indians$MLB.Runs.Against+ Indians$MLB.HRs + Indians$MLB.Payroll+ Indians$MLB.Division.Finish)
summary(CLE_Reg)
MLB_Correlation_CLE<-cor(Indians)
MLB_Correlation_CLE
corrplot(MLB_Correlation_CLE, method = "circle")
corrplot(MLB_Correlation_CLE, method = "number", number.cex=0.75, main="Cleveland")

#COL
Rockies<-MLB_dataframe_noCapacity[167:187,]
COL_Reg<-lm(Rockies$MLB.Avg_Attend ~ Rockies$MLB.Wins + Rockies$MLB.Year+ Rockies$MLB.Runs.Scored+ Rockies$MLB.Runs.Against+ Rockies$MLB.HRs + Rockies$MLB.Payroll+ Rockies$MLB.Division.Finish)
summary(COL_Reg)
MLB_Correlation_COL<-cor(Rockies)
MLB_Correlation_COL
corrplot(MLB_Correlation_COL, method = "circle")
corrplot(MLB_Correlation_COL, method = "number", number.cex=0.75, main="Colorado")

#DET
Tigers<-MLB_dataframe[188:208,]
DET_Reg<-lm(Tigers$MLB.Avg_Attend ~ Tigers$MLB.Wins + Tigers$MLB.Year+ Tigers$MLB.Capacity+ Tigers$MLB.Runs.Scored+ Tigers$MLB.Runs.Against+ Tigers$MLB.HRs + Tigers$MLB.Payroll+ Tigers$MLB.Division.Finish)
summary(DET_Reg)
MLB_Correlation_DET<-cor(Tigers)
MLB_Correlation_DET
corrplot(MLB_Correlation_DET, method = "circle")
corrplot(MLB_Correlation_DET, method = "number", number.cex=0.75, main="Detroit")

#HOU
Astros<-MLB_dataframe[209:229,]
HOU_Reg<-lm(Astros$MLB.Avg_Attend ~ Astros$MLB.Wins + Astros$MLB.Year+ Astros$MLB.Capacity+ Astros$MLB.Runs.Scored+ Astros$MLB.Runs.Against+ Astros$MLB.HRs + Astros$MLB.Payroll+ Astros$MLB.Division.Finish)
summary(HOU_Reg)
MLB_Correlation_HOU<-cor(Astros)
MLB_Correlation_HOU
corrplot(MLB_Correlation_HOU, method = "circle")
corrplot(MLB_Correlation_HOU, method = "number", number.cex=0.75, main="Houston")


#KC
Royals<-MLB_dataframe[230:250,]
KC_Reg<-lm(Royals$MLB.Avg_Attend ~ Royals$MLB.Wins + Royals$MLB.Year+ Royals$MLB.Capacity+ Royals$MLB.Runs.Scored+ Royals$MLB.Runs.Against+ Royals$MLB.HRs + Royals$MLB.Payroll+ Royals$MLB.Division.Finish)
summary(KC_Reg)
MLB_Correlation_KC<-cor(Royals)
MLB_Correlation_KC
corrplot(MLB_Correlation_KC, method = "circle")
corrplot(MLB_Correlation_KC, method = "number", number.cex=0.75, main="Kansas City")

#LAA
Angel<-MLB_dataframe[251:271,]
LAA_Reg<-lm(Angel$MLB.Avg_Attend ~ Angel$MLB.Wins + Angel$MLB.Year+ Angel$MLB.Capacity+ Angel$MLB.Runs.Scored+ Angel$MLB.Runs.Against+ Angel$MLB.HRs + Angel$MLB.Payroll+ Angel$MLB.Division.Finish)
summary(LAA_Reg)
MLB_Correlation_LAA<-cor(Angel)
MLB_Correlation_LAA
corrplot(MLB_Correlation_LAA, method = "circle")
corrplot(MLB_Correlation_LAA, method = "number", number.cex=0.75, main="Los Angeles Angels")



#LAD
Dodgers<-MLB_dataframe_noCapacity[272:292,]
LAD_Reg<-lm(Dodgers$MLB.Avg_Attend ~ Dodgers$MLB.Wins + Dodgers$MLB.Year+ Dodgers$MLB.Runs.Scored+ Dodgers$MLB.Runs.Against+ Dodgers$MLB.HRs + Dodgers$MLB.Payroll+ Dodgers$MLB.Division.Finish)
summary(LAD_Reg)
MLB_Correlation_LAD<-cor(Dodgers)
MLB_Correlation_LAD
corrplot(MLB_Correlation_LAD, method = "circle")
corrplot(MLB_Correlation_LAD, method = "number", number.cex=0.75, main="Los Angeles Dodgers")


#MIA
Marlins<-MLB_dataframe[293:313,]
MIA_Reg<-lm(Marlins$MLB.Avg_Attend ~ Marlins$MLB.Wins + Marlins$MLB.Year+ Marlins$MLB.Capacity+ Marlins$MLB.Runs.Scored+ Marlins$MLB.Runs.Against+ Marlins$MLB.HRs + Marlins$MLB.Payroll+ Marlins$MLB.Division.Finish)
summary(MIA_Reg)
MLB_Correlation_MIA<-cor(Marlins)
MLB_Correlation_MIA
corrplot(MLB_Correlation_MIA, method = "circle")
corrplot(MLB_Correlation_MIA, method = "number", number.cex=0.75, main="Miami")

#MIL
Brewers<-MLB_dataframe[314:334,]
MIL_Reg<-lm(Brewers$MLB.Avg_Attend ~ Brewers$MLB.Wins + Brewers$MLB.Year+ Brewers$MLB.Capacity+ Brewers$MLB.Runs.Scored+ Brewers$MLB.Runs.Against+ Brewers$MLB.HRs + Brewers$MLB.Payroll+ Brewers$MLB.Division.Finish)
summary(MIL_Reg)
MLB_Correlation_MIL<-cor(Brewers)
MLB_Correlation_MIL
corrplot(MLB_Correlation_MIL, method = "circle")
corrplot(MLB_Correlation_MIL, method = "number", number.cex=0.75, main="Milwaukee")

#MIN
Twins<-MLB_dataframe[335:355,]
MIN_Reg<-lm(Twins$MLB.Avg_Attend ~ Twins$MLB.Wins + Twins$MLB.Year+ Twins$MLB.Capacity+ Twins$MLB.Runs.Scored+ Twins$MLB.Runs.Against+ Twins$MLB.HRs + Twins$MLB.Payroll+ Twins$MLB.Division.Finish)
summary(MIN_Reg)
MLB_Correlation_MIN<-cor(Twins)
MLB_Correlation_MIN
corrplot(MLB_Correlation_MIN, method = "circle")
corrplot(MLB_Correlation_MIN, method = "number", number.cex=0.75, main="Minnesota")


#NYM
Mets<-MLB_dataframe[356:376,]
NYM_Reg<-lm(Mets$MLB.Avg_Attend ~ Mets$MLB.Wins + Mets$MLB.Year+ Mets$MLB.Capacity+ Mets$MLB.Runs.Scored+ Mets$MLB.Runs.Against+ Mets$MLB.HRs + Mets$MLB.Payroll+ Mets$MLB.Division.Finish)
summary(NYM_Reg)
MLB_Correlation_NYM<-cor(Mets)
MLB_Correlation_NYM
corrplot(MLB_Correlation_NYM, method = "circle")
corrplot(MLB_Correlation_NYM, method = "number", number.cex=0.75, main="New York Mets")


#NYY
Yankees<-MLB_dataframe[377:397,]
NYY_Reg<-lm(Yankees$MLB.Avg_Attend ~ Yankees$MLB.Wins + Yankees$MLB.Year+ Yankees$MLB.Capacity+ Yankees$MLB.Runs.Scored+ Yankees$MLB.Runs.Against+ Yankees$MLB.HRs + Yankees$MLB.Payroll+ Yankees$MLB.Division.Finish)
summary(NYY_Reg)
MLB_Correlation_NYY<-cor(Yankees)
MLB_Correlation_NYY
corrplot(MLB_Correlation_NYY, method = "circle")
corrplot(MLB_Correlation_NYY, method = "number", number.cex=0.75, main="New York Yankees")



#OAK
Athletics<-MLB_dataframe[398:418,]
OAK_Reg<-lm(Athletics$MLB.Avg_Attend ~ Athletics$MLB.Wins + Athletics$MLB.Year+ Athletics$MLB.Capacity+ Athletics$MLB.Runs.Scored+ Athletics$MLB.Runs.Against+ Athletics$MLB.HRs + Athletics$MLB.Payroll+ Athletics$MLB.Division.Finish)
summary(OAK_Reg)
MLB_Correlation_OAK<-cor(Athletics)
MLB_Correlation_OAK
corrplot(MLB_Correlation_OAK, method = "circle")
corrplot(MLB_Correlation_OAK, method = "number", number.cex=0.75, main="Oakland")

#PHI
Phillies<-MLB_dataframe[419:439,]
PHI_Reg<-lm(Phillies$MLB.Avg_Attend ~ Phillies$MLB.Wins + Phillies$MLB.Year+ Phillies$MLB.Capacity+ Phillies$MLB.Runs.Scored+ Phillies$MLB.Runs.Against+ Phillies$MLB.HRs + Phillies$MLB.Payroll+ Phillies$MLB.Division.Finish)
summary(PHI_Reg)
MLB_Correlation_PHI<-cor(Phillies)
MLB_Correlation_PHI
corrplot(MLB_Correlation_PHI, method = "circle")
corrplot(MLB_Correlation_PHI, method = "number", number.cex=0.75, main="Philadelphia")

#PIT
Pirates<-MLB_dataframe[440:460,]
PIT_Reg<-lm(Pirates$MLB.Avg_Attend ~ Pirates$MLB.Wins + Pirates$MLB.Year+ Pirates$MLB.Capacity+ Pirates$MLB.Runs.Scored+ Pirates$MLB.Runs.Against+ Pirates$MLB.HRs + Pirates$MLB.Payroll+ Pirates$MLB.Division.Finish)
summary(PIT_Reg)
MLB_Correlation_PIT<-cor(Pirates)
MLB_Correlation_PIT
corrplot(MLB_Correlation_PIT, method = "circle")
corrplot(MLB_Correlation_PIT, method = "number", number.cex=0.75, main="Pittsburgh")


#SD
Padres<-MLB_dataframe[461:481,]
SD_Reg<-lm(Padres$MLB.Avg_Attend ~ Padres$MLB.Wins + Padres$MLB.Year+ Padres$MLB.Capacity+ Padres$MLB.Runs.Scored+ Padres$MLB.Runs.Against+ Padres$MLB.HRs + Padres$MLB.Payroll+ Padres$MLB.Division.Finish)
summary(SD_Reg)
MLB_Correlation_SD<-cor(Padres)
MLB_Correlation_SD
corrplot(MLB_Correlation_SD, method = "circle")
corrplot(MLB_Correlation_SD, method = "number", number.cex=0.75, main="San Diego")

#SF
Giants<-MLB_dataframe[482:502,]
SF_Reg<-lm(Giants$MLB.Avg_Attend ~ Giants$MLB.Wins + Giants$MLB.Year+ Giants$MLB.Capacity+ Giants$MLB.Runs.Scored+ Giants$MLB.Runs.Against+ Giants$MLB.HRs + Giants$MLB.Payroll+ Giants$MLB.Division.Finish)
summary(SF_Reg)
MLB_Correlation_SF<-cor(Giants)
MLB_Correlation_SF
corrplot(MLB_Correlation_SF, method = "circle")
corrplot(MLB_Correlation_SF, method = "number", number.cex=0.75, main="San Francisco")

#SEA
Mariners<-MLB_dataframe[503:523,]
SEA_Reg<-lm(Mariners$MLB.Avg_Attend ~ Mariners$MLB.Wins + Mariners$MLB.Year+ Mariners$MLB.Capacity+ Mariners$MLB.Runs.Scored+ Mariners$MLB.Runs.Against+ Mariners$MLB.HRs + Mariners$MLB.Payroll+ Mariners$MLB.Division.Finish)
summary(SEA_Reg)
MLB_Correlation_SEA<-cor(Mariners)
MLB_Correlation_SEA
corrplot(MLB_Correlation_SEA, method = "circle")
corrplot(MLB_Correlation_SEA, method = "number", number.cex=0.75, main="Seattle")

#STL
Cardinals<-MLB_dataframe[524:544,]
STL_Reg<-lm(Cardinals$MLB.Avg_Attend ~ Cardinals$MLB.Wins + Cardinals$MLB.Year+ Cardinals$MLB.Capacity+ Cardinals$MLB.Runs.Scored+ Cardinals$MLB.Runs.Against+ Cardinals$MLB.HRs + Cardinals$MLB.Payroll+ Cardinals$MLB.Division.Finish)
summary(STL_Reg)
MLB_Correlation_STL<-cor(Cardinals)
MLB_Correlation_STL
corrplot(MLB_Correlation_STL, method = "circle")
corrplot(MLB_Correlation_STL, method = "number", number.cex=0.75, main="St. Louis")

#TB
Rays<-MLB_dataframe_noCapacity[545:563,]
TB_Reg<-lm(Rays$MLB.Avg_Attend ~ Rays$MLB.Wins + Rays$MLB.Year+ Rays$MLB.Runs.Scored+ Rays$MLB.Runs.Against+ Rays$MLB.HRs + Rays$MLB.Payroll+ Rays$MLB.Division.Finish)
summary(TB_Reg)
MLB_Correlation_TB<-cor(Rays)
MLB_Correlation_TB
corrplot(MLB_Correlation_TB, method = "circle")
corrplot(MLB_Correlation_TB, method = "number", number.cex=0.75, main="Tampa Bay")


#TEX
Rangers<-MLB_dataframe[564:584,]
TEX_Reg<-lm(Rangers$MLB.Avg_Attend ~ Rangers$MLB.Wins + Rangers$MLB.Year+ Rangers$MLB.Capacity+ Rangers$MLB.Runs.Scored+ Rangers$MLB.Runs.Against+ Rangers$MLB.HRs + Rangers$MLB.Payroll+ Rangers$MLB.Division.Finish)
summary(TEX_Reg)
MLB_Correlation_TEX<-cor(Rangers)
MLB_Correlation_TEX
corrplot(MLB_Correlation_TEX, method = "circle")
corrplot(MLB_Correlation_TEX, method = "number", number.cex=0.75, main="Texas")

#TOR
BlueJays<-MLB_dataframe_noCapacity[585:605,]
TOR_Reg<-lm(BlueJays$MLB.Avg_Attend ~ BlueJays$MLB.Wins + BlueJays$MLB.Year+ BlueJays$MLB.Runs.Scored+ BlueJays$MLB.Runs.Against+ BlueJays$MLB.HRs + BlueJays$MLB.Payroll+ BlueJays$MLB.Division.Finish)
summary(TOR_Reg)
MLB_Correlation_TOR<-cor(BlueJays)
MLB_Correlation_TOR
corrplot(MLB_Correlation_TOR, method = "circle")
corrplot(MLB_Correlation_TOR, method = "number", number.cex=0.75, main="Toronto")


#WAS
Nationals<-MLB_dataframe[606:626,]
WAS_Reg<-lm(Nationals$MLB.Avg_Attend ~ Nationals$MLB.Wins + Nationals$MLB.Year+ Nationals$MLB.Capacity+ Nationals$MLB.Runs.Scored+ Nationals$MLB.Runs.Against+ Nationals$MLB.HRs + Nationals$MLB.Payroll+ Nationals$MLB.Division.Finish)
summary(WAS_Reg)
MLB_Correlation_WAS<-cor(Nationals)
MLB_Correlation_WAS
corrplot(MLB_Correlation_WAS, method = "circle")
corrplot(MLB_Correlation_WAS, method = "number", number.cex=0.75, main="Washington")


























####JONH_REST_STEPWISE

options(scipen=999)   #Removes scientific notation

#ATLANTA #R^2 = 0.5927
Atlanta<-MLB[1:21,]
ATL_Reg2<-lm(Atlanta$Avg_Attend~ Atlanta$Wins+ Atlanta$Runs_Scored+ 
               Atlanta$Runs_Against+ Atlanta$HRs + Atlanta$CPI_Pay_MM+ 
               Atlanta$Division_Finish)
StepAIC_ATL=stepAIC(ATL_Reg2, direction="both")
StepAIC_ATL$anova
summary(StepAIC_ATL)

#ARIZONA #R^2 = 0.1136
Arizona<-MLB[22:40,]
ARI_Reg2<-lm(Arizona$Avg_Attend~ Arizona$Wins+ Arizona$Runs_Scored+ 
               Arizona$Runs_Against+ Arizona$HRs + Arizona$CPI_Pay_MM+ 
               Arizona$Division_Finish)
StepAIC_ARI=stepAIC(ARI_Reg2, direction="both")
StepAIC_ARI$anova
summary(StepAIC_ARI)



###BAL #R^2 = 0.409

Baltimore<-MLB[41:61,]
BAL_Reg2<-lm(Baltimore$Avg_Attend~ Baltimore$Wins+ Baltimore$Runs_Scored+ 
               Baltimore$Runs_Against+ Baltimore$HRs + Baltimore$CPI_Pay_MM+ 
               Baltimore$Division_Finish)
StepAIC_BAL=stepAIC(BAL_Reg2, direction="both")
StepAIC_BAL$anova
summary(StepAIC_BAL)



##Bos #R^2 = 0.67
Boston<-MLB[62:82,]
BOS_Reg2<-lm(Boston$Avg_Attend~ Boston$Wins+ Boston$Runs_Scored+ 
               Boston$Runs_Against+ Boston$HRs + Boston$CPI_Pay_MM+ 
               Boston$Division_Finish)
StepAIC_BOS=stepAIC(BOS_Reg2, direction="both")
StepAIC_BOS$anova
summary(StepAIC_BOS)

#CHC #R^2 = 0.7544

Cubs<-MLB[83:103,]
CHC_Reg2<-lm(Cubs$Avg_Attend~ Cubs$Wins+ Cubs$Runs_Scored+ 
               Cubs$Runs_Against+ Cubs$HRs + Cubs$CPI_Pay_MM+ 
               Cubs$Division_Finish)
StepAIC_CHC=stepAIC(CHC_Reg2, direction="both")
StepAIC_CHC$anova
summary(StepAIC_CHC)

#CHW #R^2 = 0.6069

WhiteSox<-MLB[104:124,]
CHW_Reg2<-lm(WhiteSox$Avg_Attend~ WhiteSox$Wins+ WhiteSox$Runs_Scored+ 
               WhiteSox$Runs_Against+ WhiteSox$HRs + WhiteSox$CPI_Pay_MM+ 
               WhiteSox$Division_Finish)
StepAIC_CHW=stepAIC(CHW_Reg2, direction="both")
StepAIC_CHW$anova
summary(StepAIC_CHW)

#CIN #R^2 = 0.3237
Reds<-MLB[125:145,]
CIN_Reg2<-lm(Reds$Avg_Attend~ Reds$Wins+ Reds$Runs_Scored+ 
               Reds$Runs_Against+ Reds$HRs + Reds$CPI_Pay_MM+ 
               Reds$Division_Finish)
StepAIC_CIN=stepAIC(CIN_Reg2, direction="both")
StepAIC_CIN$anova
summary(StepAIC_CIN)


#CLE #R^2 = 0.82
Indians<-MLB[146:166,]
CLE_Reg2<-lm(Indians$Avg_Attend~ Indians$Wins+ Indians$Runs_Scored+ 
               Indians$Runs_Against+ Indians$HRs + Indians$CPI_Pay_MM+ 
               Indians$Division_Finish)
StepAIC_CLE=stepAIC(CLE_Reg2, direction="both")
StepAIC_CLE$anova
summary(StepAIC_CLE)




#COL #R^2 = 0.4877
Rockies<-MLB[167:187,]
COL_Reg2<-lm(Rockies$Avg_Attend~ Rockies$Wins+ Rockies$Runs_Scored+ 
               Rockies$Runs_Against+ Rockies$HRs + Rockies$CPI_Pay_MM+ 
               Rockies$Division_Finish)
StepAIC_COL=stepAIC(COL_Reg2, direction="both")
StepAIC_COL$anova
summary(StepAIC_COL)

#DET #R^2 = 0.8562
Tigers<-MLB[188:208,]
DET_Reg2<-lm(Tigers$Avg_Attend~ Tigers$Wins+ Tigers$Runs_Scored+ 
               Tigers$Runs_Against+ Tigers$HRs + Tigers$CPI_Pay_MM+ 
               Tigers$Division_Finish)
StepAIC_DET=stepAIC(DET_Reg2, direction="both")
StepAIC_DET$anova
summary(StepAIC_DET)

#HOU #R^2 = 0.8264
Astros<-MLB[209:229,]
HOU_Reg2<-lm(Astros$Avg_Attend~ Astros$Wins+ Astros$Runs_Scored+ 
               Astros$Runs_Against+ Astros$HRs + Astros$CPI_Pay_MM+ 
               Astros$Division_Finish)
StepAIC_HOU=stepAIC(HOU_Reg2, direction="both")
StepAIC_HOU$anova
summary(StepAIC_HOU)

#KC #R^2 = 0.8686
Royals<-MLB[230:250,]
KC_Reg2<-lm(Royals$Avg_Attend~ Royals$Wins+ Royals$Runs_Scored+ 
              Royals$Runs_Against+ Royals$HRs + Royals$CPI_Pay_MM+ 
              Royals$Division_Finish)
StepAIC_KC=stepAIC(KC_Reg2, direction="both")
StepAIC_KC$anova
summary(StepAIC_KC)

#LAA #R^2 = 0.8643
Angel<-MLB[251:271,]
LAA_Reg2<-lm(Angel$Avg_Attend~ Angel$Wins+ Angel$Runs_Scored+ 
               Angel$Runs_Against+ Angel$HRs + Angel$CPI_Pay_MM +
               Angel$Division_Finish)
StepAIC_LAA=stepAIC(LAA_Reg2, direction="both")
StepAIC_LAA$anova
summary(StepAIC_LAA)



#LAD #R^2 = 0.5157
Dodgers<-MLB[272:292,]
LAD_Reg2<-lm(Dodgers$Avg_Attend~ Dodgers$Wins+ Dodgers$Runs_Scored+ 
               Dodgers$Runs_Against+ Dodgers$HRs + Dodgers$CPI_Pay_MM+ 
               Dodgers$Division_Finish)
StepAIC_LAD=stepAIC(LAD_Reg2, direction="both")
StepAIC_LAD$anova
summary(StepAIC_LAD)


#MIA #R^2 = 0.4561
Marlins<-MLB[293:313,]
MIA_Reg2<-lm(Marlins$Avg_Attend~ Marlins$Wins+ Marlins$Runs_Scored+ 
               Marlins$Runs_Against+ Marlins$HRs + Marlins$CPI_Pay_MM+ 
               Marlins$Division_Finish)
StepAIC_MIA=stepAIC(MIA_Reg2, direction="both")
StepAIC_MIA$anova
summary(StepAIC_MIA)

#MIL #R^2 = 0.8019
Brewers<-MLB[314:334,]
MIL_Reg2<-lm(Brewers$Avg_Attend~ Brewers$Wins+ Brewers$Runs_Scored+ 
               Brewers$Runs_Against+ Brewers$HRs + Brewers$CPI_Pay_MM+ 
               Brewers$Division_Finish)
StepAIC_MIL=stepAIC(MIL_Reg2, direction="both")
StepAIC_MIL$anova
summary(StepAIC_MIL)

#MIN #R^2 = 0.7749
Twins<-MLB[335:355,]
MIN_Reg2<-lm(Twins$Avg_Attend~ Twins$Wins+ Twins$Runs_Scored+ 
               Twins$Runs_Against+ Twins$HRs + Twins$CPI_Pay_MM+ 
               Twins$Division_Finish)
StepAIC_MIN=stepAIC(MIN_Reg2, direction="both")
StepAIC_MIN$anova
summary(StepAIC_MIN)


#NYM #R^2 = 0.7475
Mets<-MLB[356:376,]
NYM_Reg2<-lm(Mets$Avg_Attend~ Mets$Wins+ Mets$Runs_Scored+ 
               Mets$Runs_Against+ Mets$HRs + Mets$CPI_Pay_MM+ 
               Mets$Division_Finish)
StepAIC_NYM=stepAIC(NYM_Reg2, direction="both")
StepAIC_NYM$anova
summary(StepAIC_NYM)



#NYY #R^2 = 0.8097
Yankees<-MLB[377:397,]
NYY_Reg2<-lm(Yankees$Avg_Attend~ Yankees$Wins+ Yankees$Runs_Scored+ 
               Yankees$Runs_Against+ Yankees$HRs + Yankees$CPI_Pay_MM+ 
               Yankees$Division_Finish)
StepAIC_NYY=stepAIC(NYY_Reg2, direction="both")
StepAIC_NYY$anova
summary(StepAIC_NYY)



#OAK #R^2 = 0.7183
Athletics<-MLB[398:418,]
OAK_Reg2<-lm(Athletics$Avg_Attend~ Athletics$Wins+ Athletics$Runs_Scored+ 
               Athletics$Runs_Against+ Athletics$HRs + Athletics$CPI_Pay_MM+ 
               Athletics$Division_Finish)
StepAIC_OAK=stepAIC(OAK_Reg2, direction="both")
StepAIC_OAK$anova
summary(StepAIC_OAK)

#PHI #R^2 = 0.9082
Phillies<-MLB[419:439,]
PHI_Reg2<-lm(Phillies$Avg_Attend~ Phillies$Wins+ Phillies$Runs_Scored+ 
               Phillies$Runs_Against+ Phillies$HRs + Phillies$CPI_Pay_MM+ 
               Phillies$Division_Finish)
StepAIC_PHI=stepAIC(PHI_Reg2, direction="both")
StepAIC_PHI$anova
summary(StepAIC_PHI)

#PIT #R^2 = 0.6737
Pirates<-MLB[440:460,]
PIT_Reg2<-lm(Pirates$Avg_Attend~ Pirates$Wins+ Pirates$Runs_Scored+ 
               Pirates$Runs_Against+ Pirates$HRs + Pirates$CPI_Pay_MM+ 
               Pirates$Division_Finish)
StepAIC_PIT=stepAIC(PIT_Reg2, direction="both")
StepAIC_PIT$anova
summary(StepAIC_PIT)


#SD #R^2 = 0.5405
Padres<-MLB[461:481,]
SD_Reg2<-lm(Padres$Avg_Attend~ Padres$Wins+ Padres$Runs_Scored+ 
              Padres$Runs_Against+ Padres$HRs + Padres$CPI_Pay_MM+ 
              Padres$Division_Finish)
StepAIC_SD=stepAIC(SD_Reg2, direction="both")
StepAIC_SD$anova
summary(StepAIC_SD)

#SF #R^2 = 0.6454
Giants<-MLB[482:502,]
SF_Reg2<-lm(Giants$Avg_Attend~ Giants$Wins+ Giants$Runs_Scored+ 
              Giants$Runs_Against+ Giants$HRs + Giants$CPI_Pay_MM+ 
              Giants$Division_Finish)
StepAIC_SF=stepAIC(SF_Reg2, direction="both")
StepAIC_SF$anova
summary(StepAIC_SF)

#SEA #R^2 = 0.7448
Mariners<-MLB[503:523,]
SEA_Reg2<-lm(Mariners$Avg_Attend~ Mariners$Wins+ Mariners$Runs_Scored+ 
               Mariners$Runs_Against+ Mariners$HRs + Mariners$CPI_Pay_MM+ 
               Mariners$Division_Finish)
StepAIC_SEA=stepAIC(SEA_Reg2, direction="both")
StepAIC_SEA$anova
summary(StepAIC_SEA)

Mariners<-MLB[503:523,]
SEA_Reg2<-lm(Mariners$Attendance~ Mariners$Wins+ Mariners$Runs_Scored+ 
               Mariners$Runs_Against+ Mariners$HRs + Mariners$CPI_Pay_MM+ 
               Mariners$Division_Finish)
StepAIC_SEA=stepAIC(SEA_Reg2, direction="both")
StepAIC_SEA$anova
summary(StepAIC_SEA)

#STL #R^2 = 0.5142
Cardinals<-MLB[524:544,]
STL_Reg2<-lm(Cardinals$Avg_Attend~ Cardinals$Wins+ Cardinals$Runs_Scored+ 
               Cardinals$Runs_Against+ Cardinals$HRs + Cardinals$CPI_Pay_MM+ 
               Cardinals$Division_Finish)
StepAIC_STL=stepAIC(STL_Reg2, direction="both")
StepAIC_STL$anova
summary(StepAIC_STL)

#TB #R^2 = 0.1376
Rays<-MLB[545:563,]
TB_Reg2<-lm(Rays$Avg_Attend~ Rays$Wins+ Rays$Runs_Scored+ 
              Rays$Runs_Against+ Rays$HRs + Rays$CPI_Pay_MM+ 
              Rays$Division_Finish)
StepAIC_TB=stepAIC(TB_Reg2, direction="both")
StepAIC_TB$anova
summary(StepAIC_TB)


#TEX #R^2 = 0.4339
Rangers<-MLB[564:584,]
TEX_Reg2<-lm(Rangers$Avg_Attend~ Rangers$Wins+ Rangers$Runs_Scored+ 
               Rangers$Runs_Against+ Rangers$HRs + Rangers$CPI_Pay_MM+ 
               Rangers$Division_Finish)
StepAIC_TEX=stepAIC(TEX_Reg2, direction="both")
StepAIC_TEX$anova
summary(StepAIC_TEX)

#TOR #R^2 = 0.3113
BlueJays<-MLB[585:605,]
TOR_Reg2<-lm(BlueJays$Avg_Attend~ BlueJays$Wins+ BlueJays$Runs_Scored+ 
               BlueJays$Runs_Against+ BlueJays$HRs + BlueJays$CPI_Pay_MM+ 
               BlueJays$Division_Finish)
StepAIC_TOR=stepAIC(TOR_Reg2, direction="both")
StepAIC_TOR$anova
summary(StepAIC_TOR)


#WAS #R^2 = 0.5407
Nationals<-MLB[606:626,]
WAS_Reg2<-lm(Nationals$Avg_Attend~ Nationals$Wins+ Nationals$Runs_Scored+ 
               Nationals$Runs_Against+ Nationals$HRs + Nationals$CPI_Pay_MM+ 
               Nationals$Division_Finish)
StepAIC_WAS=stepAIC(WAS_Reg2, direction="both")
StepAIC_WAS$anova
summary(StepAIC_WAS)


#SEA #R^2 = 0.7448
#PHI #R^2 = 0.9082
#OAK #R^2 = 0.7183
#NYY #R^2 = 0.8097
#NYM #R^2 = 0.7475
#MIN #R^2 = 0.7749
#MIL #R^2 = 0.8019
#LAA #R^2 = 0.8643
#KC #R^2 = 0.8686
#HOU #R^2 = 0.8264
#DET #R^2 = 0.8562
#CLE #R^2 = 0.82
#CHC #R^2 = 0.7544
