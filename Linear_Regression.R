library(ggplot2)
library(dplyr)
NHLData <- read.csv("NHLTop100csv.csv")
print(NHLData)
summary(NHLData)

#Part A

NHLPredict <- (lm(A ~ G, data = NHLData))

summary(NHLPredict)
coefs <- coef(NHLPredict)
coefs

plot(NHLData$G, NHLData$A, xlim = range(1:1000), ylim = range(1:2000), pch=20,col ="red", xlab = "Goals", ylab="Assists", main = "NHL top 100")
with(subset(NHLData,Player=="Wayne Gretzky"),text(G,A,Player))
abline(coefs[1],coefs[2], col = 3, lwd = 3)



#Part B: Gretzky 894 goals 1963 assists

WayneGretzky <- lm(I(A - 1963) ~ I(G - 894) + 0, data = NHLData)
summary(WayneGretzky)
coef(WayneGretzky)
inter <- 1963 - coef(WayneGretzky) * 894 
abline(inter, coef(WayneGretzky), col = 4, lwd = 2)

#Part C: Patrick Kane(current player)

#Rank- 101, Name - Patrick Kane, Team - CHI, Position - R, FirstNHL - 2007-2008, LastNHL - NA, GP - 735, Goals - 285, Assists - 462, Pts - 747, +/- = 81, PIM - 284, PP - 164, SH - 0, GW - 49, GT - NA, OT - NA, Shots - 2290)
#Patrick_Kane <- list(101, "Patrick Kane", "CHI", "R", "2007-2008", NA, 735, 285, 462, 747, 81, 284, 164, 0, 49, NA, NA, 2290)

Patrick_Kane <- data.frame(101,"Patrick Kane", "CHI", "R", "2007-2008", NA, 735, 285, 462, 747, 81, 284, 164, 0, 49, NA, NA, 2290)
names(Patrick_Kane) <- c("Rank","Player","Team", "Pos", "X1st.NHL.Season", "Last.NHL.Season", "GP","G", "A", "P" ,"X...", "PIM", "PP","SH", "GW", "GT" ,"OT", "Shots")
Patrick_Kane

NHLData1 <- rbind(NHLData, Patrick_Kane)
#add his data into the NHLtop100 players data frame. No idea why his name doesn't show up

Patrick_Kane_regression <- lm(I(A - 462) ~ I(G - 285) + 0, data = NHLData1)
summary(Patrick_Kane_regression)
coef(Patrick_Kane_regression)

plot(NHLData1$G, NHLData1$A, xlim = range(1:1000), ylim = range(1:2000), pch=20,col ="red", xlab = "Goals", ylab="Assists", main = "NHL top 100")
with(subset(NHLData1,Player=="Patrick Kane"),text(G,A,Player))
inter1 <- 462 - coef(Patrick_Kane_regression) * 285
abline(inter1, coef(Patrick_Kane_regression), col = 5, lwd = 3)

#Part D: Do Make models go through origin and the points
plot(NHLData1$G, NHLData1$A, xlim = range(0:1000), ylim = range(0:2000), pch=20,col ="red", xlab = "Goals", ylab="Assists", main = "NHL top 100")
abline(0,coefs[2], col = 3, lwd = 3)
abline(0, coef(WayneGretzky), col = 4, lwd = 2)
abline(0, coef(Patrick_Kane_regression), col = 5, lwd = 3)

summary()

#Part E:

Multiple_R_Squared <- c(0.1082,0.9091,0.5759)
Adjusted_R_Squared <- c(0.09913,0.9082,0.5717)
P_Value <- c(0.0008317, 2.2e-16, 2.2e-16)
D_Values <-c(0.09913,0.9082,0.5717)

final_df <- data.frame(Multiple_R_Squared, Adjusted_R_Squared, P_Value, D_Values)
final_df

