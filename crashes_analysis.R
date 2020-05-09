install.packages("MASS")
library("ggplot2")
library("MASS")

sum(crashesDeer$Injury.Count) # 29,173
sum(crashesDeer$Fatality.Count) # 233
sum(crashesDeer$Vehicle.Towed) # 84,233

summary(crashesDeer$Injury.Count)
# Min.    1st Qu.  Median  Mean   3rd Qu. Max. 
# 0.0000  0.0000  0.0000  0.3316 1.0000  9.0000

summary(crashesDeer$Fatality.Count)
#   Min.     1st Qu.   Median   Mean    3rd Qu.    Max. 
# 0.000000 0.000000 0.000000 0.002648 0.000000 3.000000 

# alpha = 0.05

tblIllum <- table(crashesDeer$Sudden.Deer, crashesDeer$Illumination.Dark)
chisq.test(tblIllum)
# X-squared = 20.417, df = 1, p-value = 6.228e-06
# 6.228x10^-6 is less than alpha, therefore reject the null hypothesis. "Sudden Deer" is not 
# independent of a lack of illumination.

tblFatigue <- table(crashesDeer$Sudden.Deer, crashesDeer$Fatigue_Asleep)
chisq.test(tblFatigue)
# X-squared = 0.26459, df = 1, p-value = 0.607
# 0.607 is greather than alpha, so we fail to reject the null hypothesis that "Sudden Deer" is 
# independent of the driver's fatigue level

tblCell <- table(crashesDeer$Sudden.Deer, crashesDeer$Cell.Phone)
chisq.test(tblCell)
# X-squared = 4.982, df = 1, p-value = 0.02561
# 0.02561 is less than alpha, so we reject the null hypothesis. "Sudden Deer" is not
# independent of whether the driver is using their cell phone at the time of the crash

tblHitCell <- table(crashesDeer$Hit.Deer, crashesDeer$Cell.Phone)
chisq.test(tblHitCell)
# X-squared = 28.315, df = 1, p-value = 1.031e-07
# 1.031x10^-7 is less than 0.05 so we reject the null hypothesis. Whether or not the driver
# hit the deer is not independent of whether they were using their cell phone. 

ggplot(crashesDeer, aes(x = Injury.Count)) +
  geom_histogram(fill = "purple", bins = 10) + 
  facet_grid(Sudden.Deer ~ Urban_Rural) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(name = "Injury Count", breaks = seq(0, 10, 2)) +
  ylab("Frequency")

ggplot(crashesDeer, aes(x = Injury.Count)) +
  geom_histogram(fill = "blue",bins = 10) + 
  facet_grid(Hit.Deer ~ Urban_Rural) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(name = "Injury Count", breaks = seq(0, 10, 2)) +
  ylab("Frequency")

sixteen <- sum(crashesDeer$X16.Year.Old.Driver.Count)
seventeen <- sum(crashesDeer$X17.Year.Old.Driver.Count)
eighteen <- sum(crashesDeer$X18.Year.Old.Driver.Count)
nineteen <- sum(crashesDeer$X19.Year.Old.Driver.Count)
twenty <- sum(crashesDeer$X20.Year.Old.Driver.Count)
sixteenToTwenty <- (sixteen + seventeen + eighteen + nineteen + twenty) / 87976 * 100
twentyone <- 100 - sum(sixteen, seventeen, eighteen, nineteen, twenty, fifty, sixtyfive, seventyfive)
fifty <- sum(crashesDeer$X50.64.Year.Old.Driver.Count) / 87976  * 100
sixtyfive <- sum(crashesDeer$X65.74.Year.Old.Driver.Count) / 87976  * 100
seventyfive <- sum(crashesDeer$X75.Plus.Year.Old.Driver.Count) / 87976  * 100

twentyone

ages <- c(sixteenToTwenty, twentyone, fifty, sixtyfive, seventyfive)
labels <- c("16-20", "21-49", "50-64", "65-74", ">= 75")
pie(ages, labels = format(round(ages, 2), nsmall = 2), col=c("chartreuse3", "cadetblue1" 
                            , "coral", "deeppink2", "blueviolet"),  main ="Percentage of Deer Related Crashes By Age")
legend("right", legend=labels, fill =c("chartreuse3", "cadetblue1" , "coral", "deeppink2", "blueviolet")
       , title = "Ages", box.lty =0)


