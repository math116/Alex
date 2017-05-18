AA<-tbl_df(boi_120_data)
require(dplyr)
day1<- filter(AA,day=="one")
day2<- filter(AA,day=="two")
day3<- filter(AA,day=="three")
modAnova1=aov(scans~species,data=day1)
anova(modAnova1)
TukeyHSD(modAnova1)
plot(TukeyHSD(modAnova1))

modAnova2=aov(scans~species,data=day2)
anova(modAnova2)
TukeyHSD(modAnova2)
plot(TukeyHSD(modAnova2))

modAnova3=aov(scans~species,data=day3)
anova(modAnova3)
TukeyHSD(modAnova3)
plot(TukeyHSD(modAnova3))

coots<- filter(AA,species=="coot")
ducks<- filter(AA,species=="duck")
geese<- filter(AA,species=="goose")

modAnova4=aov(scans~day,data=coots)
anova(modAnova4)
TukeyHSD(modAnova4)
plot(TukeyHSD(modAnova4))


modAnova5=aov(scans~day,data=ducks)
anova(modAnova5)
TukeyHSD(modAnova5)
plot(TukeyHSD(modAnova5))

modAnova6=aov(scans~day,data=geese)
anova(modAnova6)
TukeyHSD(modAnova6)
plot(TukeyHSD(modAnova6))