dat <- read.csv("C:/Users/melanie.harsch/documents/fisheyeapp/Analytics  20160115-20160214.csv")
tapply(dat$TotalEvents[dat$Month=="February"], dat$Day[dat$Month=="February"], sum)

par(mfrow=c(2,2))
plot(tapply(dat$Users[dat$Month=="February"],dat$Day[dat$Month=="February"],sum), type="l", ylab="No. users",xlab="Days, February")
plot(tapply((dat$PercNewSessions[dat$Month=="February"&dat$Users>0])*100, dat$Day[dat$Month=="February"&dat$Users>0], sum)/tapply(dat$Users[dat$Month=="February"&dat$Users>0], dat$Day[dat$Month=="February"&dat$Users>0], length), type="l",ylab="% New Sessions", xlab="Days, February")
plot(tapply(dat$TotalEvents[dat$Month=="February"], dat$Day[dat$Month=="February"], sum), type="l",ylab="Total Number of downloads", xlab="Days, February")

plot(dat$AvgSessionDuration[dat$Month=="February"]~dat$Day[dat$Month=="February"], type="l",ylab="Average Session duration (minutes, seconds)", xlab="Days, February")


