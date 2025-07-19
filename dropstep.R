#Pick sample size
n<-300

#How many variables do you want?
vars<-5

#Generate data
d <- as.data.frame(matrix(rnorm(n * vars), nrow = n, ncol = vars))

#Name the variables as you like
names(d)<-c("altruism","testosterone","cortisol","openness","attachment")

m <- lm(altruism ~ testosterone*cortisol*openness*attachment, data = d)
summary(m)

drop1(m, test="F")

#Drop stepwise
m_step <- step(m, direction = "backward")
summary(m_step)

