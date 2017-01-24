# continuous$methods("Bin" = callSuper())
data(titanic)
b <- Continuous$new(x=titanic$Fare, perf=new("Binary_Performance", y=titanic$Survived))
b$bin(exceptions=8.05)
b$factorize()


d <- Discrete$new(x=titanic$Pclass, perf=new("Binary_Performance", y=titanic$Survived))
d$bin()
d$collapse(2:3)
d$factorize()


b$perf$y <- b$perf$y + 10


# b <- Continuous$new(x=titanic$Fare, Perf$new(y=titanic$Survived))
b$bin(exceptions = c(8.05))
b$collapse(2:7)
b$expand(3)


#levels(titanic$Embarked)[1] <- NA
d <- Discrete$new(x=titanic$Pclass, y=titanic$Survived)
d$bin()
d$collapse(2:3)
d$expand(2)


# d$Bin()
