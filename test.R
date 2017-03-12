## when bin is called again, need to overriwte args

x <- titanic
for (i in 1:10) x <- rbind(x, x)

perf=Binary_Performance$new(y=x$Survived)
perf$split(seg=titanic$Sex)
perf$copy()
b <- Continuous$new(name="Fare", x=x$Fare, perf=Binary_Performance$new(y=x$Survived))
b$bin()
b$neutralize(2:5)
b$expand(7)
b$collapse(1:3)
b$mono(2)
b$exceptions(c(24,28))
b$reset()
b$bin(mono=2, exceptions=c(24, 28), min.res=20)

b$mono(0)
b$save_to_disk("test.rds")

neutralize_(b$tf, 2:3)

levels(titanic$Embarked)[levels(titanic$Embarked) == ""] <- NA
classing <- Classing$new(titanic, performance=Binary_Performance$new(y=titanic$Survived))
classing$bin(mono = 2, exceptions = -1)


saveRDS(classing, "classing.rds")
classing <- readRDS("classing.rds")


d <- Discrete$new(name="Embarked", x=titanic$Embarked, perf=Binary_Performance$new(y=titanic$Survived))
d$bin(exceptions=-1)
d$predict()
d$neutralize(1:3)
d$reset()
d$collapse(1:2)

x <- titanic
levels(x$Embarked)[levels(x$Embarked) == ""] <- "S"

sc <- bin(x, y=x$Survived, w=x$Fare)
adjust(sc)
fit(sc)

sc <- rubbish:::Scorecard$new(d=x, performance=Binary_Performance$new(y=x$Survived))
sc$bin()
clust <- sc$cluster()

sc$fit("model 1", "initial fit of model", nfolds=3)
sc$fit("model 2", nfolds=5, lower.limits=-Inf)
sc$fit("model 3", nfolds=10)

sc$bin(mono=2, min.res=10)
sc$fit("model 7", nfolds=10)

sc$drop(c("Sex", "Fare"))
sc$fit("model 8", nfolds=10)

sc2 <- sc$branch("model 3")
sc2$fit("separate model", nfolds=10, alpha=1)

sc$predict()

sc$step["Embarked"] <- 3
