b <- Continuous$new(name="Age", x=titanic$Age, perf=Binary_Performance$new(y=titanic$Survived))
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


d <- Discrete$new(name="Pclass", x=titanic$Pclass, perf=Binary_Performance$new(y=titanic$Survived))
d$bin()
d$neutralize(1:3)
d$reset()
d$collapse(1:2)



