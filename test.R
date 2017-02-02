b <- Continuous$new(name="Age", x=titanic$Age, perf=Binary_Performance$new(y=titanic$Survived))
b$bin()
b$neutralize(2:5)
b$expand(5)
b$collapse(1:3)


b$bin(mono=2, exceptions=c(24, 28))

neutralize_(b$tf, 2:3)

classing <- Classing$new(titanic, performance=Binary_Performance$new(y=titanic$Survived))
classing$bin(mono = 2, exceptions = -1)

d <- Discrete$new(name="Pclass", x=titanic$Pclass, perf=Binary_Performance$new(y=titanic$Survived))
d$bin()
d$neutralize(1:3)
