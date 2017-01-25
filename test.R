b <- Continuous$new(name="Age", x=titanic$Age, perf=Binary_Performance$new(y=titanic$Survived))
b$bin()
b$bin(mono=2)
b$update()



d <- Discrete$new(name="Pclass", x=titanic$Pclass, perf=Binary_Performance$new(y=titanic$Survived))
d$bin()
d$bin()
d$update()
