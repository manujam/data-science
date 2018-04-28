simple_lr <- read.csv("Simple_LR.csv")

model_1 <- glm(Diabetes~.,simple_lr,family = "binomial")

summary(model_1)

