library(ggplot2)

my_df <- read.csv("E:/Install/Statistics/R/train.csv", sep=";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

fit  <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

fit$coefficients

exp(fit$coefficients)

head(predict(object = fit)) # передбачені логарифми йморвіностей
head(predict(object = fit, type = "response")) # передбачені ймовірності

my_df$prob  <- predict(object = fit, type = "response")

# Используем данные mtcars. Сохраните в переменную логистическую регрессионную модель, 
# где в качестве зависимой переменной выступает тип коробки передач (am), в качестве предикторов переменные disp, vs, mpg.
# Значения коэффициентов регрессии сохраните в переменную log_coef.
df <- mtcars
fit  <- glm(am ~ disp + vs + mpg, df, family = "binomial")
log_coef <- fit$coefficients

# Дополните предложенный в задании код, чтобы построить следующий график по данным ToothGrowth.
# Изобразите различия длины зубов морских свинок в различных условиях дозировки и типа потребляемого продукта.
# По оси x - переменная supp.
# По оси y - переменная len.
# Цвет ящиков с усами (boxplot) - переменная dose.
df_2 <- ToothGrowth
str(df_2)
obj <- ggplot(data = ToothGrowth, aes(supp, len, fill = factor(dose), col = factor(dose)))+
  geom_boxplot(col = "black")
obj


library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))

auc  <- performance(pred_fit, measure = "auc") # площа під кривою ROC
str(auc)

# spec - наскільки добре передбачається негативний результат подій:
# передбачили, що не получить диплом і він не получив
perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
# sens - наскільки добре передбачається позитивний результат подій:
# передбачили, що получить диплом і він получив
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
# acc - середнє для двох параметрів
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)

my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)

ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot(binwidth = 0.03)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

mean(my_df$correct)

test_df  <- read.csv("E:/Install/Statistics/R/test.csv", sep = ";")

test_df$hon  <- NA

test_df$hon_predict <- predict(fit, newdata = test_df, type = "response")
View(test_df)

#Exercises
df  <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv")
df
str(df)

df_known <- subset(df, admit != "NA")
df_NA <- subset(df, is.na(admit))
str(df_known)
str(df_NA)

fit  <- glm(admit ~ rank * gpa, df_known, family = "binomial")
fit$coefficients

head(predict(object = fit, type = "response"))

df_known$prob <- predict(object = fit, type = "response")
df_NA$prob_predict <- predict(fit, newdata = df_NA, type = "response")
df_NA$admit <- ifelse(df_NA$prob_predict > 0.4, 1, 0)

sum(df_NA$admit)
