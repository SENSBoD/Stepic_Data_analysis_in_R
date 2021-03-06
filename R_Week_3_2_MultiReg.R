# multiple linear regression

?swiss

swiss <- data.frame(swiss)
str(swiss)
hist(swiss$Fertility, col = 'red')

# numeric predictors (перелбачаємо народжуваність)
fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

confint(fit2) # довірчі інтервали для моделі

# �������� ������� fill_na, ������� ��������� �� ���� ������ � ����� �����������:
# x_1  -  �������� ������
# x_2 - �������� ������
# y - �������� ������ � ������������ ����������.
# ������ � ����� ����������. �� ������ �����, ��������� ������ ����������, � ������� ��� ����������� ��������, 
# �� �������� ������������� ������ (��� ��������������), ���  y � ��������� ����������, x_1 � x_2 � ����������� ����������. 
# �����, ��������� ����������� ������, �� �������� ����������� �������� �������������� ������.
# ������� ������ ���������� dataframe c ����� ����������  y_full. ��������� � ��� ���������� y, 
# � ������� ����������� �������� ��������� �������������� ���������� ����������� ������.
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na <- function(x){
  x_known <- subset(x, y != "NA")
  fit <- lm(y ~ x_1 + x_2, data = x_known)
  x$y_full <- ifelse(!is.na(x$y), x$y, predict(fit, x))
  return(x)
}
fill_na(test_data)

# � ���������� df �������� subset ������ mtcars ������ � ����������� "wt", "mpg", "disp", "drat", "hp". 
# �������������� ������������� ������������� ��������, ����� ����������� ��� ������ (���������� "wt"). 
# �������� ����� ���������� ����������� ���������� (�� "mpg", "disp", "drat", "hp"), ����� �������� R^2 adjusted ���� ����������. 
# �������������� �������� ��������� �� ����. 
df <- mtcars[, c("wt", "mpg", "disp", "drat", "hp")]

fit <- lm(wt ~ mpg + disp + drat + hp, data = df)
summary(fit)

fit2 <- lm(wt ~ mpg + disp + hp, data = df)
summary(fit2)

#library(corrplot)
#M <- cor(df)
#corrplot(M)

library(PerformanceAnalytics)
chart.Correlation(df)

library(leaps)
x <- regsubsets(wt ~ mpg + disp + drat + hp, df)
plot(x, scale = "adjr2")                
                
# �������������� ���������� ��������� attitude, ����� ����������� ������� (rating) �� ���������� complaints � critical. 
# ������ t-�������� ��� �������������� ���� ��������?
?attitude
df2 <- attitude
fit3 <- lm(rating ~ complaints * critical, data = df2)
summary(fit3)


# categorical predictors
hist(swiss$Catholic, col = 'red')
swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  #geom_smooth()
  geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)

# �������������� �������������� ���������� wt � am, �������� ���, ���������� � �������:
# ��� x - ���������� wt
#� �� y - ���������� mpg
# ���� ������������� ������ - ���������� am
df3 <- mtcars
df3$am <- factor(df3$am, labels = c('Automatic', 'Manual'))

fit6 <- lm(mpg ~ wt*am, data = df3)
summary(fit6)

my_plot <- ggplot(df3, aes(y = mpg, x = wt, col = am))+
  #geom_point()+
  geom_smooth(method = 'lm')

# model comparison
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss) # всі предиктори (+)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

# model selection
optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)

# C ������� ������� step ������� ����������� ������ ��� ������������ rating � �������� attitude. Model_full � model_null ��� �������. 
# ��������� ������� � �������� step � ���������� ideal_model.
attitude <- data.frame(attitude)

model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)

ideal_model <- step(model_full, direction = 'backward')
summary(ideal_model)

# �������� ������ ������ �� ����������� ����� � ����������� ������ � ������� ������� anova. ������� ������������ F-��������.
anova(model_full, ideal_model)

# ���������� ������������� � ����������� ��������� ������. � ���� ������ ����� �������� �� ���������� ��������� LifeCycleSavings. 
# ���������� ����������� �������� sr �� ������ ���� ��������� ���������� � ���� ��������. 
# ��������� ������� ���������� ������ � �������� �������, 
# ������� ������ �������� ��������� � �������� ��������� � ����� ���������� ���������������� ������� ������. 
# ��������� ������ � ���������� model.
LifeCycleSavings <- data.frame(LifeCycleSavings)
model <- lm(sr ~ (.)^2, LifeCycleSavings)
