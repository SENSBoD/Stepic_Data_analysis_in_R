# Categorical data

df <- read.csv("grants.csv")

str(df)

df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded")) # те саме що дві попередні строки

# 1d Table
t1 <- table(df$status) #аналог pivot table
t1

dim(t1) # розмірність таблиці (в даному випадку - одновимірна)

# 2d Table
t2 <- table(df$status, df$field)
t2

t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2) # показує проценти значень в таблиці (разом 100%)

prop.table(t2, 1) # 100% по строці
prop.table(t2, 2) # 100% по колонці

# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)

df3 <- df[,c("npersons", "field", "status")]
df3

# Mosaic
#table_for_mosaic <- table(df$npersons, df$field, df$status)
table_for_mosaic <- table(df$field, df$npersons, df$status)
mosaicplot(table_for_mosaic, main = "masaic", col = c(2, 3, 4))

df <- HairEyeColor 

dimnames(HairEyeColor)

# HairEyeColor - таблица с данными, встроенными в R. Посмотрите на неё в R. 
# Команда dimnames(HairEyeColor) позволит нам посмотреть, какие измерения есть в этой таблице и как они называются. 
# Например, чтобы обратиться к части таблицы, в которой хранятся данные только о мужчинах, нам нужно выполнить следующую команду: 
HairEyeColor[ , ,'Male']

# Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) от общего числа голубоглазых мужчин.
#Обратите внимание, что нужны не проценты, а просто доля, то есть десятичная дробь  (например, не 10%, а 0.1).
t4 <- prop.table(HairEyeColor[,'Blue','Male'])
red_men <- t4['Red']

red_men <- prop.table(HairEyeColor[ , ,'Male'],2)['Red','Blue']

# Напишите число зеленоглазых женщин в наборе данных HairEyeColor.
t5 <- HairEyeColor[ , 'Green','Female']
sum(t5)

# plots
barplot(t1)

barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из 
# таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.
library("ggplot2")
mydata <- as.data.frame(HairEyeColor[,,'Female'])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq, col = Eye, fill = Eye)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

# Binomial Test
binom.test(x = 5, n = 20, p = 0.5) # Значима різниця є (p=0.041)

binom.test(t1) # значимої різниці між підтвердженням і відмовою заяви на дослідженян немає (p=0.052)

# Chi-Square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs

t2
chisq.test(t2)

# Fisher's Exact Test

fisher.test(t2)

# На основе таблицы HairEyeColor создайте ещё одну таблицу, в которой хранится информация о распределении цвета глаз у женщин-шатенок (Hair = 'Brown'). 
#Проведите тест равномерности распределения цвета глаз у шатенок и выведите значение хи-квадрата для этого теста.
t6 <- HairEyeColor['Brown', ,'Female']
chisq.test(t6)

# Воспользуемся данными diamonds из библиотеки ggplot2. 
# При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи качества огранки бриллианта (сut) и его цвета (color). 
# В переменную main_stat сохраните значение статистики критерия Хи - квадрат. Обратите внимание, main_stat должен быть вектором из одного элемента, а не списком (листом).
library("ggplot2")
df6 <- diamonds
t7 <- table(diamonds$cut, diamonds$color)
chi <- chisq.test(t7)
main_stat <- chi$statistic

# Опять воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов. 
# Для этого сначала нужно перевести эти количественные переменные в формат пригодный для Хи - квадрат. Создайте две новые переменные в данных diamonds:
# factor_price - где будет 1, если значение цены больше либо равно чем среднее, и 0, если значение цены ниже среднего цены по выборке.
# factor_carat - где будет 1, если число карат больше либо равно чем среднее,  и 0, если ниже среднего числа карат по выборке.
# Важный момент - на больших данных цикл for() работает довольно медленно, постарайтесь решить эту задачу без его использования!
  # Используя эти шкалы при помощи Хи - квадрат проверьте исходную гипотезу. Сохраните в переменную main_stat значение критерия  Хи - квадрат.
df6$factor_price <- ifelse(df6$price >= mean(df6$price), 1, 0)
df6$factor_carat <- ifelse(df6$carat >= mean(df6$carat), 1, 0)

df6$factor_price <- as.factor(df6$factor_price)
df6$factor_carat <- as.factor(df6$factor_carat)

t8 <- table(df6$factor_price, df6$factor_carat)
chi_result <- chisq.test(t8)
main_stat <- chi_result$statistic

# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) и типа двигателя (vs) в данных mtcars. 
# Результат выполнения критерия сохраните в переменную.Получившийся p - уровень значимости сохраните в переменную fisher_test.
df <- mtcars
t9 <- table(df$am ,df$vs)
fisher <- fisher.test(t9)
fisher_test <- fisher$p.value
