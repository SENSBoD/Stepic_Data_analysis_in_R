df <- iris

str(df)

df1 <- subset(df, Species != 'setosa')
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

library(ggplot2)
ggplot(df1, aes(x = Sepal.Length))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()


shapiro.test(df1$Sepal.Length) # нормальність розподілу

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

# The same result in one line
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)


bartlett.test(Sepal.Length~ Species, df1) # гомогенність дисперсії


t.test(Sepal.Length ~ Species, df1)
1.866e-07 == 0.0000001866
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

t.test(Sepal.Length  ~ Species, df1, var.equal = T)

t.test(df1$Sepal.Length, mu = 8) # поірвняти середнє з указаним середнім  (мю=8)

mean(df1$Sepal.Length)
t.test(df1$Sepal.Length, mu = 6.262)

# Порівняння залежних вибірок
t.test(df1$Petal.Length, df1$Petal.Width, paired = T) # paired=True для залежних вибірок

# Воспользуемся еще одним встроенным набором данных в R  - ToothGrowth. 
# Данные позволяют исследовать рост зубов у морских свинок в зависимости от дозировки витамина C и типа потребляемых продуктов.
# Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, 
# со средним значением длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 
# Значение t - критерия сохраните в переменную t_stat.
df <- ToothGrowth
df$group <- ifelse((df$supp == 'OJ' & df$dose == 0.5), 'first', 'second')
df1 <- subset(df, dose != 1.0)

s1 <- subset(df, df$supp == 'OJ' & df$dose == 0.5)
s2 <- subset(df, df$supp == 'VC' & df$dose == 2.0)
df2 <- rbind(s1, s2)
test_result <- t.test(len ~ supp, df2)
t_stat <- test_result$statistic

correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)    
t_stat <- t.test(len ~ supp, correct_data)$statistic

# По всем испытуемым сравните показатель давления до начала лечения (Pressure_before) с показателем давления после лечения (Pressure_after)
# при помощи t - критерия для зависимых выборок. 
# В поле для ответа укажите значение t - критерия.
df <- read.csv('https://stepic.org/media/attachments/lesson/11504/lekarstva.csv')
df
t.test(df$Pressure_before, df$Pressure_after, paired = T)


# Visualize T-test
library(ggplot2)
library(Hmisc)
df <- iris
df1 <- subset(df, Species != 'setosa')

mean_cl_normal(df$Sepal.Length)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = 'point', size = 3)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', 
               size = 1)

# Не параметричнйи аналог t-сиюдента
?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)
paired_wtest$p.value

# Exercises

# В этом задании нужно проверить гипотезу о равенстве средних двух выборок, загрузив набор данных 
# (нажмите начать решать задание) и выполнив все необходимые операции на вашем компьютере. 
# В скачанных данных вы найдете две переменные: количественную переменную, и номинативную переменную с двумя градациями 
# (которая разделяет наблюдения на две группы).
# Сначала с помощью теста Бартлетта проверьте гомогенность дисперсий двух выборок. 
# В случае, если дисперсии значимо не отличаются (с уровнем 0.05), примените тест Стьюдента, 
# иначе - непараметрический тест (Манна-Уитни). В поле для ответа введите получившийся p-value, с точностью четыре знака после запятой.
# Обратите внимание, что по умолчанию в t.test стоит var.equal = FALSE, 
# так как мы будем применять его только в случае гомогенности дисперсий, измените значение этого параметра на  var.equal = TRUE.
df <- read.csv('dataset_11504_15.txt')
df
t1 <- read.table("dataset_11504_15.txt")
t1
#t2 <- as.data.frame(t1)
bartlett.test(V1 ~ V2, t1) # гомогенність дисперсії
t.test(V1  ~ V2, t1, var.equal = T)
wilcox.test(Petal.Length ~ Species, t1)

# В данных сохранены две количественные переменные, проверьте гипотезу о равенстве средних этих переменных при помощи t- теста для независимых выборок.
# Если обнаружены значимые различия (p< 0.05), то введите через пробел три числа: среднее значение первой переменной, 
# среднее значение второй переменной, p - уровень значимости.
t3 <- read.table("dataset_11504_16.txt")
t3
t.test(t3$V1, t3$V2, var.equal = F)
mean_cl_normal(t3$V1)
mean_cl_normal(t3$V2)
mean(t3$V1)
mean(t3$V2)
