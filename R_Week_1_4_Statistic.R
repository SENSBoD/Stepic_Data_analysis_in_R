?mtcars

df <- mtcars

str(df) # структура даних, показує тип

df$new_var <- NULL # видалить new_var

df$vs <- factor(df$vs, labels = c("V", "S")) # vs стає номінативними даними, де 0 = V, а 1 = S
df$am <- factor(df$am  , labels = c("Auto", "Manual")) # 0 = Auto, 1 = Manual

# Descriptive statistics
median(df$mpg) # медіана
mean(df$disp) # середнє
sd(df$hp) # стандартне відхилення
range(df$cyl) # розмах

mean_disp <- mean(df$disp)

mean(df$mpg[df$cyl == 6]) # середнє значення, тільки для машин з 6 циліндрами

mean(df$mpg[df$cyl == 6 & df$vs == "V"]) # середнє для 6 циліндрів і V двигун 

sd(df$hp[df$cyl != 3 & df$am == "Auto"]) # середньоквадратичне відхилення для НЕ 3 циліндрів і Auto коробка передач

# Вновь вернемся к данным mtcars. Рассчитайте среднее значение времени разгона (qsec) для автомобилей, число цилиндров (cyl) у которых не равняется 3 и показатель количества миль на галлон топлива (mpg) больше 20.
# Получившийся результат (среднее значение) сохраните в переменную result.
result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])

# Aggregation
?aggregate # розраховує описові статистики для даних

mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean) # середнє для hp розподілено по значенню vs
mean_hp_vs # датафрейм

colnames(mean_hp_vs)  <- c("VS", "Mean HP") # змінює назви колонок в датафреймі mean_hp_vs

aggregate(hp ~ vs, df, mean) # скорочений варіант запису 

aggregate(hp ~ vs + am, df, mean) # середнє для hp розподілено по значенню vs і am
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median) #медіана для всіх колонок крім 8 і 9, розподілено по am

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd) # sd для 1 і 3 колонки розподілено по am і vs
aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
cbind(df$mpg, df$disp)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

# При помощи функции aggregate рассчитайте стандартное отклонение переменной hp (лошадиные силы) и переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач.
# Полученные результаты (результаты выполнения функции aggregate) сохраните в переменную descriptions_stat.
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd)

# Library "psych"
library(psych)

?describe

describe(x = df)
describe(x = df[,-c(8, 9)])
descr  <- describe(x = df[,-c(8,9)])

?describeBy()
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs) # статистики по групам (в даному випадку по vs)
descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T)
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1) # один знак після коми

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T) # при fast розраховуються тільки базові речі

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)

# NA values
is.na(df) # чи є пропущені значення
sum(is.na(df)) # сума пропущених
sum(is.na(df$mpg))

df$mpg[1:10] <- NA
sum(is.na(df$mpg))
sum(is.na(df))

mean(df$mpg) # буде NA якщо є пропущені

mean(df$mpg, na.rm = T) # NA remove = True

aggregate(mpg ~am, df, sd)

# describe(na.rm = )

# Воспользуемся встроенными данными airquality. В новую переменную сохраните subset исходных данных, оставив наблюдения только для месяцев 7, 8 и 9.
# При помощи функции aggregate рассчитайте количество непропущенных наблюдений по переменной Ozone в 7, 8 и 9 месяце. Для определения количества наблюдений используйте функцию length(). 
# Результат выполнения функции aggregate сохраните в переменную result.
?airquality
df <- airquality
new_airquality <- subset(df, Month == 7 | Month == 8 | Month == 9)
new_airquality <- subset(df,  Month%in%c(7,8,9))
result <- aggregate(x = new_airquality$Ozone, by = list(new_airquality$Month), FUN = length, simplify = TRUE)
result <- aggregate(Ozone ~ Month, new_airquality, length)

res <- describeBy(x = df[,c(1,2,3,4)], group = df$Month, mat = T, digits = 1)

?iris
df <- iris
describe(x=df)

df1 <- subset(df, Species == "virginica")
describe(x=df1)

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA
# mean(my_vector, na.rm = T)

# В переменной my_vector сохранен вектор с пропущенными значениями. Вам нужно создать новый вектор fixed_vector, в котором все пропущенные значения вектора my_vector будут заменены на среднее значение по имеющимся наблюдениям.
# При этом исходный вектор оставьте без изменений!
# Напоминаю, переменная my_vector уже создана, сразу начинайте работать с ней. Перед тем, как сдавать решение, вы можете потренироваться на различных примерах. Ниже небольшой код, который может создать случайный вектор (выборка из нормального распределения) с пропущенными значениями.
my_vector2 <- my_vector
my_vector2[is.na(my_vector2)] <- mean(my_vector2, na.rm = TRUE)
fixed_vector <- my_vector2

# Изучите справку по функции replace. Вызвать справку можно исполнив команду:
?replace
fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))