?read.table
?read.csv

df <- read.csv('evals.csv')
df

head(df) # показує перші 6 рядків
head(df, 3) # показує перші 3 рядків
tail(df) # показує останні 6 рядків

View(df) # до 1000 рядків

str(df) # показє тип даних (колонок) і частину даних

names(df) # показує всі стовці
a <- names(df)

a == 'score'
b = a[a == 'score' | a == 'gender' | a == 'rank']
b

summary(df) # показує загальну інформацію по стовбцям: мінімум, максимум, середнє, медіана, 1 і 3 квартилі

# Variables
b <- df$score

mean(df$score)

summary(df$score)

df$score * 2 # значення score помножені на 2

df$ten_point_scale <- df$score * 2 # створює нову змінну ten_point_scale і дадє її в датафрейм

summary(df$ten_point_scale)

df$new_variable <- 0

View(df$new_variable) # View пишеться з великої букви

df$new_numbes <- 1:463
df$new_numbes <- 1:nrow(df)

summary(df$new_numbes)

nrow(df)
ncol(df)
     
# Subsetting
df$score[2] # виведе другий елемент стовбця score
df$score[1:10] # виведе перші 10 елементів стовбця score

df[1,1] # виведе елемент 1(строка) 1(стовбець)
df[c(2,193,225),1] # виведе 2, 193, 225 елемент з першого стовбця
df[101:200,1] # виведе з 101 по 200 елементи з першого стовбця

df[5,] # вся 5 строка (всі стовбці)
df[,1] # весь перший стовбець
df[,1] == df$score # одне і теж (весь перший стовбець)

df[,2:5] # стовбці з 2 по 5
head(df[,2:5]) # перші 6 рядків стовбців з 2 по 5

# Subsetting with condition
df$gender # стовбець gender
df$gender == 'female' # стовбцеь gender, перевірка на female
df[df$gender == 'female',1] # стовбець 1 (score), де gender=female
df[df$gender == 'female',1:3] # стовбці 1 до 3, де gender=female
head(df[df$gender == 'female',1:3]) # перші 6 рядків для стовбців 1 до 3, де gender=female

subset(df, gender == 'female') # тільки жінки
head(subset(df, gender == 'female')) # тільки жінки

head(subset(df, score > 3.5)) # всі дані для score>3.5

# rbind, cbind - зєднання по строкам і стовбцям
df2 <- subset(df, gender == 'female')
df3 <- subset(df, gender == 'male')
df4 <- rbind(df2, df3)

df5 <- df[,1:10]
df6 <- df[,11:24]
df7 <- cbind(df6, df5)

library(help = "datasets")

# Exercises

# В этой задче поработаем со встроенными данными mtcars. 
# В датафрэйме mtcars создайте новую колонку (переменную) под названием even_gear, в которой будут единицы, если значение переменной (gear) четное, и нули если количество нечетное.
data(mtcars)
my_data <- mtcars
help(mtcars)
my_data$even_gear <- (my_data$gear + 1) %% 2

# Продолжим нашу работу с данными mtcars. Теперь ваша задача создать переменную - вектор mpg_4 и сохранить в нее значения расхода топлива (mpg) для машин с четырьмя цилиндрами (cyl).
# mpg_4 <- my_data[my_data$cyl == 4, 2]
# mpg_4 <- subset(my_data, cyl == 4, c("mpg"))

mpg_4 <- my_data[my_data$cyl == 4,]$mpg
# or
mpg_4 <- my_data$mpg[my_data$cyl == 4]

# А теперь научимся отбирать только некоторые строчки из исходных данных. 
# Ваша задача создать новый dataframe под названием mini_mtcars, в котором будут сохранены только третья, седьмая, десятая, двенадцатая и последняя строчка датафрейма mtcars.
mini_mtcars <- my_data[c(3,7,10,12,nrow(my_data)),]

new_data <- subset(mtcars, cyl != 3 & qsec > mean(qsec)) # новий датафрейм, де циліднри != 3, а розгін > за середній

mtcars[, -c(3, 4)] # отберем все строчки и все колонки кроме 3 и 4. 
subset(mtcars, hp > 100 & am == 1)

mtcars$new_var <- NULL # видаляє змінну із датафрейма