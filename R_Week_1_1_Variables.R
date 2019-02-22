10 + 30
mean(10 : 30)

?plot

my_var1 <- 1
my_var2 <- 2
my_var3 <- 4
my_var1 <- 42
my_var2 <- 35.25

my_var1 + 100
my_var1 - my_var2
my_var1 - my_var1 - 20
my_var3 <- my_var1^2 + my_var2^2
my_var3

my_var3 > 200
my_var1 == my_var2
my_var1 != my_var2
my_new_var <- my_var1 == my_var2
my_new_var

# В уже созданных переменных number_1, number_2 и number_3, сохранены целые числа. Проверьте, действительно ли сумма первых двух чисел строго больше, чем третье число. 
# Результат сравнения (TRUE или FALSE) сохраните в новую переменную с именем result.
number_1 = 3
number_2 = 4
number_3 = 6
result <- number_1 + number_2 > number_3
result

1 : 67
0 : 12
my_Vector1 <- 1:67
my_vector2 <- c(-32, 45, 67, 12.78, 129, 0, -65)
my_vector2
my_Vector1[1]
my_vector2[3]
my_vector2[c(1,2,3)]
my_vector2[1:4]
my_vector2[c(1,5,6,10)]

# При помощи функции с() мы можем объединять не только несколько чисел, но сразу несколько векторов. 
# Создайте переменную the_best_vector, в которой хранятся числа от 1 до 5000 и затем числа от 7000 до 10000.
vect1 <- 1:5000
vect2 <- 7000:10000
the_best_vector <- c(vect1, vect2)
the_best_vector <- c(1:5000, 7000:10000)
the_best_vector

# В уже созданной переменной my_numbers сохранен вектор из 20 целых чисел. 
# Ваша задача создать новую переменную my_numbers_2, в которой будет сохранен 2, 5, 7, 9, 12, 16 и 20 элемент вектора my_numbers.
my_numbers <- 1:20
my_numbers_2 <- my_numbers[c(2,5,7,9,12,16,20)]
my_numbers_2

my_Vector1 + 10
my_Vector1

my_vector2 == 0

my_Vector1 > 30
x <- 23
my_Vector1 > x

my_vector2 > 0
my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]
my_vector2[my_vector2 == 0]

my_vector2[my_vector2 > 5 & my_vector2 < 30]

my_Vector1[my_Vector1 > 20 & my_Vector1 < 30]
my_numbers2 <- my_Vector1[my_Vector1 > 20 & my_Vector1 < 30]
my_numbers2

positive_numbers <- my_vector2[my_vector2 > 0]
positive_numbers

v1 <- c(165, 178, 180, 181, 178, 167, 187)
mean_v1 <- mean(v1)
v1[v1 > mean_v1]
greater_than_mean  <- v1[v1 > mean_v1]

# В уже созданной переменной my_vector хранится вектор из 20 целых чисел.
# Найдите сумму всех элементов вектора , которые больше 10. Сохраните сумму в переменную my_sum.
my_vector <- 1:20
my_vector <- c(8, 13, 9, 18, 7, 2, 15, 2, 8, 18, 6, 8, 9, 6, 12, 11, 3, 1, 2, 14)
my_sum <- sum(my_vector[my_vector > 10])

#Step 13: Lists and dataframes
age  <- c(16, 18, 22, 27)
is_married  <- c(F, F, T, T)
name  <- c("Olga", "Maria", "Nastya", "Polina")

data <- list(age, is_married, name)
data[[1]]
data[[2]]
data[[1]][3]
data[[2]][1]

df  <- data.frame(Name = name, Age = age, 
                  Status = is_married)
typeof(df)

my_vector <- c(21, 18, 21, 19, 25, 20, 17, 17, 18, 22, 17, 18, 18, 19, 19, 27, 21, 20, 24, 17, 15, 24, 24, 29, 19, 14, 21, 17, 19, 18, 18, 20, 21, 21, 19, 19, 17, 21, 13, 17, 13, 23, 15, 23, 24, 16, 17, 25, 24, 22)
mean(my_vector)
sd(my_vector)
min <- mean(my_vector) - sd(my_vector)
max <- mean(my_vector) + sd(my_vector)
my_vector_2 <- my_vector[my_vector > min & my_vector < max]

# В векторе  my_vector отберите только те наблюдения, которые отклоняются от среднего меньше чем на одно стандартное отклонение. Сохраните эти наблюдения в новую переменную my_vector_2.  
# При этом исходный вектор my_vector оставьте без изменений.
my_vector_2 <- my_vector[my_vector > mean(my_vector) - sd(my_vector) & my_vector < mean(my_vector) + sd(my_vector)]