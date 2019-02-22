df <- read.csv('evals.csv')

# if

a <- -10

if (a>0){
  print('positive')
}else{
  print('negative')
  print(a+1)
}

if (a > 0){
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')

# ifelse
ifelse(a > 0, 'positive', 'not positive') # pos якщо true
# ifelse працює з векторами різної довжини

a <- c(1, -1)
ifelse(a > 0, 'positive', 'not positive') # pos якщо true

# for
for (i in 1:100){
  print(i)
#  cat(paste(i," "))
}

for (i in 1:nrow(df)){
  print(df$score[i])
}

# for + if
for (i in 1:nrow(df)){
  if (df$gender[i] == 'male'){
    print(df$score[i]) 
  }
}

# for + if  VS  ifelse
df$quality <- rep(NA, nrow(df))

for (i in 1:nrow(df)){
  if (df$score[i] > 4){
    df$quality[i] <- 'good'
  } else df$quality[i] <- 'bad'
}

df$quality <- ifelse(df$score > 4, 'good', 'bad')

# while
i <- 1

while(i < 51){
  print(df$score[i])
  i <- i+1
}

# Exercises

# Создайте новую числовую переменную  new_var в данных mtcars, которая содержит единицы в строчках, если в машине не меньше четырёх карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl"). В строчках, в которых условие не выполняется, должны стоять нули.
data(mtcars)
mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

# В уже существующей переменной my_vector сохранен вектор из 50 чисел.
# Если среднее значение вектора my_vector больше 20, в переменную result сохраните "My mean is great",  если среднее значение my_vector меньше или равно 20 то в переменную result сохраните  строку "My mean is not so great".
my_vector <- 1:50
if (mean(my_vector)>20){
  result <- "My mean is great"
}else{
  result <- "My mean is not so great"
}

# В этой задаче от вас потребуется узнать некоторую информацию о типах данных в R самостоятельно! Встроенные в R данные AirPassengers - это новый для нас формат данных типа Time-Series. 
# Изучите структуру этих данных, прежде чем начать решение задачи! Например напишите команды:
?AirPassengers # справка о данных
str(AirPassengers) # структура данных
as.vector(AirPassengers)

# В встроенных в R данных AirPassengers хранится 144 значения (количество пассажиров в месяц) с 1949 по 1960 год. Данные Time-Series очень похожи на вектор по своей структуре, например мы можем обратиться к любому из 144 элементов используя уже знакомую нам индексацию AirPassengers[1] или AirPassengers[56].
# Можно вообще перевести исходные данные в вектор при помощи команды as.vector(AirPassengers) и продолжить с ними работу как с вектором.
# И так ваша задача создать переменную good_months и сохранить в нее число пассажиров только в тех месяцах, в которых это число больше, чем показатель в предыдущем месяце.  
# Важный момент! В R оператор : для создания последовательности имеет приоритет над арифметическими действиями. Таким образом, если у вас есть переменная i, равная 10, и вы хотите создать вектор от 1 до i - 1, воспользуйтесь скобками, чтобы указать последовательность действий.
good_months <- c()
good_months <- 1:144
for(i in 2:144){
  if(AirPassengers[i]>AirPassengers[i-1]){
    good_months[i] <- AirPassengers[i]
  }
}

good_months <- c()    
index <- 1    
for (i in 2:length(AirPassengers)) {    
  if (AirPassengers[i]>AirPassengers[i-1]){    
    good_months[index] <- AirPassengers[i]    
    index <- index + 1    
  }    
}

# Для встроенных в R данных AirPassengers рассчитайте скользящее среднее с интервалом сглаживания равным 10. 
# Напечатайте получившийся результат (первым значением в выводе должно быть среднее для элементов 1:10, во втором значении - среднее для элементов 2:11 и т.д., в последнем  - среднее для элементов 135 :144)
# Все полученные значения средних сохраните в переменную moving_average.
moving_average <- numeric(135)
moving_average <- c()
index <- 1
for(i in 1:(length(AirPassengers)-9)){
  moving_average[index] <- mean(AirPassengers[i:(i+9)])
  index <- index + 1  
}

mean(AirPassengers[1:10])

# Можно решить и без цикла при помощи разностей кумулятивных сумм!
n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n