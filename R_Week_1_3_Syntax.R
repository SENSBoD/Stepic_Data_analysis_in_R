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
ifelse(a > 0, 'positive', 'not positive') # pos ���� true
# ifelse ������ � ��������� ���� �������

a <- c(1, -1)
ifelse(a > 0, 'positive', 'not positive') # pos ���� true

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

# �������� ����� �������� ����������  new_var � ������ mtcars, ������� �������� ������� � ��������, ���� � ������ �� ������ ������ ������������ (���������� "carb") ��� ������ ����� ��������� (���������� "cyl"). � ��������, � ������� ������� �� �����������, ������ ������ ����.
data(mtcars)
mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

# � ��� ������������ ���������� my_vector �������� ������ �� 50 �����.
# ���� ������� �������� ������� my_vector ������ 20, � ���������� result ��������� "My mean is great",  ���� ������� �������� my_vector ������ ��� ����� 20 �� � ���������� result ���������  ������ "My mean is not so great".
my_vector <- 1:50
if (mean(my_vector)>20){
  result <- "My mean is great"
}else{
  result <- "My mean is not so great"
}

# � ���� ������ �� ��� ����������� ������ ��������� ���������� � ����� ������ � R ��������������! ���������� � R ������ AirPassengers - ��� ����� ��� ��� ������ ������ ���� Time-Series. 
# ������� ��������� ���� ������, ������ ��� ������ ������� ������! �������� �������� �������:
?AirPassengers # ������� � ������
str(AirPassengers) # ��������� ������
as.vector(AirPassengers)

# � ���������� � R ������ AirPassengers �������� 144 �������� (���������� ���������� � �����) � 1949 �� 1960 ���. ������ Time-Series ����� ������ �� ������ �� ����� ���������, �������� �� ����� ���������� � ������ �� 144 ��������� ��������� ��� �������� ��� ���������� AirPassengers[1] ��� AirPassengers[56].
# ����� ������ ��������� �������� ������ � ������ ��� ������ ������� as.vector(AirPassengers) � ���������� � ���� ������ ��� � ��������.
# � ��� ���� ������ ������� ���������� good_months � ��������� � ��� ����� ���������� ������ � ��� �������, � ������� ��� ����� ������, ��� ���������� � ���������� ������.  
# ������ ������! � R �������� : ��� �������� ������������������ ����� ��������� ��� ��������������� ����������. ����� �������, ���� � ��� ���� ���������� i, ������ 10, � �� ������ ������� ������ �� 1 �� i - 1, �������������� ��������, ����� ������� ������������������ ��������.
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

# ��� ���������� � R ������ AirPassengers ����������� ���������� ������� � ���������� ����������� ������ 10. 
# ����������� ������������ ��������� (������ ��������� � ������ ������ ���� ������� ��� ��������� 1:10, �� ������ �������� - ������� ��� ��������� 2:11 � �.�., � ���������  - ������� ��� ��������� 135 :144)
# ��� ���������� �������� ������� ��������� � ���������� moving_average.
moving_average <- numeric(135)
moving_average <- c()
index <- 1
for(i in 1:(length(AirPassengers)-9)){
  moving_average[index] <- mean(AirPassengers[i:(i+9)])
  index <- index + 1  
}

mean(AirPassengers[1:10])

# ����� ������ � ��� ����� ��� ������ ��������� ������������ ����!
n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n