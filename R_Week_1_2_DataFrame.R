?read.table
?read.csv

df <- read.csv('evals.csv')
df

head(df) # ������ ����� 6 �����
head(df, 3) # ������ ����� 3 �����
tail(df) # ������ ������� 6 �����

View(df) # �� 1000 �����

str(df) # ����� ��� ����� (�������) � ������� �����

names(df) # ������ �� ������
a <- names(df)

a == 'score'
b = a[a == 'score' | a == 'gender' | a == 'rank']
b

summary(df) # ������ �������� ���������� �� ��������: ������, ��������, �������, ������, 1 � 3 �������

# Variables
b <- df$score

mean(df$score)

summary(df$score)

df$score * 2 # �������� score ��������� �� 2

df$ten_point_scale <- df$score * 2 # ������� ���� ����� ten_point_scale � ��� �� � ���������

summary(df$ten_point_scale)

df$new_variable <- 0

View(df$new_variable) # View �������� � ������ �����

df$new_numbes <- 1:463
df$new_numbes <- 1:nrow(df)

summary(df$new_numbes)

nrow(df)
ncol(df)
     
# Subsetting
df$score[2] # ������ ������ ������� ������� score
df$score[1:10] # ������ ����� 10 �������� ������� score

df[1,1] # ������ ������� 1(������) 1(��������)
df[c(2,193,225),1] # ������ 2, 193, 225 ������� � ������� �������
df[101:200,1] # ������ � 101 �� 200 �������� � ������� �������

df[5,] # ��� 5 ������ (�� �������)
df[,1] # ���� ������ ��������
df[,1] == df$score # ���� � ��� (���� ������ ��������)

df[,2:5] # ������� � 2 �� 5
head(df[,2:5]) # ����� 6 ����� �������� � 2 �� 5

# Subsetting with condition
df$gender # �������� gender
df$gender == 'female' # �������� gender, �������� �� female
df[df$gender == 'female',1] # �������� 1 (score), �� gender=female
df[df$gender == 'female',1:3] # ������� 1 �� 3, �� gender=female
head(df[df$gender == 'female',1:3]) # ����� 6 ����� ��� �������� 1 �� 3, �� gender=female

subset(df, gender == 'female') # ����� ����
head(subset(df, gender == 'female')) # ����� ����

head(subset(df, score > 3.5)) # �� ���� ��� score>3.5

# rbind, cbind - ������� �� ������� � ��������
df2 <- subset(df, gender == 'female')
df3 <- subset(df, gender == 'male')
df4 <- rbind(df2, df3)

df5 <- df[,1:10]
df6 <- df[,11:24]
df7 <- cbind(df6, df5)

library(help = "datasets")

# Exercises

# � ���� ����� ���������� �� ����������� ������� mtcars. 
# � ���������� mtcars �������� ����� ������� (����������) ��� ��������� even_gear, � ������� ����� �������, ���� �������� ���������� (gear) ������, � ���� ���� ���������� ��������.
data(mtcars)
my_data <- mtcars
help(mtcars)
my_data$even_gear <- (my_data$gear + 1) %% 2

# ��������� ���� ������ � ������� mtcars. ������ ���� ������ ������� ���������� - ������ mpg_4 � ��������� � ��� �������� ������� ������� (mpg) ��� ����� � �������� ���������� (cyl).
# mpg_4 <- my_data[my_data$cyl == 4, 2]
# mpg_4 <- subset(my_data, cyl == 4, c("mpg"))

mpg_4 <- my_data[my_data$cyl == 4,]$mpg
# or
mpg_4 <- my_data$mpg[my_data$cyl == 4]

# � ������ �������� �������� ������ ��������� ������� �� �������� ������. 
# ���� ������ ������� ����� dataframe ��� ��������� mini_mtcars, � ������� ����� ��������� ������ ������, �������, �������, ����������� � ��������� ������� ���������� mtcars.
mini_mtcars <- my_data[c(3,7,10,12,nrow(my_data)),]

new_data <- subset(mtcars, cyl != 3 & qsec > mean(qsec)) # ����� ���������, �� ������� != 3, � ����� > �� ��������

mtcars[, -c(3, 4)] # ������� ��� ������� � ��� ������� ����� 3 � 4. 
subset(mtcars, hp > 100 & am == 1)

mtcars$new_var <- NULL # ������� ����� �� ����������