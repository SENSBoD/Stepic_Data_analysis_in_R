?mtcars

df <- mtcars

str(df) # ��������� �����, ������ ���

df$new_var <- NULL # �������� new_var

df$vs <- factor(df$vs, labels = c("V", "S")) # vs ��� ������������ ������, �� 0 = V, � 1 = S
df$am <- factor(df$am  , labels = c("Auto", "Manual")) # 0 = Auto, 1 = Manual

# Descriptive statistics
median(df$mpg) # ������
mean(df$disp) # �������
sd(df$hp) # ���������� ���������
range(df$cyl) # ������

mean_disp <- mean(df$disp)

mean(df$mpg[df$cyl == 6]) # ������� ��������, ����� ��� ����� � 6 ���������

mean(df$mpg[df$cyl == 6 & df$vs == "V"]) # ������� ��� 6 ������� � V ������ 

sd(df$hp[df$cyl != 3 & df$am == "Auto"]) # ������������������� ��������� ��� �� 3 ������� � Auto ������� �������

# ����� �������� � ������ mtcars. ����������� ������� �������� ������� ������� (qsec) ��� �����������, ����� ��������� (cyl) � ������� �� ��������� 3 � ���������� ���������� ���� �� ������ ������� (mpg) ������ 20.
# ������������ ��������� (������� ��������) ��������� � ���������� result.
result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])

# Aggregation
?aggregate # ��������� ������ ���������� ��� �����

mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean) # ������� ��� hp ���������� �� �������� vs
mean_hp_vs # ���������

colnames(mean_hp_vs)  <- c("VS", "Mean HP") # ����� ����� ������� � ��������� mean_hp_vs

aggregate(hp ~ vs, df, mean) # ���������� ������ ������ 

aggregate(hp ~ vs + am, df, mean) # ������� ��� hp ���������� �� �������� vs � am
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median) #������ ��� ��� ������� ��� 8 � 9, ���������� �� am

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd) # sd ��� 1 � 3 ������� ���������� �� am � vs
aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
cbind(df$mpg, df$disp)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

# ��� ������ ������� aggregate ����������� ����������� ���������� ���������� hp (��������� ����) � ���������� disp (����������� ���������)  � ����� � �������������� � ������ �������� �������.
# ���������� ���������� (���������� ���������� ������� aggregate) ��������� � ���������� descriptions_stat.
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd)

# Library "psych"
library(psych)

?describe

describe(x = df)
describe(x = df[,-c(8, 9)])
descr  <- describe(x = df[,-c(8,9)])

?describeBy()
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs) # ���������� �� ������ (� ������ ������� �� vs)
descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T)
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1) # ���� ���� ���� ����

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T) # ��� fast �������������� ����� ����� ����

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)

# NA values
is.na(df) # �� � ��������� ��������
sum(is.na(df)) # ���� ����������
sum(is.na(df$mpg))

df$mpg[1:10] <- NA
sum(is.na(df$mpg))
sum(is.na(df))

mean(df$mpg) # ���� NA ���� � ���������

mean(df$mpg, na.rm = T) # NA remove = True

aggregate(mpg ~am, df, sd)

# describe(na.rm = )

# ������������� ����������� ������� airquality. � ����� ���������� ��������� subset �������� ������, ������� ���������� ������ ��� ������� 7, 8 � 9.
# ��� ������ ������� aggregate ����������� ���������� ������������� ���������� �� ���������� Ozone � 7, 8 � 9 ������. ��� ����������� ���������� ���������� ����������� ������� length(). 
# ��������� ���������� ������� aggregate ��������� � ���������� result.
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
my_vector[sample(1:30, 10)] <- NA # �� ������ ��������� ������� �������� NA
# mean(my_vector, na.rm = T)

# � ���������� my_vector �������� ������ � ������������ ����������. ��� ����� ������� ����� ������ fixed_vector, � ������� ��� ����������� �������� ������� my_vector ����� �������� �� ������� �������� �� ��������� �����������.
# ��� ���� �������� ������ �������� ��� ���������!
# ���������, ���������� my_vector ��� �������, ����� ��������� �������� � ���. ����� ���, ��� ������� �������, �� ������ ��������������� �� ��������� ��������. ���� ��������� ���, ������� ����� ������� ��������� ������ (������� �� ����������� �������������) � ������������ ����������.
my_vector2 <- my_vector
my_vector2[is.na(my_vector2)] <- mean(my_vector2, na.rm = TRUE)
fixed_vector <- my_vector2

# ������� ������� �� ������� replace. ������� ������� ����� �������� �������:
?replace
fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))