# Categorical data

df <- read.csv("grants.csv")

str(df)

df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded")) # �� ���� �� �� ��������� ������

# 1d Table
t1 <- table(df$status) #������ pivot table
t1

dim(t1) # ���������� ������� (� ������ ������� - ����������)

# 2d Table
t2 <- table(df$status, df$field)
t2

t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2) # ������ �������� ������� � ������� (����� 100%)

prop.table(t2, 1) # 100% �� ������
prop.table(t2, 2) # 100% �� �������

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

# HairEyeColor - ������� � �������, ����������� � R. ���������� �� �� � R. 
# ������� dimnames(HairEyeColor) �������� ��� ����������, ����� ��������� ���� � ���� ������� � ��� ��� ����������. 
# ��������, ����� ���������� � ����� �������, � ������� �������� ������ ������ � ��������, ��� ����� ��������� ��������� �������: 
HairEyeColor[ , ,'Male']

# ���� ������ � ���������� red_men ��������� ���� ����������� (Red) �� ������ ����� ������������ ������.
#�������� ��������, ��� ����� �� ��������, � ������ ����, �� ���� ���������� �����  (��������, �� 10%, � 0.1).
t4 <- prop.table(HairEyeColor[,'Blue','Male'])
red_men <- t4['Red']

red_men <- prop.table(HairEyeColor[ , ,'Male'],2)['Red','Blue']

# �������� ����� ������������ ������ � ������ ������ HairEyeColor.
t5 <- HairEyeColor[ , 'Green','Female']
sum(t5)

# plots
barplot(t1)

barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

# ��������� ���������� ��������� ������������� ����� ���� �� ����� ����� ������ � ������ �� 
# ������� HairEyeColor. �� ��� X ������ ���� ���� �����, ���� ��������� ������ �������� ���� ����. �� ��� Y - ���������� ����������.
library("ggplot2")
mydata <- as.data.frame(HairEyeColor[,,'Female'])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq, col = Eye, fill = Eye)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

# Binomial Test
binom.test(x = 5, n = 20, p = 0.5) # ������� ������ � (p=0.041)

binom.test(t1) # ������� ������ �� ������������� � ������� ����� �� ���������� ���� (p=0.052)

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

# �� ������ ������� HairEyeColor �������� ��� ���� �������, � ������� �������� ���������� � ������������� ����� ���� � ������-������� (Hair = 'Brown'). 
#��������� ���� ������������� ������������� ����� ���� � ������� � �������� �������� ��-�������� ��� ����� �����.
t6 <- HairEyeColor['Brown', ,'Female']
chisq.test(t6)

# ������������� ������� diamonds �� ���������� ggplot2. 
# ��� ������ �������� �� - ������� ��������� �������� � ����������� �������� ������� ���������� (�ut) � ��� ����� (color). 
# � ���������� main_stat ��������� �������� ���������� �������� �� - �������. �������� ��������, main_stat ������ ���� �������� �� ������ ��������, � �� ������� (������).
library("ggplot2")
df6 <- diamonds
t7 <- table(diamonds$cut, diamonds$color)
chi <- chisq.test(t7)
main_stat <- chi$statistic

# ����� ������������� ������� diamonds �� ���������� ggplot2. ��� ������ �������� �� - ������� ��������� �������� � ����������� ���� (price) � ������� (carat) �����������. 
# ��� ����� ������� ����� ��������� ��� �������������� ���������� � ������ ��������� ��� �� - �������. �������� ��� ����� ���������� � ������ diamonds:
# factor_price - ��� ����� 1, ���� �������� ���� ������ ���� ����� ��� �������, � 0, ���� �������� ���� ���� �������� ���� �� �������.
# factor_carat - ��� ����� 1, ���� ����� ����� ������ ���� ����� ��� �������,  � 0, ���� ���� �������� ����� ����� �� �������.
# ������ ������ - �� ������� ������ ���� for() �������� �������� ��������, ������������ ������ ��� ������ ��� ��� �������������!
  # ��������� ��� ����� ��� ������ �� - ������� ��������� �������� ��������. ��������� � ���������� main_stat �������� ��������  �� - �������.
df6$factor_price <- ifelse(df6$price >= mean(df6$price), 1, 0)
df6$factor_carat <- ifelse(df6$carat >= mean(df6$carat), 1, 0)

df6$factor_price <- as.factor(df6$factor_price)
df6$factor_carat <- as.factor(df6$factor_carat)

t8 <- table(df6$factor_price, df6$factor_carat)
chi_result <- chisq.test(t8)
main_stat <- chi_result$statistic

# ��� ������ ������� �������� ������ ��������� �������� � ����������� ���� ������� ������� (am) � ���� ��������� (vs) � ������ mtcars. 
# ��������� ���������� �������� ��������� � ����������.������������ p - ������� ���������� ��������� � ���������� fisher_test.
df <- mtcars
t9 <- table(df$am ,df$vs)
fisher <- fisher.test(t9)
fisher_test <- fisher$p.value
