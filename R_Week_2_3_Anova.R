# ANOVA

# formulae

DV ~ IV # One-way: ������� ~ ��������� �����

DV ~ IV1 + IV2 # Two-way: ������� ~ �� ��������� �����

DV ~ IV1:IV2  # Two-way interaction: ������� �� ��������� �������

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction:
# ������� ~ �� ��������� ����� + ������� �� ����

DV ~ IV1 * IV2  # The same: Main effects + interaction: ���������� ����� ����, �� ����

DV ~ IV1 + IV2 + IV3 + IV1:IV2
# 3 ���������� + ������� �� ������� �����

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2:
# ������� ~ ��� ���������� � ����� �������, ��� �����䳿 �����

DV ~ IV1 + Error(subject/IV1) # repeated measures: �������� ����������

# ���� � ���
DV ~ IV1 * IV2
DV ~ (IV1 + IV2)^2
DV ~ (IV1*IV2)^2


df <- read.csv('shops.csv')
str(df)

library(ggplot2)

# One-way ANOVA
boxplot(price ~ origin, data=df)

ggplot(df, aes(x = origin, y = price)) + 
  geom_boxplot()

fit <- aov(price ~ origin, data=df)
summary(fit)

# Two-way ANOVA
fit1 <- aov(price ~ origin + store, data=df)
summary(fit1)

model.tables(fit1,"means") # ��� "mean" ������ ������� �� ������ (��������� ����� - origin � store)

# Interaction - ����� �����䳿
pd = position_dodge(0.1)
ggplot(df, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=df)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=df) # ���������� ����� ����, �� ����
summary(fit4)

# ������������� ����������� ������� npk, ��������������� ������� ���������� ��������� ��������� �� ����������� ������ (yield). 
# ����� ������� ����� ��������, ����������� �� ������������� ���������� ����� (������ N) � ������� (������ P). 
# ��������� ������������� ������, ��� ����� ����������� ������� ������� ���������� ����� (N), 
# ������� ������� ���������� ������� (P) � �� ��������������.
df1 <- npk
fit5 <- aov(yield ~ N * P, data=df1)
summary(fit5)

# ������ ��������� ������������� ������������� ������, ��� ��������� ���������� - ��� ����������� (yield), 
# � ��� ������� - ���� ��������� (N, P, K). 
# ����� ���������� ������� ������� �� �������� ��� �������� p - ������ ���������� (� ���������� ������� �� ��������).
fit6 <- aov(yield ~ N + P + K, data=df1)
summary(fit6)

# Pairwise comparisons - ������� ��������� � ���������� �� �������� ���������
ggplot(df, aes(x = food, y = price)) + 
  geom_boxplot()

fit7 <- aov(price ~ food, data=df)
summary(fit7) # ����������� ������� �������

TukeyHSD(fit7) # ������� ��������� � ��������� ����
# ����� ������ �� ����� �� ��� � ��� � ����������� ��������

# ��������� ������������� ������������� ������ �� ���������� ������ iris. 
# ��������� ���������� - ������ ����������� (Sepal.Width), ����������� ���������� - ��� (Species). 
# ����� ��������� �������� ��������� �����. ����� ���� ������������� ������� ����������� �� ������ ����������� (p < 0.05)?
df2 <- iris

boxplot(Sepal.Width ~ Species, data=df2)

fit8 <- aov(Sepal.Width ~ Species, data=df2)
summary(fit8)

TukeyHSD(fit8)

ggplot(df2, aes(Species, Sepal.Width))+
  stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', 
               size = 1)

# Repeated measures - ���� ������������� �� � �����������,
# � ���������� �� ������ ������
df4 <- read.csv('therapy_data.csv')
str(df4)

df4$subject <- as.factor(df4$subject)

fit9 <- aov(well_being ~ therapy, data = df4)
summary(fit9)
# ���� � ��� ������ ����������� � ������ ��������� 3 ���� � �������� 3 �������������
fit9b <- aov(well_being ~ therapy + Error(subject/therapy), data = df4)
summary(fit9b)

fit10 <- aov(well_being ~ therapy*price, data = df4)
summary(fit10)

ggplot(df4, aes(x = price, y = well_being)) + 
  geom_boxplot()

ggplot(df4, aes(price, well_being))+
  stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', 
               size = 1)

fit10b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = df4)
summary(fit10b)

ggplot(df4, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)

fit10 <- aov(well_being ~ therapy*price*sex, data = df4)
summary(fit10)
fit10b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = df4)
summary(fit10b)

# � ���� ������ ��� ��� ����� ������, � ������� ������������ ���������� � ����������� ���������� ���������, 
# ������� ������� ������� ���������� � � ������ ������.
# ��������� ������������� ������������� ������ � ���������� �����������: ������� ���� �������� (pill) 
# �� ����������� (temperature) � ������ ����������� (patient). ������ p-value ��� ������� ���� �������� �� �����������?
df5 <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')
str(df5)

df5$patient <- factor(df5$patient)

fit11 <- aov(temperature ~ pill + Error(patient/pill), data = df5)
summary(fit11)

# ������ ����� ������� ����� �������� ������������� ������������� ������ � ���������� �����������: ������� �������� doctor, 
# ������� ������� pill � �� �������������� �� temperature. ������ ��� ��������������� ����������: 
# � ��� ����, ��� ���� � ��� �� ������� ��������� ������ ��������, � ��� ����, ���  ���� � ��� �� ������� ������� � ������ ������! 
# ������ F-�������� ��� �������������� �������� ������� (doctor) � ���� �������� (pill)?
fit12 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = df5)
summary(fit12)

# ��������� ������� �� ������ � ��������� ������ ������� � ���� ��� ������ ��� (�� �������� ��� ���� geom) , 
# ����� ���������� ������� �����, ������������� ������ ������� ������� supp. �� �������� ���������� ������ ��� ���������� ������� �����.
# ����������, ��������� ������ � ���������� obj.
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
