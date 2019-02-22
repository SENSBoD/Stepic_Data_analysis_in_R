# ANOVA

# formulae

DV ~ IV # One-way: залежна ~ незалежна змінна

DV ~ IV1 + IV2 # Two-way: залежна ~ дві незалежні змінні

DV ~ IV1:IV2  # Two-way interaction: взаємодія між залежними змінними

DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction:
# залежна ~ дві незалежні змінні + взаємодія між ними

DV ~ IV1 * IV2  # The same: Main effects + interaction: скорочений запис того, що вище

DV ~ IV1 + IV2 + IV3 + IV1:IV2
# 3 предиктора + взаємодія між першими двома

DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2:
# залежна ~ три предиктора і парна взаємодія, без взаємодії трьох

DV ~ IV1 + Error(subject/IV1) # repeated measures: повторне вимірювання

# Одне і теж
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

model.tables(fit1,"means") # при "mean" показує середнє по групам (незалежні змінні - origin і store)

# Interaction - аналіз взаємодії
pd = position_dodge(0.1)
ggplot(df, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=df)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=df) # скорочений запис того, що вище
summary(fit4)

# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield). 
# Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) и фосфата (фактор P). 
# Примените дисперсионный анализ, где будет проверяться влияние фактора применения азота (N), 
# влияние фактора применения фосфата (P) и их взаимодействие.
df1 <- npk
fit5 <- aov(yield ~ N * P, data=df1)
summary(fit5)

# Теперь проведите трехфакторный дисперсионный анализ, где зависимая переменная - это урожайность (yield), 
# а три фактора - типы удобрений (N, P, K). 
# После проведения данного анализа вы получите три значения p - уровня значимости (о значимости каждого из факторов).
fit6 <- aov(yield ~ N + P + K, data=df1)
summary(fit6)

# Pairwise comparisons - попарні порівняння з поправками на множинін порівняння
ggplot(df, aes(x = food, y = price)) + 
  geom_boxplot()

fit7 <- aov(price ~ food, data=df)
summary(fit7) # статистично значима рівзниця

TukeyHSD(fit7) # попарні порівняння з поправкою Тюки
# тільки різниця між ціною на сир і хліб є статистично значимою

# Проведите однофакторный дисперсионный анализ на встроенных данных iris. 
# Зависимая переменная - ширина чашелистика (Sepal.Width), независимая переменная - вид (Species). 
# Затем проведите попарные сравнения видов. Какие виды статистически значимо различаются по ширине чашелистика (p < 0.05)?
df2 <- iris

boxplot(Sepal.Width ~ Species, data=df2)

fit8 <- aov(Sepal.Width ~ Species, data=df2)
summary(fit8)

TukeyHSD(fit8)

ggplot(df2, aes(Species, Sepal.Width))+
  stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', 
               size = 1)

# Repeated measures - якщо зпостереження не є незалежними,
# а згруповані по певним змінним
df4 <- read.csv('therapy_data.csv')
str(df4)

df4$subject <- as.factor(df4$subject)

fit9 <- aov(well_being ~ therapy, data = df4)
summary(fit9)
# одна і таж терапія проводилась з кожним пацієнтом 3 раза і відповідно 3 спостереження
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

# В этой задаче вам дан набор данных, в котором представлена информация о температуре нескольких пациентов, 
# которые лечатся разными таблетками и у разных врачей.
# Проведите однофакторный дисперсионный анализ с повторными измерениями: влияние типа таблетки (pill) 
# на температуру (temperature) с учётом испытуемого (patient). Каково p-value для влияния типа таблеток на температуру?
df5 <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')
str(df5)

df5$patient <- factor(df5$patient)

fit11 <- aov(temperature ~ pill + Error(patient/pill), data = df5)
summary(fit11)

# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями: влияние факторов doctor, 
# влияние фактора pill и их взаимодействие на temperature. Учтите обе внутригрупповые переменные: 
# и тот факт, что один и тот же больной принимает разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей! 
# Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?
fit12 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = df5)
summary(fit12)

# Вспомните графики из лекций и дополните шаблон графика в поле для ответа так (не добавляя еще один geom) , 
# чтобы объединить линиями точки, принадлежащие разным уровням фактора supp. Не забудьте подключить нужный для построение графика пакет.
# Пожалуйста, сохраните график в переменную obj.
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
