my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)

my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3(1, 2, 3)
my_calc_3(1, 2)


distr1  <- rnorm(100)
hist(distr1)
distr1[1:30]  <- NA # ����� 30 �������� - NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T) # ����� NA �� ������� �� ���

my_na_rm  <- function(x){
  x[is.na(x)]  <- mean(x, na.rm = T)
  return(x)
}

distr1 <- my_na_rm(distr1)
hist(distr1)

my_na_rm(c("1", "3", NA))

my_na_rm  <- function(x){
  if (is.numeric(x)){ # ���������� �� �����
    stat_test  <- shapiro.test(x) # ���� �� ������������
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000) # ���������� �������
d2  <- runif(2000) # ��������� �������

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)

source("my_na_rm.R")
my_na_rm(d1)

# �������� �������, ������� ������� ������ ������� ����������� ���������� � �������.
# �� ���� ������� �������� �������� ������ � ������������ ����������. 
# ������� ���������� ����� ������ � �������� ������� ����������� ��������.
# ���������: ����� ��������� �������� �� ���������� NA, �������������� �������� is.na(), ������, 
# ������� ���������������, � ���������� ����� ������� ������ ������������ ������. 
# ������ x == NA �� � ���� ������������ �� ��������. �.�. ���� x ��� NA, �� ������� x == NA ����� ������ NA, � �� TRUE!
NA.position <- function(x){
  which(is.na(x))
}
my_vector <- c(1, 2, 3, NA, NA)
NA.position(my_vector)

# �������� ������� NA.counter ��� �������� ����������� �������� � �������.
# �� ���� �������  NA.counter ������ ��������� ���� �������� - �������� ������. ������� ������ ���������� ���������� ����������� ��������.
NA.counter <- function(x){
  length(which(is.na(x)))
  #return(sum(is.na(x))) # ����� ���������
}

NA.counter(my_vector)

# ���� ��������� � ��������
setwd("E:/Install/Statistics/R/grants_data/Grants data/")

#dir_1 <- dir(path = "E:/Install/Statistics/R/grants_data/Grants data/", pattern = "*.csv")
dir_1 <- dir(pattern = "*.csv")

grants_df <- data.frame() # ������� ������ ���������

for (i in dir_1){
  temp_df <- read.csv(i)
  grants_df <- rbind(temp_df, grants_df)
}

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0 # ���� ����� ����� ��������� ������
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

grants_2 <- read_data()

# �������� ������� filtered.sum, ������� �� ���� �������� ������ � ������������, 
# �������������� � �������������� ���������� � ���������� ����� ������������� ��������� �������.
filtered.sum <- function(x){
  sum <- 0
  for(i in x){
    if(!is.na(i)){
      if(i > 0){
       sum <- sum + i
      }
    }
  }
  return(sum)
}
filtered.sum(c(1, -2, 3, 3, NA))

filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}

# �������� ������� outliers.rm, ������� ������� � ������� �������. ��� ����������� �������� ������������� ����� ������� ��������, 
# � ������� �� �� ��� �����������, ��������� ������ Box plot. 
# ��������� ����� ������� �� ����������, ������� ����������� �� 1 ��� 3 �������� ������ ��� �� 1,5 *  IQR, 
# ���  IQR  - �������������� ������.
# �� ���� ������� �������� �������� ������ x. ������� ������ ���������� ���������������� ������ x � ���������� ���������.
outliers.rm <- function(x){
  iqr <- IQR(x)
  #quant <- quantile(x, probs = c(0.25, 0.75))
  quant_1 <- quantile(x, probs = c(0.25))
  quant_3 <- quantile(x, probs = c(0.75))
  #print(typeof(quant_1))
  #return(c(iqr, quant_1, quant_3))
  #return(quant_1[[1]])
  return(x[x > (quant_1[[1]] -  (1.5 * iqr)) & x < (quant_3[[1]] + (1.5 * iqr))])
}

distr <- rnorm(100)
outliers.rm(distr)

outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])
}