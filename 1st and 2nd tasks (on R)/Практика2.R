library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")
library("Hmisc")


data = rlms_read("C:\\Users\\Mishan\\Desktop\\����������������\\��������\\r24i_os26c.sav")
glimpse(data)
d = select(data, tj13.2, t_age, th5, t_educ, status, tj6.2, t_marst)

#��������� ������ � �������������� ���������� NA
d = na.omit(d)

                            # ������� �������� �������� � ����������� �� ��������
# �������� ���������
d["wed1"]=d$t_marst
d["wed1"]=0
# �������� 1, ���� ���������� �����
d$wed1[which(d$t_marst == '2')] = 1 # ������� � ������������������ �����
d$wed1[which(d$t_marst == '6')] = 1 # ���������� ����������������, �� ������ �� ���������
d$wed1 = as.numeric(d$wed1)

d["wed2"]=d$t_marst
d["wed2"]=0
d$wed2[which(d$t_marst == '4')] = 1 # �������� � � ����� �� ������� 
d$wed2[which(d$t_marst == '5')] = 1 # ������(�����)
d$wed2 = as.numeric(d$wed2)

d["wed3"]=d$t_marst
d["wed3"]=0
d$wed3[which(d$t_marst == '1')] = 1 # ������� � ����� �� �������
d$wed3 = as.numeric(d$wed3)

#���
d["sex"]=d$th5
d$sex[which(d$sex == '2')] = 0 # �������
d$sex[which(d$sex == '1')] = 1 # �������
d$sex = as.numeric(d$sex)

# ���������� �����
d["city_status"]=d$status
d["city_status"]=0
d$city_status[which(d$status == '1')] = 1 # ��������� �����
d$city_status[which(d$status == '2')] = 1 # �����
d$city_status = as.numeric(d$city_status)

# ������ �����������
d["higher_educ"]=d$t_educ
d["higher_educ"]=0
d$higher_educ[which(d$t_educ == '21')] = 1 # ���� ������ � ������ ����������� 
d$higher_educ[which(d$t_educ == '22')] = 1 # ����������� � �.�. ��� �������
# ������ 22 ��������������� ���, ��� ���� ������ � ������ �����������, �� ��� ������� �����������
d$higher_educ[which(d$t_educ == '23')] = 1 #����������� � �.�. � �������� 
d$higher_educ = as.numeric(d$higher_educ)

                                # ����������� ������ ��������� ��������

# ��������
d["salary"]=d$tj13.2
d$salary = as.numeric(d$salary)
d["salary"] = (d["salary"] - mean(d$salary)) / sqrt(var(d$salary))

# �������
d["age"]=d$t_age
d$age = as.numeric(d$age)
d["age"] = (d["age"] - mean(d$age)) / sqrt(var(d$age))

# ����������������� ������� ������ � �����
d["workweek"]=d$tj6.2
d$workweek = as.numeric(d$workweek)
d["workweek"] = (d["workweek"] - mean(d$workweek)) / sqrt(var(d$workweek))

d2 = select(d, salary, age, sex, higher_educ, city_status, workweek, wed1, wed2, wed3)
d2

                                                    #1 
# ��������� �������� ��������� �������� �� ��� ���������, ������� �� ��������
# �� ������ �����������. �� �������� ������� ����������� ������� ��������� VIF

model1 = lm(salary~age+sex+higher_educ+city_status+workweek+wed1+wed2+wed3, d2)
vif(model1)
# Vif ��� ���� ����������� < 2.4, ��� ������� � ��� ������������� ���. �����������.

summary(model1)
# �� ������ ����� �������, ��� ��� ����������, ����� wed2(1 ������) � wed3(0 ����) ������� ��������� ������(2-3 ������),
# ������� ��� ��������� ���������: multiple R-squared = 0.175,	adjusted R-squared = 0.1735.

#��������� ������� ��������� wed3:
model2 = lm(salary~age+sex+higher_educ+city_status+workweek+wed1+wed2, d2)
vif(model2)
# ��� ���������� ��� ����������
summary(model2)
# Multiple R-squared:  0.1745,	Adjusted R-squared:  0.1732,
# �-�������� ���� ����������� ����������� �� ���� �����.

                                                  #2
#������������������� � ��������� ������������ ����������: �����������
# �������� � �������

# � ��������� ����� �������� ���� ������������� ��������,
# ������� �� ��� ��������, ������� ������� �� �� �������������

describe(d2)
# salary lowest = -1.396326
d2["salary"] = d2["salary"] + 1.5

# age lowest = -1.969413 
d2["age"] = d2["age"] + 2

# workweek lowest = -3.362522
d2["workweek"] = d2["workweek"] + 3.5

# ��������, ����� ����� �������� � ����������������� ������������.
# ��� ���������� salary, age � workweek

model3 = lm(log(salary)~age+sex+higher_educ+city_status+workweek+wed1+wed2+log(age)+log(workweek), d2)
summary(model3)
vif(model3)
#  Multiple R-squared:  0.2201,	Adjusted R-squared:  0.2185
#  p-�������� wed1, wed2 �����(0 ����� � �����), � workweek - ���� ������, ��������� ���������� �� 3.
#  Vif ������� ����� ����������� ���������.
#  �� vif log(age) � age ����� 10, log(workweek) � workweek ����� 6. ��� ������ ������� � ��� �����������.

# ��������� ������ �������� � salary
model4 = lm(salary~age+sex+higher_educ+city_status+workweek+wed1+wed2+log(age)+log(workweek), d2)
summary(model4)
vif(model4)
# Vif �� ��������� => ������ �� ��������.
# Multiple R-squared:  0.1874,	Adjusted R-squared:  0.1858 
# �-�������� ���� ����������� ������(3 ������), ����� workweek(0 �����), �� �� �����, ��� ��������
# ����� �������� � �������� ������

# ������ ��������� workweek(0 �����) � ��������� log(age)(p-�������� ������, ��� � age)
model5 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek), d2)
summary(model5)
vif(model5)
# Multiple R-squared:  0.1768,	Adjusted R-squared:  0.1754.
# �-�������� ���� ����������� �� 3 ������.
# vif < 1.7, ��� ������ �������.
# � ����������� �� �������� �������� ������, ��������� �������� �
# Multiple R-squared � Adjusted R-squared � ������� ����������� ���������� � �����-�� �������.

model6 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek) + I(age*age), d2)
summary(model6)
vif(model6)
# age � I(age*age) ��� ��������, ������ ��� �� ��������.

model7 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek) + I(age^1.3), d2)
vif(model7)
summary(model7)
# ��� ����������� ������ �������� age � I(age*age) ��� ��������, ������ ��� �� ��������.
# ��������� ������ ��������.

model8 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek) + I((log(workweek))^2), d2)
vif(model8)
summary(model8)
# ��� ������ �� ��������� � workweek, � log(workweek) ������ �������� �� �����. �� � ������ ���� �������
# ��������� ����������, �� Multiple R-squared � Adjusted R-squared �� ��������, � p-��������
# ��������� ��������� �� �������� � ���������� 0 ����.

                                                    #3. 
#�������� ��������� ������ �� �����������: �� ���������� ����������,
#���������� � �����������, � �� ������������ � ������� ����������� ������������ �������� adjusted R^2.

# ������ ������ - model5(������������ log(workweek)), model2(������ �������� ������)
# �������� ���������������� model5, ������ ���������� ������� �������� ������ model2.
# ����������� ��������� ��������� ������� ����� ����, ��� ���������� � ��������� salary ���������
# ��� ������� R^2 � adj R^2, ���� �� ������ ������ � p-���������. ��������� ��� ��������� � �������� 
# �������, �.�. ������� � ��� �������� ����������� ����������. �������� ����������� ������:

spec_model = lm(log(salary)~age+sex+higher_educ+city_status+workweek+wed1+wed2, d2)
summary(spec_model)
vif(spec_model)
# Multiple R-squared:  0.1974,	Adjusted R-squared:  0.1961
# p-�������� ���� ���������� 2-3 ������(2 ������ � wed1 � wed2, ��� ���� �������)
# ����������, ��� ���� ������ ������
                                                    #4 
# �������� ����� � ���, ����� �������� �������� ���������� ��������.

# ����� �� ������ ������� ������� ��� ��������� ��������� "salary"

spec_model = lm(log(salary)~age+sex+higher_educ+city_status+workweek+wed1+wed2, d2)
summary(spec_model)

# log(salary) = -0.478954 - 0.050834*age + 0.346430*sex + 0.323261*higher_educ + 0.250875*city_status +
# + 0.088260*workweek + 0.060366*wed1 + 0.085338*wed2

# ������ ��� ����� ��� ������� ���������� � log(salary). ��� ��� ��� ����, ����� ����� �������� �� ������
# ��������� ����� ������������ �� ���������� � ������� �������� 1.5, �� ���� ������� �� �������� ����������
# log(salary+1.5), �� ��� �������������� ��������, �� �� ������ ������ ����� ������� �� ���������� �����
# ���������, �� ��� �� ������� �� ����� ��� ������� �������� ���������, 
# ��� ��� � �������� ��������� log(salary+1.5)!

# �� �������� ��������� �����, ��� �� ������ ������ ������ ����� ���������� �������(sex = 1) � ������ 
# ������������(higher_educ = 1), ����������� � ������ ��� ��������� ������;
# log(salary) = 0.441612 - 0.050834*age + 0.088260*workweek + 0.060366*wed1 + 0.085338*wed2

# ������, ��� ������ ������������ ����� ������� � ������ ������������, �����������, 
# ��� �� ��������� � �����, ��� ������(wed2 = 1 => wed1 = 0, wed2 �� �������, �� ���� ������� �����):
# salary = 0.52695 - 0.050834*age + 0.088260*workweek
# ������ ������� ����� ������ �������������� � ������ ���� ������������
# ������� ��������� ������� ����������� ���� �������. ������ ����� �� ����� ������������ ������ ����.

                                                  #5.
# ������� ��������� ��� ������������ ���������, ��������� � ��������:
# 1. ��������� ������, ��������� � �����; 
# 2. �����������, ��� ������� �����������.

# log(salary) = -0.478954 - 0.050834*age + 0.346430*sex + 0.323261*higher_educ + 0.250875*city_status +
# + 0.088260*workweek + 0.060366*wed1 + 0.085338*wed2
# 1. log(salary) = -0.167713 - 0.050834*age + 0.346430*sex + 0.323261*higher_educ + 0.088260*workweek
#     ����� ����� ����������� ��� ������ ��������� ���������. ������� ����� �������� ������, �.�.
#      �� ��������� � ������ ��� ��������� ������, �� � ������� �������� � ��������, �.�. �� 
#     ��������� � �����(wed1 = 1 => wed2 = 0, � ��� ������, �. �. ����. ��� wed2 > ����. ��� wed1).
# 2. log(salary) = -0.393616 - 0.050834*age + 0.346430*sex + 0.250875*city_status + 0.088260*workweek
#     ����� ����� ����������� ��� ������ ��������� ���������. ������� ����� ������ ��������, ��� ��� ��,
#     �. �. �� ����� ������ ������ ����� ������ � ���������� ����� ��-�� ��������� ������� �����������
#     (higher_educ = 0), �� ������� ����������(���� ����� ���� �� ������) ��-�� ����, ��� �� ��������
#     (wed2 = 1 => wed1 = 0).