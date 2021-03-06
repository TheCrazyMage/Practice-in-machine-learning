#�������� ������ ����-01-18 ����� �����(���. 1)

#����� ��������� ����������, ���� ����������� � ������ � ������� ctrl + Enter
library("psych")
library("dplyr")
library("ggplot2")
library("GGally")
library("car")

#��������� �� ����� ������
data = swiss
data #������� ��� ������� � ������� ��.1
help(data) #������� ���������� �� ��������
glimpse(data) #������� ��� ������� � ������� ��.2

#������� ���� �� ����������, � ������� ����� ��������
d = select(data, Fertility, Agriculture, Education, Catholic)
d

                                            #�1
#����������� ������, ����� �� ������� ������� ������� �������� mean(x) �
#������� �� �������������������� ���������� ����� ~ sqrt(var(x)), ��� x � �������
#������
d[1] = (d[1] - mean(d$Fertility)) / sqrt(var(d$Fertility))
d[2] = (d[2] - mean(d$Agriculture)) / sqrt(var(d$Agriculture))
d[3] = (d[3] - mean(d$Education)) / sqrt(var(d$Education))
d[4] = (d[4] - mean(d$Catholic)) / sqrt(var(d$Catholic))
d
describe(d)
summary(d)

                                          #�2
#���������, ��� � ������ ������ ��� �������� �����������, ���� �� ��� ����,
#�� ��������� �������������� ��������(�������� �� R^2)

#�������� ������ ����������� ����� ������������ � ������ �� R^2
m1 = lm(Fertility~Agriculture+Education+Catholic, d)
summary(m1)
# R^2 �������� ����� 64 %, ��� ������� � ���. ������������� �����������
# ��������� vif �����������
vif(m1)
# vif < 2.1, ��� � ����������� ���� �������

                                            #�3
#��������� �������� ������ ��������� ���������� �� ��������� � �������� 
#����������� �� ������ ���������� ���������. ������� ��������� ������ ��������: 
#1) R^2, 2) p-��������� ������� ������������

model1 = lm(Fertility~Agriculture+Education+Catholic, d)
model1
summary(model1)

# �� ������ ��������, ��� multiple R-squared = 0.6423,	adjusted R-squared = 0.6173;
# p-�������� ������� ����������� ������(2-3 ���������), �� ����������� ���������� �����,
# ������������ ��� �������� ����������, ��� ��� ������ �� ��� �� �����.
# � �����, ��� ������ ������ ��������� ������!


                                              #�4 
# ������ � ������ ��������� �����������. �������� ������ � ������� ���������.

#��� ��� ��� ��������� ������������ ������ ����������, ��� ��� ���� ������������� ��������,
#�� �� ����� ��������������� ����������. �� ����� � �������� � �������������� ����������
#��������� ����������� ����� ��� ���������� �� � �������������.

describe(d)#������ ������ ��� �����. ��-� � ��������� � �������� �� ����������� 
d[2] = d[2] + 2.2
d[3] = d[3] + 1.1
d[4] = d[4] + 1
d[1] = d[1] + 3
d

model2 = lm(Fertility~Agriculture+Education+Catholic, d)
summary(model2)
vif(model2)
# �� ��������� � model1 ������ �� �����������

model3 = lm(Fertility~Agriculture+Education+Catholic+log(Agriculture)+log(Education)+log(Catholic), d)
summary(model3)
vif(model3)
#Multiple R-squared = 0.6492(������� ����������), Adjusted R-squared = 0.5966(����������). 
#P-�������� ���� ������������ ����� ���������� � Education ������. 
#Vif ���� ������������ ������ 5, ��� ������� ���. ����������� ����� ����.
#��������� ��� ������(������� ��, ��� vif ������)

model4 = lm(Fertility~Agriculture+Education+Catholic+log(Agriculture), d)
summary(model4)
vif(model4)
# ������ ������ ���� ����� ����������, �. �. �-�������� ������������ ��� ������(�� ��� ������) �����
# Agriculture(1 ������) � log(Agriculture)(0 �����), ��� ������� � ���������� ���������. Vif ������ �����������(��� < 5),
# �� vif(Agriculture) � vif(log(Agriculture)) ����� 5.
# Multiple R-squared:  0.6448,	Adjusted R-squared:  0.611.

model5 = lm(Fertility~Agriculture+Education+Catholic+log(Education), d)
summary(model5)
vif(model5)
# ��� ������ ����������� ����� �� ���������� �� ����������.

model6 = lm(Fertility~Agriculture+Education+Catholic+log(Catholic), d)
summary(model6)
vif(model6)
# Catholic � log(Catholic) ������ ��� ��������. 

spec_model = lm(Fertility~log(Agriculture)+Education+Catholic, d)
summary(spec_model)
vif(spec_model)
# �������� ���������� ������ ����� ����������� ����� ������ �� ����

# �������� ��������� ������ ��������� ������ �� ����, �������� ������ �����.

                                      #5 
# ������ � ������ ������������ ������������ �� ��� �����������, � ��� �����
# �������� �����������. ������� ���� ��� ��������� ��������� ������� �� ����
# ������������ �������� � ������ R^2

summary(model2)$r.squared # = 0.6422541

model7 = lm(Fertility~Agriculture+Education+Catholic+I(Agriculture*Education), d)
summary(model7)
vif(model7)
# Multiple R-squared:  0.6426,	Adjusted R-squared:  0.6085
# P-�������� ���������� I(Agriculture * Education) � Agriculture 0 � 1 ������
# ��������������, ��������� �� ��� ������. ��� vif < 2.6. 
# � ����� �������� ��������� I(Agriculture*Education) ������ ����� ������� �� ����.

model8 = lm(Fertility~Agriculture+Education+Catholic+I(Agriculture*Catholic), d)
summary(model8)
vif(model8)
# Vif(I(Agriculture * Catholic)) = 16.170904, vif(Catholic) = 12.076665, ������ �� ��������.
# Multiple R-squared:  0.6426,	Adjusted R-squared:  0.6086 

model9 = lm(Fertility~Agriculture+Education+Catholic+I(Education*Catholic), d)
summary(model9)
vif(model9)
# ������ ���������� ���������� ��������������.
# � ����� �������, Multiple R-squared:  0.6592,	Adjusted R-squared:  0.6267, � ������
# �-�������� I(Education * Catholic) - 0 �����, vif(Education) = 5.414695, 
# I(Education * Catholic) = 5.832477.

model10 = lm(Fertility~Agriculture+Education+Catholic+I(Agriculture^2), d)
summary(model10)
vif(model10)
# vif(Agriculture) = 20.740242, vif(I(Agriculture^2)) = 18.703768, ������ �� ��������.
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6179.

model11 = lm(Fertility~Agriculture+Education+Catholic+I(Education^2), d)
summary(model11)
vif(model11)
# vif(Education) = 10.392245, vif(I(Education^2)) = 8.540909, ������ �� ��������.
# Multiple R-squared:  0.6437,	Adjusted R-squared:  0.6098.

model12 = lm(Fertility~Agriculture+Education+Catholic+I(Catholic^2), d)
summary(model12)
vif(model12)
# vif(Catholic) = 76.136499, vif(I(Catholic^2)) = 81.211267, ������ �� ��������.
# Multiple R-squared:  0.6527,	Adjusted R-squared:  0.6196 

# �����, ������ ������ �� ���� �������� � ������ R^2 - model7, model9.
# Model7 - ������� ������ ������ model1.
# Model9 - �������������� ������, �� ������� R^2 ����������(�� �������� 1%).