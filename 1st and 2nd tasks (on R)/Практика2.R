library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")
library("Hmisc")


data = rlms_read("C:\\Users\\Mishan\\Desktop\\Программирование\\Практика\\r24i_os26c.sav")
glimpse(data)
d = select(data, tj13.2, t_age, th5, t_educ, status, tj6.2, t_marst)

#исключаем строки с отсутствующими значениями NA
d = na.omit(d)

                            # Изменим названия столбцов и преобразуем их значения
# семейное положение
d["wed1"]=d$t_marst
d["wed1"]=0
# поставим 1, если респондент женат
d$wed1[which(d$t_marst == '2')] = 1 # состоит в зарегистрированном браке
d$wed1[which(d$t_marst == '6')] = 1 # официально зарегистрированы, но вместе не проживают
d$wed1 = as.numeric(d$wed1)

d["wed2"]=d$t_marst
d["wed2"]=0
d$wed2[which(d$t_marst == '4')] = 1 # разведен и в браке не состоит 
d$wed2[which(d$t_marst == '5')] = 1 # вдовец(вдова)
d$wed2 = as.numeric(d$wed2)

d["wed3"]=d$t_marst
d["wed3"]=0
d$wed3[which(d$t_marst == '1')] = 1 # никогда в браке не состоял
d$wed3 = as.numeric(d$wed3)

#пол
d["sex"]=d$th5
d$sex[which(d$sex == '2')] = 0 # женщина
d$sex[which(d$sex == '1')] = 1 # мужчина
d$sex = as.numeric(d$sex)

# населенный пункт
d["city_status"]=d$status
d["city_status"]=0
d$city_status[which(d$status == '1')] = 1 # областной центр
d$city_status[which(d$status == '2')] = 1 # город
d$city_status = as.numeric(d$city_status)

# высшее образование
d["higher_educ"]=d$t_educ
d["higher_educ"]=0
d$higher_educ[which(d$t_educ == '21')] = 1 # есть диплом о высшем образовании 
d$higher_educ[which(d$t_educ == '22')] = 1 # аспирантура и т.п. без диплома
# случай 22 характеризуется тем, что есть диплом о высшем образовании, но нет диплома аспирантуры
d$higher_educ[which(d$t_educ == '23')] = 1 #аспирантура и т.п. с дипломом 
d$higher_educ = as.numeric(d$higher_educ)

                                # Нормализуем данные некоторых столбцов

# зарплата
d["salary"]=d$tj13.2
d$salary = as.numeric(d$salary)
d["salary"] = (d["salary"] - mean(d$salary)) / sqrt(var(d$salary))

# возраст
d["age"]=d$t_age
d$age = as.numeric(d$age)
d["age"] = (d["age"] - mean(d$age)) / sqrt(var(d$age))

# продолжительность рабочей недели в часах
d["workweek"]=d$tj6.2
d$workweek = as.numeric(d$workweek)
d["workweek"] = (d["workweek"] - mean(d$workweek)) / sqrt(var(d$workweek))

d2 = select(d, salary, age, sex, higher_educ, city_status, workweek, wed1, wed2, wed3)
d2

                                                    #1 
# Постройте линейную регрессию зарплаты на все параметры, которые Вы выделили
# из данных мониторинга. Не забудьте оценить коэффициент вздутия дисперсии VIF

model1 = lm(salary~age+sex+higher_educ+city_status+workweek+wed1+wed2+wed3, d2)
vif(model1)
# Vif для всех регрессоров < 2.4, что говорит о лин независимости лин. регрессоров.

summary(model1)
# Из данных можно сказать, что все регрессоры, кроме wed2(1 звезда) и wed3(0 звёзд) неплохо описывают данные(2-3 звезды),
# разброс они описывают маленький: multiple R-squared = 0.175,	adjusted R-squared = 0.1735.

#Попробуем удалить регрессор wed3:
model2 = lm(salary~age+sex+higher_educ+city_status+workweek+wed1+wed2, d2)
vif(model2)
# все регрессоры лин независимы
summary(model2)
# Multiple R-squared:  0.1745,	Adjusted R-squared:  0.1732,
# р-значение всех регрессоров увеличились до трех звезд.

                                                  #2
#Поэкспериментируйте с функциями вещественных параметров: используйте
# логарифм и степени

# в некоторых наших столбцах есть отрицательные значения,
# добавим им мин значения, которые доведут их до положительных

describe(d2)
# salary lowest = -1.396326
d2["salary"] = d2["salary"] + 1.5

# age lowest = -1.969413 
d2["age"] = d2["age"] + 2

# workweek lowest = -3.362522
d2["workweek"] = d2["workweek"] + 3.5

# Наверное, имеет смысл работать с некатегориальными регрессорами.
# Это регрессоры salary, age и workweek

model3 = lm(log(salary)~age+sex+higher_educ+city_status+workweek+wed1+wed2+log(age)+log(workweek), d2)
summary(model3)
vif(model3)
#  Multiple R-squared:  0.2201,	Adjusted R-squared:  0.2185
#  p-значения wed1, wed2 плохи(0 звезд и точка), у workweek - одна звезда, остальные регрессоры по 3.
#  Vif большей части регрессоров нормальны.
#  Но vif log(age) и age около 10, log(workweek) и workweek около 6. Это скорее говорит о лин зависимости.

# попробуем убрать логарифм с salary
model4 = lm(salary~age+sex+higher_educ+city_status+workweek+wed1+wed2+log(age)+log(workweek), d2)
summary(model4)
vif(model4)
# Vif не изменился => модель не подходит.
# Multiple R-squared:  0.1874,	Adjusted R-squared:  0.1858 
# Р-значения всех регрессоров хороши(3 звезды), кроме workweek(0 звезд), но мы видим, что потеряли
# часть значений в разбросе данных

# уберем регрессор workweek(0 звезд) и регрессор log(age)(p-значение меньше, чем у age)
model5 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek), d2)
summary(model5)
vif(model5)
# Multiple R-squared:  0.1768,	Adjusted R-squared:  0.1754.
# Р-значения всех регрессоров по 3 звезды.
# vif < 1.7, что просто отлично.
# С логарифмами мы получили отличную модель, попробуем улучшить её
# Multiple R-squared и Adjusted R-squared с помощью прибавления регрессора в какой-то степени.

model6 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek) + I(age*age), d2)
summary(model6)
vif(model6)
# age и I(age*age) лин зависимы, модель нам не подходит.

model7 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek) + I(age^1.3), d2)
vif(model7)
summary(model7)
# При подстановке других степеней age и I(age*age) лин зависимы, модель нам не подходит.
# Попробуем другой параметр.

model8 = lm(salary~age+sex+higher_educ+city_status+wed1+wed2+log(workweek) + I((log(workweek))^2), d2)
vif(model8)
summary(model8)
# При работе со степенями и workweek, и log(workweek) ничего дельного не вышло. То в модели были линейно
# зависимые регрессоры, то Multiple R-squared и Adjusted R-squared не менялись, а p-значения
# вводимого аргумента не менялись и оставались 0 звёзд.

                                                    #3. 
#Выделите наилучшие модели из построенных: по значимости параметров,
#включённых в зависимости, и по объяснённому с помощью построенных зависимостей разбросу adjusted R^2.

# Лучшие модели - model5(используется log(workweek)), model2(просто линейная модель)
# Учитывая противоречивость model5, лучшей становится обычная линейная модель model2.
# Протестируя несколько различных моделей стало ясно, что добавление к параметру salary логарифма
# даёт прирост R^2 и adj R^2, хотя мы всегда теряем в p-значениях. Попробуем это проделать с линейной 
# моделью, т.е. добавим к ней логарифм объясняемой переменной. Построим специальную модель:

spec_model = lm(log(salary)~age+sex+higher_educ+city_status+workweek+wed1+wed2, d2)
summary(spec_model)
vif(spec_model)
# Multiple R-squared:  0.1974,	Adjusted R-squared:  0.1961
# p-значения всех переменных 2-3 звезды(2 звезды у wed1 и wed2, что тоже неплохо)
# однозначно, это наша лучшая модель
                                                    #4 
# Сделайте вывод о том, какие индивиды получают наибольшую зарплату.

# Вывод мы сможем сделать написав лин регрессию параметра "salary"

spec_model = lm(log(salary)~age+sex+higher_educ+city_status+workweek+wed1+wed2, d2)
summary(spec_model)

# log(salary) = -0.478954 - 0.050834*age + 0.346430*sex + 0.323261*higher_educ + 0.250875*city_status +
# + 0.088260*workweek + 0.060366*wed1 + 0.085338*wed2

# Теперь нам стоит еще немного поработать с log(salary). Так как для того, чтобы взять логарифм от данной
# категории после нормализации мы прибавилии к столбцу значений 1.5, то есть реально мы описвали переменную
# log(salary+1.5), но так преобразование линейное, то мы просто должны будем вычесть из свободного члена
# константу, но это не повляет на общий вид картины описания индивидов, 
# так что я продолжу описывать log(salary+1.5)!

# из линейной регрессии видно, что на данный момент больше всего заработает мужчина(sex = 1) с высшим 
# образованием(higher_educ = 1), проживающий в городе или областном центре;
# log(salary) = 0.441612 - 0.050834*age + 0.088260*workweek + 0.060366*wed1 + 0.085338*wed2

# Поймем, что больше зарабатывать будет мужчина с высшим образованием, разведенный, 
# или не состоящий в браке, или вдовец(wed2 = 1 => wed1 = 0, wed2 на немного, но дает больший вклад):
# salary = 0.52695 - 0.050834*age + 0.088260*workweek
# Данный мужчина также должен перерабатывать и должен быть относительно
# времени получения высшего образования быть молодым. Именно тогда он будет зарабатывать больше всех.

                                                  #5.
# Оцените регрессии для подмножества индивидов, указанных в варианте:
# 1. городские жители, состоящие в браке; 
# 2. разведенные, без высшего образования.

# log(salary) = -0.478954 - 0.050834*age + 0.346430*sex + 0.323261*higher_educ + 0.250875*city_status +
# + 0.088260*workweek + 0.060366*wed1 + 0.085338*wed2
# 1. log(salary) = -0.167713 - 0.050834*age + 0.346430*sex + 0.323261*higher_educ + 0.088260*workweek
#     Такая будет зависимость для первой категории индивидов. Индивид будет получать больше, т.к.
#      он проживает в городе или областном центре, но и немного потеряет в зарплете, т.к. он 
#     находится в браке(wed1 = 1 => wed2 = 0, а это потеря, т. к. коэф. при wed2 > коэф. при wed1).
# 2. log(salary) = -0.393616 - 0.050834*age + 0.346430*sex + 0.250875*city_status + 0.088260*workweek
#     Такая будет зависимость для второй категории индивидов. Индивид будет меньше получать, чем мог бы,
#     т. к. он очень сильно теряет часть баллов к свободному члену из-за отсутсвия высшего образования
#     (higher_educ = 0), но немного отыгравает(хотя очень даже не сильно) из-за того, что он разведен
#     (wed2 = 1 => wed1 = 0).