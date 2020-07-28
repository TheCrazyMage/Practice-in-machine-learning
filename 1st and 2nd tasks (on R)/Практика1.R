#Выполнил ученик КМБО-01-18 Алиев Мишан(вар. 1)

#Чтобы программа заработала, надо пробежаться с начала с помощью ctrl + Enter
library("psych")
library("dplyr")
library("ggplot2")
library("GGally")
library("car")

#посмотрим на набор данных
data = swiss
data #выводит сам датасет в консоль сп.1
help(data) #выводит информацию по датасету
glimpse(data) #выводит сам датасет в консоль сп.2

#выберем лишь те переменные, с котрыми будем работать
d = select(data, Fertility, Agriculture, Education, Catholic)
d

                                            #№1
#Нормализуем данные, вычтя из каждого столбца среднее значение mean(x) и
#поделив на среднеквадратическое отклонение сигма ~ sqrt(var(x)), где x – столбец
#данных
d[1] = (d[1] - mean(d$Fertility)) / sqrt(var(d$Fertility))
d[2] = (d[2] - mean(d$Agriculture)) / sqrt(var(d$Agriculture))
d[3] = (d[3] - mean(d$Education)) / sqrt(var(d$Education))
d[4] = (d[4] - mean(d$Catholic)) / sqrt(var(d$Catholic))
d
describe(d)
summary(d)

                                          #№2
#Проверить, что в наборе данных нет линейной зависимости, если же она есть,
#то исключить соответсвующее значение(смотреть по R^2)

#Построим модели зависимости между регрессорами и оценим их R^2
m1 = lm(Fertility~Agriculture+Education+Catholic, d)
summary(m1)
# R^2 примерно равен 64 %, что говорит о лин. независимости регрессоров
# посмотрим vif регрессоров
vif(m1)
# vif < 2.1, что и подтвеждает нашу догадку

                                            #№3
#Построить линейную модель зависимой переменной от указанных в варианте 
#регрессоров по методу наименьших квадратов. Оценить удачность модели согласно: 
#1) R^2, 2) p-значениям каждого коэффициента

model1 = lm(Fertility~Agriculture+Education+Catholic, d)
model1
summary(model1)

# Из данных получаем, что multiple R-squared = 0.6423,	adjusted R-squared = 0.6173;
# p-занчения каждого коэффицента хороши(2-3 звездочки), за исключением свободного члена,
# объединяющий все осталные регрессоры, так что убрать мы его не можем.
# В целом, для первой модели результат неплох!


                                              #№4 
# Ввести в модель логарифмы регрессоров. Сравнить модели и выбрать наилучшую.

#Так как при начальной нормализации данных выяснолось, что там есть отрицательные значения,
#мы не можем логарифмировать регрессоры. Но можно к столбцам с отрицательными значениями
#прибавить минимальное число для приведения их к положительным.

describe(d)#отсюда узнаем мин отриц. зн-я и добавляем к столбцам их округленное 
d[2] = d[2] + 2.2
d[3] = d[3] + 1.1
d[4] = d[4] + 1
d[1] = d[1] + 3
d

model2 = lm(Fertility~Agriculture+Education+Catholic, d)
summary(model2)
vif(model2)
# по сравнению с model1 ничего не изменилосью

model3 = lm(Fertility~Agriculture+Education+Catholic+log(Agriculture)+log(Education)+log(Catholic), d)
summary(model3)
vif(model3)
#Multiple R-squared = 0.6492(немного увеличился), Adjusted R-squared = 0.5966(уменьшился). 
#P-значение всех коэффицентов кроме свободного и Education ужасны. 
#Vif всех коэффицентов больше 5, что говорит лин. зависимости между ними.
#Попробуем еще модели(возьмем те, где vif меньше)

model4 = lm(Fertility~Agriculture+Education+Catholic+log(Agriculture), d)
summary(model4)
vif(model4)
# Данная модель чуть лучше предыдущей, т. к. р-значения коэффицентов все хороши(по три звезды) кроме
# Agriculture(1 звзеда) и log(Agriculture)(0 звзед), что говорит о ненужности логарифма. Vif сильно уменьшились(все < 5),
# но vif(Agriculture) и vif(log(Agriculture)) около 5.
# Multiple R-squared:  0.6448,	Adjusted R-squared:  0.611.

model5 = lm(Fertility~Agriculture+Education+Catholic+log(Education), d)
summary(model5)
vif(model5)
# Эта модель практически ничем не отличается от предыдущей.

model6 = lm(Fertility~Agriculture+Education+Catholic+log(Catholic), d)
summary(model6)
vif(model6)
# Catholic и log(Catholic) сильно лин зависимы. 

spec_model = lm(Fertility~log(Agriculture)+Education+Catholic, d)
summary(spec_model)
vif(spec_model)
# введение логарифмов вместо самих регрессоров также ничего не дало

# Введение логарифма сильно полезного ничего не дало, линейная модель лучше.

                                      #5 
# Ввести в модель всевозможные произведения из пар регрессоров, в том числе
# квадраты регрессоров. Найдите одну или несколько наилучших моделей по доле
# объяснённого разброса в данных R^2

summary(model2)$r.squared # = 0.6422541

model7 = lm(Fertility~Agriculture+Education+Catholic+I(Agriculture*Education), d)
summary(model7)
vif(model7)
# Multiple R-squared:  0.6426,	Adjusted R-squared:  0.6085
# P-значения регрессора I(Agriculture * Education) и Agriculture 0 и 1 звезда
# соответственно, остальные по три звезды. Все vif < 2.6. 
# В целом введение множителя I(Agriculture*Education) ничего особо ценного не дало.

model8 = lm(Fertility~Agriculture+Education+Catholic+I(Agriculture*Catholic), d)
summary(model8)
vif(model8)
# Vif(I(Agriculture * Catholic)) = 16.170904, vif(Catholic) = 12.076665, модель не подходит.
# Multiple R-squared:  0.6426,	Adjusted R-squared:  0.6086 

model9 = lm(Fertility~Agriculture+Education+Catholic+I(Education*Catholic), d)
summary(model9)
vif(model9)
# Модель получилась достаточно противоречивой.
# С одной стороны, Multiple R-squared:  0.6592,	Adjusted R-squared:  0.6267, с другой
# р-значения I(Education * Catholic) - 0 звезд, vif(Education) = 5.414695, 
# I(Education * Catholic) = 5.832477.

model10 = lm(Fertility~Agriculture+Education+Catholic+I(Agriculture^2), d)
summary(model10)
vif(model10)
# vif(Agriculture) = 20.740242, vif(I(Agriculture^2)) = 18.703768, модель не подходит.
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6179.

model11 = lm(Fertility~Agriculture+Education+Catholic+I(Education^2), d)
summary(model11)
vif(model11)
# vif(Education) = 10.392245, vif(I(Education^2)) = 8.540909, модель не подходит.
# Multiple R-squared:  0.6437,	Adjusted R-squared:  0.6098.

model12 = lm(Fertility~Agriculture+Education+Catholic+I(Catholic^2), d)
summary(model12)
vif(model12)
# vif(Catholic) = 76.136499, vif(I(Catholic^2)) = 81.211267, модель не подходит.
# Multiple R-squared:  0.6527,	Adjusted R-squared:  0.6196 

# Итого, лучшая модели по доле разброса в данных R^2 - model7, model9.
# Model7 - немного худшая версия model1.
# Model9 - противоречивая модель, но прирост R^2 происходит(на примерно 1%).