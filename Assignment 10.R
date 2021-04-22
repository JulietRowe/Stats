data = read.table("anova_assignment_data.txt")
names(data) = c("subjects", "age", "gender", "rt")

rt = data$rt
age = factor(data$age)
gender = factor(data$gender)
model = aov(rt~age + gender + age*gender)
summary(model)


#Effects of age
plot(rt~age)

#Effects of gender
plot(rt~gender)

#Interaction
interaction.plot(age, gender, rt)

#Testing homogenity of variance
libray('car')
leveneTest(rt, age)
leveneTest(rt, gender)
leveneTest(rt, interaction(age, gender))

#Testing normality
shapiro.test(rt[age == 1])
shapiro.test(rt[age == 2])
shapiro.test(rt[age == 3])
shapiro.test(rt[gender == 1])
shapiro.test(rt[gender == 2])

#Young (group 3) appear to have reaction times that are faster
#Than the other two age groups
#Females have faster reaction times than males

TukeyHSD(model)

#Can also run an independent samples t-test at each level of age to see where the difference lies
age1 = subset(data, age == 1)
t.test(age1$rt~age1$gender)
age2 = subset(data, age == 2)
t.test(age2$rt~age2$gender)
age3 = subset(data, age == 3)
t.test(age3$rt~age3$gender)

#Alternative split data into gender
gender1 = subset(data, gender == 1)
model1 = aov(gender1$rt~factor(gender1$age))
summary(model1)

gender2 = subset(data,gender == 2)
model2 = aov(gender2$rt~factor(gender2$age))
summary(model2)
TukeyHSD(model2)
