########################################################################################
#
# Author: Ishani Jariwala
# Purpose: Poster 
# Date: 11/12/2016
#
########################################################################################

#WA_Fn-UseC_-HR-Employee-Attrition.csv file read
#Choosing file and saving it in temperory directory
temp.dir <- file.choose()
temp.dir
#Assigning the path
#my.path <- "C:\\Users\\Ishani\\Desktop\\Syracuse University\\Fall 2016\\IST 719\\"
my.path <- "\\\\hd.ad.syr.edu\\01\\229d5e\\Documents\\Desktop\\Syracuse University\\Fall2016\\IST719\\Project\\"
#Listing all the files in the mentioned path
list.files(my.path)

# Using paste function to Concatenate vectors after converting to character
dataHR <- paste(my.path, "WA_Fn-UseC_-HR-Employee-Attrition.csv", sep = "")
dataHR

#Reading the csv file
emp <- read.csv(file = dataHR
                    , header = TRUE
                    , stringsAsFactors = FALSE
                    , sep = ",")
str(emp)
colnames(emp)

emp.attr <- emp[emp$Attrition == "Yes",]

summary(emp.attr)

emp.inc <- table(emp.attr$BusinessTravel,emp.attr$Department)

#BesidePlot - In each department, how much is each type of 
#business travel where attrition is "Yes"
barplot(emp.inc,beside = TRUE, col=c("orange","darkblue","red")
        , xlab = "Departments"
        , main = "Attrition Distribution by Business Travel type")
legend('topleft', legend = rownames(emp.inc) , lwd=5, bty=1)

# RESULTS
#Highest Attrition Number for R&D Department
#Travel frequency is travel rarely so not giving them 
#enough onsite oppurtunity

# BoxPlot - Dept wise box plot for monthly income for attrition yes
boxplot(MonthlyIncome~Department,data=emp.attr
        ,main="Department wise monthly salary"
        ,ylab="Monthly Income", xlab="Departments"
        ,col = c("orange","darkblue","red"))

#RESULTS
# HR and R&D has almost the same avg monthly salary
# Whereas Sales has higher avg salary

agg.hike <- aggregate(as.numeric(emp.attr$PercentSalaryHike)
                      , by = list(
                        dept = emp.attr$Department )
                      , FUN=mean)
View(agg.hike)
barplot(agg.hike$x,names.arg = agg.hike$dept
        ,main="Department wise salary hike"
        ,ylab="Salary Hike", xlab="Departments"
        ,col = c("orange","darkblue","red"))

#RESULT
#Though avg percentage hike is high, 
#the avg monthly income is not that high

View(table(emp.attr$Department))
barplot(table(emp.attr$Department))

emp.rnd <- emp.attr[emp.attr$Department == "Research & Development",]

#Attrition based on education for R&D dept
pie(table(emp.rnd$EducationField))

#Avg Num of yrs since last promotion by job role 
#for Life Sciences education

emp.ls <- emp.rnd[emp.rnd$EducationField == "Life Sciences",]
agg.yrs <- aggregate(as.numeric(emp.ls$YearsSinceLastPromotion)
                     , by = list(jrole = emp.ls$JobRole)
                     , FUN = mean)
par(mfrow=c(1,2))

#barplot(cbind(agg.yrs$x,table(emp.ls$JobRole)),beside=TRUE,names.arg = c("Num of Yrs Since Last Promotion","Num of Attrition"),legend = agg.yrs$jrole)
barplot(agg.yrs$x,names.arg = agg.yrs$jrole,las=2)
#Attritrion for Life Sciences education
barplot(table(emp.ls$JobRole),las=2)

#RESULTS
#Attritrion for Life Sciences education for Laboratory Technician role is
#high as the avg num of yrs since last promotion is quite low for that role

#Medical - Plot
emp.med <- emp.rnd[emp.rnd$EducationField == "Medical",]
View(emp.med)
agg.per <- aggregate(as.numeric(emp.med$PerformanceRating)
                     , by = list(jrole = emp.med$JobRole)
                     , FUN = mean)
pie(table(emp.med$JobSatisfaction))

#Technical Degree - Plot
emp.tech <- emp.rnd[emp.rnd$EducationField == "Technical Degree",]
pie(table(emp.tech$WorkLifeBalance))
