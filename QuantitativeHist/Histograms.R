library(formatR)
tidy_source(file = "Histogramoutput.R")

getwd()


C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)

#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")



#####Histograms######
graphics.off()

library(formatR)


par(mfrow=c(3,4))

#Variable: Months

h1 = hist(C_D$Months,col = "blue",breaks = 10,ylim = c(0,300),
          xlim = c(min(C_D$Months),max(C_D$Months)),xlab = "No.of Months", 
          main = "Histogram of Variable: Months")

#Variable:Age




h2 = hist(C_D$Age,col = "blue",breaks = 20,ylim = c(0,150),
          xlim = c(min(C_D$Age),max(C_D$Age)),xlab = "Age in Years", 
          main = "Histogram of Variable: Age")

#Variable:Credit Amount


h3 = hist(C_D$Credit_Amount,col = "blue",breaks = 50,ylim = c(0,220),
          xlim = c(min(C_D$Credit_Amount),max(C_D$Credit_Amount)),xlab = "Credit Amount", 
          main = "Histogram of Variable: Credit_Amount")

#Variable: Residence Since 

h4 = hist(C_D$Residence_Since,col = "blue",breaks = 5,ylim = c(0,220),
          xlim = c(min(C_D$Residence_Since),max(C_D$Residence_Since)),xlab = "Residence_Since", 
          main = "Histogram of Variable: Residence_Since")

#lines(density(C_D$Residence_Since, adjust = 220), col = "orange")

#Variable: Installment Rate

h5 = hist(C_D$Installment_Rate,col = "blue",breaks = 5,ylim = c(0,600),
          xlim = c(min(C_D$Installment_Rate),max(C_D$Installment_Rate)),xlab = "Installment Rate", 
          main = "Histogram of Variable: Installment Rate")

#Variable: Existing Credit


h6 = hist(C_D$Existing_Credit,col = "blue",breaks = 5,ylim = c(0,700),
          xlim = c(min(C_D$Existing_Credit),max(C_D$Existing_Credit)),xlab = "Existing Credit", 
          main = "Histogram of Variable: Existing Credit")


#Variable: Dependents

h7 = hist(C_D$Dependents,col = "blue",breaks = 4,ylim = c(0,1000),
          xlim = c(min(C_D$Dependents),max(C_D$Dependents)),xlab = "Dependents", 
          main = "Histogram of Variable: Dependents")



