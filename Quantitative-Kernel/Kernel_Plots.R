library(formatR)
tidy_source(file = "Kerneloutput.R")

graphics.off()


C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)

#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")




#####Kernel Density Plots######



par(mfrow=c(3,4))

d1 = density(C_D$Months)  
plot(d1,main = "Kernel Density:Months")
polygon(d1, col="red", border="blue")

d2 = density(C_D$Age)  
plot(d2,main = "Kernel Density:Age")
polygon(d2, col="red", border="blue")

d3 = density(C_D$Credit_Amount)  
plot(d3,main = "Kernel Density:Credit Amount")
polygon(d3, col="red", border="blue")

d4 = density(C_D$Residence_Since)  
plot(d4,main = "Kernel Density:Residence_Since")
polygon(d4, col="red", border="blue")

d5 = density(C_D$Installment_Rate)  
plot(d5,main = "Kernel Density:Installment_Rate")
polygon(d5, col="red", border="blue")

d6 = density(C_D$Existing_Credit)  
plot(d6,main = "Kernel Density:Existing_Credit")
polygon(d6, col="red", border="blue")

d7 = density(C_D$Dependents)  
plot(d7,main = "Kernel Density:Dependents")
polygon(d7, col="red", border="blue")


