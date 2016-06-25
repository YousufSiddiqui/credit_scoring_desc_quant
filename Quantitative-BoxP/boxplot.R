library(formatR)
tidy_source(file = "boxplot.R")

getwd()


C_D = read.table('german.data')

ncol(C_D)
nrow(C_D)

#Setting Variable Names
colnames(C_D) = c("Account_Status","Months","Credit_History","Credit_Purpose","Credit_Amount","Savings_Account"
                  ,"Employed_Since","Installment_Rate","Status_and_Sex","Other_Debtors","Residence_Since",
                  "Property","Age","Other_Installement_Plans","Housing_Type","Existing_Credit","Job_Type",
                  "Dependents","Telephone","Foreign_Worker","Cost_Matrix")

#####Boxplots######
graphics.off()

par(mfrow=c(3,2))

#Variable: Months
mmonths = mean(C_D$Months)


boxplot(Months~Cost_Matrix, data=C_D, main="Boxplot of Variable: Months by Decision", 
        xlab="Decision", ylab="No.of Months", col="gray")
abline(h=mean(mmonths), col="red")

#Variable: Age

mAge = mean(C_D$Age)

boxplot(Age~Cost_Matrix, data=C_D, main="Boxplot of Variable: Age by Decision", 
        xlab="Decision", ylab="Age", col="gray", ylim= c(15,65))
abline(h=mean(mAge), col="red")


#Variable: Credit Amount

mCredit_Amount = mean(C_D$Credit_Amount)

boxplot(Credit_Amount~Cost_Matrix, data=C_D, main="Boxplot of Variable: Credit Amount by Decision", 
        xlab="Decision", ylab="Credit Amount", col="gray",ylim= c(0,10000), breaks = 2000)
abline(h=mean(mCredit_Amount), col="red")


#Variable: Installment Rate

mInstallment_Rate = mean(C_D$Installment_Rate)

boxplot(Installment_Rate~Cost_Matrix, data=C_D, main="Boxplot of Variable: Installment Rate by Decision", 
        xlab="Decision", ylab="Installment Rate (%)", col="gray", ylim = c(0.5,4.5))
abline(h=mean(mInstallment_Rate), col="red")


#Variable: Residence Since

mResidence_Since = mean(C_D$Residence_Since)

boxplot(Residence_Since~Cost_Matrix, data=C_D, main="Boxplot of Variable: Residence Since by Decision", 
        xlab="Decision", ylab="Residence Since", col="gray", ylim = c(0.5,4.5))
abline(h=mean(mResidence_Since), col="red")


#Variable: Existing Credit

mExisting_Credit = mean(C_D$Existing_Credit)

boxplot(Existing_Credit~Cost_Matrix, data=C_D, main="Boxplot of Variable: Existing Credit by Decision", 
        xlab="Decision", ylab="Existing Credit", ylim = c(0.5,3.5), col="gray")
abline(h=mean(mExisting_Credit), col="red")




