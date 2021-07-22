# Paypal_case
Case submission for Paypal
input<-read.csv("data.csv")
#loop to calculate rank
#to store updated values
input_up<-data.frame()
for( i in unique(input$match_day))
{
  match_data<-input[which(input$match_day==i),]
  match_data$Rank<-ifelse(match_data$score>=mean(match_data$score),1,
                          ifelse(match_data$score==mean(match_data$score),0,2))
  input_up<-rbind(input_up,match_data)
  
}
#test if the hand is significant for either of them
#for Boris first
bor_hand<-chisq.test(input_up[which(input_up$name=="Boris"),]$hand,input_up[which(input_up$name=="Boris"),]$Rank)
#it is significant for Boris
ari_hand<-chisq.test(input_up[which(input_up$name=="Arielle"),]$hand,input_up[which(input_up$name=="Arielle"),]$Rank)
#it is not significant for Arielle
#using both the variables for AOV
input_up$Rank<-as.character(input_up$Rank)
aov_op<-aov(score~name+hand,data=input_up)
summary(aov_op)
aov_op<-aov(score~name*hand,data=input_up)
summary(aov_op)
aov_op<-aov(Rank~name*hand,data=input_up)
summary(aov_op)
#regardless of the using rank or score, only hand is turning out to be significant, not the names
#hence of Arielle and Boris none is better than the other
chisq.test(input_up$Rank,input_up$name)
