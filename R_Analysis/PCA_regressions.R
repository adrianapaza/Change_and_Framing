setwd()

setwd('/Users/adrianapaza/Documents/ResearchProjects/Fluency_framing')
df<-read.csv('Data/Post_PCA_data.csv')

df$total_signature_count
df$log_signatures_1<-log(df$total_signature_count+1)
attach(df)
hist(as.integer(df$is_victory))

model1<-lm( I(log(total_signature_count+1))~ PCA1 +PCA2+PCA3+PCA4+PCA5,data=df)

summary(model1)


df$title
df$ask
df$media.type
df$targeting_description

df$weekly_signup_count

model2<-glm( I(as.integer(is_victory)-1)~  PCA1 +PCA2+PCA3+PCA4+PCA5+I(log(num_words))+user.locale
             ,data=df,family=binomial(link='logit'))
summary(model2)

model1<-lm( I(log(total_signature_count+1))~ PCA1 +PCA2+PCA3+PCA4+I(log(num_words)),data=df)

summary(model1)
model_shares<-lm(I(log(total_share_count+1)) ~ PCA1 +PCA2+PCA3+PCA4+I(log(num_words)),data=df)

summary(model_shares)

library(stargazer)
stargazer(model1,model_shares)

df$user.locale

df$total_share_count
df$is_verified_victory

df$user.country_code
?glm
colnames(df)
df$tags
df$is_victory
as.integer(df$is_victory)
df$PCA1
hist(df$PCA1)
