library(dplyr)
a<-matrix(c(0,1,0,0,0,0,
            1,0,1,0,0,0,
            0,1,0,1,0,0,
            0,0,1,0,1,1,
            0,0,0,1,0,0,
            0,0,0,1,0,0), nrow = 6, byrow = TRUE);
a;
a%*%a %>% (\(x){diag(x)<-0;x});
a%*%a%*%a %>% (\(x){diag(x)<-0;x<-ifelse(x>0,1,x);x})*-(a-1);
a%*%a%*%a%*%a %>% (\(x){diag(x)<-0;x<-ifelse(x>0,1,x);x})*-(a+a%*%a %>% (\(x){diag(x)<-0;x})-1)



a <- ifelse(a==1, 2, a)

a;
a%*%a %>% (\(x){diag(x)<-0;x}) / max(a);
a%*%a%*%a %>% (\(x){diag(x)<-0;x})*-(a-1) %>% (\(x){x<-ifelse(x<0,0,x);x}) / max(a%*%a %>% (\(x){diag(x)<-0;x}));
a%*%a%*%a%*%a %>% (\(x){diag(x)<-0;x})*-(a+a%*%a %>% (\(x){diag(x)<-0;x})-1) %>% (\(x){x<-ifelse(x<0,0,x);x}) / max(a%*%a%*%a %>% (\(x){diag(x)<-0;x})*-(a-1) %>% (\(x){x<-ifelse(x<0,0,x);x}));



a <- ifelse(a==2, 0.5, a)

a;
a%*%a %>% (\(x){diag(x)<-0;x}) / max(a);
a%*%a%*%a %>% (\(x){diag(x)<-0;x})*-(ifelse(a==0,0,1)-1) / max(a%*%a %>% (\(x){diag(x)<-0;x}));
a%*%a%*%a%*%a %>% (\(x){diag(x)<-0;x})*-(ifelse((a+(a%*%a %>% (\(x){diag(x)<-0;x}) / max(a)))==0,0,1)-1) / max(a%*%a%*%a %>% (\(x){diag(x)<-0;x})*-(ifelse(a==0,0,1)-1));

a<-matrix(c(0,1,0,0,0,0,
            1,0,2,0,0,0,
            0,2,0,2,0,0,
            0,0,2,0,1,1,
            0,0,0,1,0,0,
            0,0,0,1,0,0), nrow = 6, byrow = TRUE)





