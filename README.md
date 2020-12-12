# BRFSS-PROJECT
> NY2019drop_var_respondent<-read.csv("NY2019drop_var_respondent.csv",header = TRUE)
> NY2019_lr<- NY2019drop_var_respondent%>%select(X_STATE,WEIGHT2,HEIGHT3,X_SEX, X_AGE80,MARITAL,X_HISPANC,X_PRACE1,EDUCA,INCOME2,MEDCOST,HLTHPLN1,EMPLOY1,PERSDOC2)
> attach(NY2019_lr)
> usevar<- (X_AGE80>=18) & (X_AGE80<=55)
> NY2019<-subset(NY2019_lr,usevar)
> detach()
> attach(NY2019)
> model1<-lm(HLTHPLN1~X_AGE80+ I(X_AGE80^2) + I(INCOME2^2)+X_SEX+MARITAL+X_PRACE1+X_HISPANC +EDUCA+INCOME2 +EMPLOY1, data = NY2019)
> summary(model1)


Call:
lm(formula = HLTHPLN1 ~ X_AGE80 + I(X_AGE80^2) + I(INCOME2^2) + 
    X_SEX + MARITAL + X_PRACE1 + X_HISPANC + EDUCA + INCOME2 + 
    EMPLOY1, data = NY2019)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.6387 -0.1746 -0.1091 -0.0262  8.0983 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.043e+00  1.015e-01  10.272  < 2e-16 ***
X_AGE80       1.739e-02  5.011e-03   3.470 0.000523 ***
I(X_AGE80^2) -2.382e-04  6.502e-05  -3.663 0.000251 ***
I(INCOME2^2)  4.289e-06  1.639e-05   0.262 0.793591    
X_SEX        -4.437e-02  1.328e-02  -3.341 0.000840 ***
MARITAL       2.309e-02  3.495e-03   6.607 4.26e-11 ***
X_PRACE1      2.319e-03  3.608e-04   6.426 1.40e-10 ***
X_HISPANC     7.275e-03  6.984e-03   1.042 0.297652    
EDUCA        -4.922e-02  5.989e-03  -8.219 2.48e-16 ***
INCOME2       3.421e-04  1.598e-03   0.214 0.830488    
EMPLOY1       5.162e-03  2.909e-03   1.775 0.076011 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5208 on 6248 degrees of freedom
  (171 observations deleted due to missingness)
Multiple R-squared:  0.03915,	Adjusted R-squared:  0.03761 
F-statistic: 25.46 on 10 and 6248 DF,  p-value: < 2.2e-16

> 
> NY2019$HCOV<- ifelse(NY2019$HLTHPLN1==1,1,0)
> NY2019$female<-if_else(NY2019$X_SEX==2,1,0)
> NY2019$Black<-if_else(NY2019$X_PRACE1==2,1,0)
> NY2019$White<-if_else(NY2019$X_PRACE1==1,1,0)
> NY2019$Asian<-if_else(NY2019$X_PRACE1==4,1,0)
> NY2019$HISPANIC<-if_else(NY2019$X_HISPANC==1,1,0)
> NY2019$NOHS<-if_else(NY2019$EDUCA<4,1,0)
> NY2019$HS<-if_else(NY2019$EDUCA==4,1,0)
> NY2019$Somecol<-if_else(NY2019$EDUCA==5,1,0)
> NY2019$BMdegr<-if_else(NY2019$EDUCA==6,1,0)
> NY2019$Student<-if_else(NY2019$EMPLOY1==6,1,0)
> NY2019$Sefemploy<-if_else(NY2019$EMPLOY1==2,1,0)
> NY2019$Hired<-if_else(NY2019$EMPLOY1==1,1,0)
> NY2019$Married<-if_else(NY2019$MARITAL==1,1,0)
> NY2019$Divorced<-if_else(NY2019$MARITAL==2,1,0)
> NY2019$Single<-if_else(NY2019$MARITAL==5,1,0)
> NY2019$Lessthan10000<-if_else(NY2019$INCOME2==1,1,0)
> NY2019$Lessthan25000<-if_else(NY2019$INCOME2==4,1,0)
> NY2019$Lessthan35000<-if_else(NY2019$INCOME2==5,1,0)
> NY2019$Lessthan50000<-if_else(NY2019$INCOME2==6,1,0)
> NY2019$Morethan75000<-if_else(NY2019$INCOME2==8,1,0)
> 
> dataforanalysis<-data.frame(NY2019$HCOV,
+                             NY2019$X_AGE80,
+                             NY2019$female,
+                             NY2019$Black,
+                             NY2019$White,
+                             NY2019$Asian,
+                             NY2019$HISPANIC,
+                             NY2019$Married,
+                             NY2019$Divorced,
+                             NY2019$Single,
+                             NY2019$NOHS,
+                             NY2019$HS,
+                             NY2019$Somecol,
+                             NY2019$BMdegr,
+                             NY2019$Student,
+                             NY2019$Sefemploy,
+                             NY2019$Hired,
+                             NY2019$Lessthan10000,
+                             NY2019$Lessthan25000,
+                             NY2019$Lessthan35000,
+                             NY2019$Lessthan50000,
+                             NY2019$Morethan75000)
> 
> names(dataforanalysis)<- c("HCOV",
+                            "X_AGE80",
+                            "female",
+                            "Black",
+                            "White",
+                            "Asian",
+                            "HISPANIC",
+                            "Married",
+                            "Divorced",
+                            "Single",
+                            "NOHS",
+                            "HS",
+                            "Somecol",
+                            "BMdegr",
+                            "Student",
+                            "Sefemploy",
+                            "Hired",
+                            "Lessthan10000",
+                            "Lessthan25000",
+                            "Lessthan35000",
+                            "Lessthan50000",
+                            "Morethan75000"
+                            )
> detach()
> attach(dataforanalysis)
The following object is masked _by_ .GlobalEnv:

    HCOV

The following objects are masked from dataforanalysis (pos = 3):

    Asian, Black, BMdegr, Divorced, female, HCOV, Hired, HISPANIC, HS, Lessthan10000,
    Lessthan35000, Lessthan50000, Married, Morethan75000, NOHS, Sefemploy, Single,
    Somecol, Student, White, X_AGE80

> require("standardize")
> set.seed(12345)
> NN<-length(dataforanalysis$HCOV)
> restrict_1<-(runif(NN)<0.7)
> summary(restrict_1)
   Mode   FALSE    TRUE 
logical    1931    4499 
> dat_train<-subset(dataforanalysis,restrict_1)
> dat_test<-subset(dataforanalysis,!restrict_1)
> sobj<-standardize(HCOV~X_AGE80+I(X_AGE80^2)+female+Black+I(female*Black)+White+Asian+HISPANIC+I(White*Lessthan25000)+I(Black*Lessthan25000) +Married+Divorced+Single+NOHS+HS + Somecol+ BMdegr+ Student+Sefemploy+Hired+Lessthan10000+Lessthan25000+Lessthan35000+Lessthan50000+Morethan75000, dat_train, family = binomial)
> s_data_test<- predict(sobj,dat_test)
> 
> 
> #LPM
> model_lpm1<-lm(sobj$formula,  data = sobj$data)
> summary(model_lpm1)

Call:
lm(formula = sobj$formula, data = sobj$data)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.06227  0.00950  0.07169  0.13953  0.57244 

Coefficients:
                          Estimate Std. Error t value Pr(>|t|)    
(Intercept)               0.808836   0.065931  12.268  < 2e-16 ***
X_AGE80                  -0.083988   0.041762  -2.011 0.044376 *  
I_X_AGE80.pow.2           0.105703   0.040464   2.612 0.009024 ** 
female1                   0.016598   0.005021   3.306 0.000955 ***
Black1                    0.005684   0.012236   0.465 0.642260    
I_female.t.Black1         0.014500   0.013695   1.059 0.289756    
White1                    0.034174   0.007675   4.453 8.69e-06 ***
Asian1                    0.002674   0.011658   0.229 0.818623    
HISPANIC1                -0.038785   0.007191  -5.393 7.29e-08 ***
I_White.t.Lessthan250001  0.042990   0.020932   2.054 0.040058 *  
I_Black.t.Lessthan250001  0.017374   0.027205   0.639 0.523087    
Married1                  0.030391   0.007240   4.198 2.75e-05 ***
Divorced1                 0.030322   0.009742   3.113 0.001867 ** 
Single1                   0.029330   0.007606   3.856 0.000117 ***
NOHS1                    -0.091293   0.027937  -3.268 0.001092 ** 
HS1                      -0.019548   0.027108  -0.721 0.470866    
Somecol1                 -0.005148   0.027113  -0.190 0.849409    
BMdegr1                   0.007476   0.027036   0.277 0.782156    
Student1                  0.030925   0.011538   2.680 0.007385 ** 
Sefemploy1               -0.025459   0.008718  -2.920 0.003516 ** 
Hired1                    0.016368   0.006244   2.621 0.008788 ** 
Lessthan100001           -0.015295   0.012207  -1.253 0.210296    
Lessthan250001           -0.041028   0.017052  -2.406 0.016169 *  
Lessthan350001           -0.006051   0.009147  -0.662 0.508306    
Lessthan500001           -0.020925   0.008835  -2.368 0.017904 *  
Morethan750001            0.012846   0.005984   2.147 0.031887 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.303 on 4354 degrees of freedom
  (119 observations deleted due to missingness)
Multiple R-squared:  0.1143,	Adjusted R-squared:  0.1092 
F-statistic: 22.48 on 25 and 4354 DF,  p-value: < 2.2e-16

> predi_val_lpm<-predict(model_lpm1, s_data_test)
contrasts dropped from factor femalecontrasts dropped from factor Blackcontrasts dropped from factor I_female.t.Blackcontrasts dropped from factor Whitecontrasts dropped from factor Asiancontrasts dropped from factor HISPANICcontrasts dropped from factor I_White.t.Lessthan25000contrasts dropped from factor I_Black.t.Lessthan25000contrasts dropped from factor Marriedcontrasts dropped from factor Divorcedcontrasts dropped from factor Singlecontrasts dropped from factor NOHScontrasts dropped from factor HScontrasts dropped from factor Somecolcontrasts dropped from factor BMdegrcontrasts dropped from factor Studentcontrasts dropped from factor Sefemploycontrasts dropped from factor Hiredcontrasts dropped from factor Lessthan10000contrasts dropped from factor Lessthan25000contrasts dropped from factor Lessthan35000contrasts dropped from factor Lessthan50000contrasts dropped from factor Morethan75000
> predi_model_lpm1<-(predi_val_lpm>0.6)
> table(pred = predi_model_lpm1, true = dat_test$HCOV)
       true
pred       0    1
  FALSE   37   28
  TRUE   205 1609
> #GPM
> model_logit1<-glm(sobj$formula,family= binomial,data = sobj$data)
> summary(model_logit1)

Call:
glm(formula = sobj$formula, family = binomial, data = sobj$data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.0016   0.2400   0.3538   0.5034   1.5527  

Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)               1.78169    0.61935   2.877 0.004019 ** 
X_AGE80                  -1.07833    0.44861  -2.404 0.016230 *  
I_X_AGE80.pow.2           1.33475    0.44495   3.000 0.002702 ** 
female1                   0.20949    0.05713   3.667 0.000246 ***
Black1                    0.00315    0.11083   0.028 0.977328    
I_female.t.Black1         0.09245    0.13717   0.674 0.500349    
White1                    0.30434    0.07243   4.202 2.65e-05 ***
Asian1                   -0.08109    0.11313  -0.717 0.473534    
HISPANIC1                -0.29755    0.06676  -4.457 8.31e-06 ***
I_White.t.Lessthan250001  0.18912    0.18702   1.011 0.311925    
I_Black.t.Lessthan250001  0.04116    0.22948   0.179 0.857661    
Married1                  0.29174    0.07114   4.101 4.12e-05 ***
Divorced1                 0.26535    0.10237   2.592 0.009542 ** 
Single1                   0.24552    0.07243   3.390 0.000700 ***
NOHS1                    -0.53959    0.26041  -2.072 0.038259 *  
HS1                      -0.17744    0.25733  -0.690 0.490477    
Somecol1                 -0.03714    0.25894  -0.143 0.885942    
BMdegr1                   0.14632    0.25910   0.565 0.572260    
Student1                  0.34529    0.12812   2.695 0.007040 ** 
Sefemploy1               -0.18809    0.08187  -2.298 0.021589 *  
Hired1                    0.20889    0.06478   3.225 0.001261 ** 
Lessthan100001           -0.11484    0.10536  -1.090 0.275729    
Lessthan250001           -0.20775    0.13151  -1.580 0.114168    
Lessthan350001           -0.07850    0.08973  -0.875 0.381607    
Lessthan500001           -0.21598    0.08546  -2.527 0.011499 *  
Morethan750001            0.24030    0.07862   3.057 0.002238 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 3155.6  on 4379  degrees of freedom
Residual deviance: 2703.0  on 4354  degrees of freedom
  (119 observations deleted due to missingness)
AIC: 2755

Number of Fisher Scoring iterations: 6

> predi_val_glm<-predict(model_logit1, s_data_test, type = "response")
contrasts dropped from factor femalecontrasts dropped from factor Blackcontrasts dropped from factor I_female.t.Blackcontrasts dropped from factor Whitecontrasts dropped from factor Asiancontrasts dropped from factor HISPANICcontrasts dropped from factor I_White.t.Lessthan25000contrasts dropped from factor I_Black.t.Lessthan25000contrasts dropped from factor Marriedcontrasts dropped from factor Divorcedcontrasts dropped from factor Singlecontrasts dropped from factor NOHScontrasts dropped from factor HScontrasts dropped from factor Somecolcontrasts dropped from factor BMdegrcontrasts dropped from factor Studentcontrasts dropped from factor Sefemploycontrasts dropped from factor Hiredcontrasts dropped from factor Lessthan10000contrasts dropped from factor Lessthan25000contrasts dropped from factor Lessthan35000contrasts dropped from factor Lessthan50000contrasts dropped from factor Morethan75000
> predi_model_logit1<-(predi_val_glm>0.6)
> table(pred = predi_model_logit1, true = dat_test$HCOV)
       true
pred       0    1
  FALSE   48   42
  TRUE   194 1595
> require("randomForest")
Loading required package: randomForest
randomForest 4.6-14
Type rfNews() to see new features/changes/bug fixes.

Attaching package: �randomForest�

The following object is masked from �package:dplyr�:

    combine

The following object is masked from �package:ggplot2�:

    margin

> set.seed(54321)
> model_randFor<- randomForest(as.factor(HCOV)~ ., data= sobj$data, importance = TRUE, proximity= TRUE)
require("randomForest")
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)
# lets look the confusion Matrix
pred_model1<-predict(model_randFor, s_data_test)
table(pred = pred_model1, true = dat_test$HCOV)
