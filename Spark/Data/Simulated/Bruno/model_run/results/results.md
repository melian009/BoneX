results



**################################################################################**

**# 1. General Effects of mutualistic effect** 

**################################################################################**



**## 1.1 mutualistic effects on species persistence**



&#x20;> anova(mod, null, test = "Chisq")

Data: resultados

Models:

null: cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ 1 + , zi=\~0, disp=\~1

null:     (1 | nome\_rede), zi=\~0, disp=\~1

mod: cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ param\_nivel\_BCe + , zi=\~0, disp=\~1

mod:     (1 | nome\_rede), zi=\~0, disp=\~1

&#x20;    Df      AIC      BIC   logLik deviance   Chisq Chi Df Pr(>Chisq)    

null  2 16131947 16131970 -8065971 16131943                              

mod   4  7989800  7989847 -3994896  7989792 8142151      2  < 2.2e-16 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> summary(mod)

&#x20;Family: binomial  ( logit )

Formula:          cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ param\_nivel\_BCe +      (1 | nome\_rede)

Data: resultados



&#x20;     AIC       BIC    logLik -2\*log(L)  df.resid 

&#x20; 7989800   7989847  -3994896   7989792   1029596 



Random effects:



Conditional model:

&#x20;Groups    Name        Variance Std.Dev.

&#x20;nome\_rede (Intercept) 0.3608   0.6006  

Number of obs: 1029600, groups:  nome\_rede, 286



Conditional model:

&#x20;                        Estimate Std. Error z value Pr(>|z|)    

(Intercept)              3.881449   0.035549   109.2   <2e-16 \*\*\*

param\_nivel\_BCelow      -2.956083   0.001510 -1958.2   <2e-16 \*\*\*

param\_nivel\_BCemoderate -1.223450   0.001634  -748.8   <2e-16 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1







**################################################################################**

**#1.2 Mutualism effect on time to convergence**



> anova(mod\_ttc, null, test = "Chisq")

Data: resultados

Models:

null: time\_to\_convergence \~ 1 + (1 | nome\_rede), zi=\~0, disp=\~1

mod\_ttc: time\_to\_convergence \~ param\_nivel\_BCe + (1 | nome\_rede), zi=\~0, disp=\~1

&#x20;       Df     AIC     BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)    

null     3 2900975 2901010 -1450484  2900969                             

mod\_ttc  5 2723365 2723424 -1361678  2723355 177613      2  < 2.2e-16 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> summary(mod\_ttc)

&#x20;Family: nbinom2  ( log )

Formula:          time\_to\_convergence \~ param\_nivel\_BCe + (1 | nome\_rede)

Data: resultados



&#x20;     AIC       BIC    logLik -2\*log(L)  df.resid 

&#x20; 2723365   2723424  -1361678   2723355   1029595 



Random effects:



Conditional model:

&#x20;Groups    Name        Variance Std.Dev.

&#x20;nome\_rede (Intercept) 0.02028  0.1424  

Number of obs: 1029600, groups:  nome\_rede, 286



Dispersion parameter for nbinom2 family (): 9.92e+07 



Conditional model:

&#x20;                       Estimate Std. Error z value Pr(>|z|)    

(Intercept)             0.203939   0.008560    23.8   <2e-16 \*\*\*

param\_nivel\_BCelow      0.746167   0.001862   400.7   <2e-16 \*\*\*

param\_nivel\_BCemoderate 0.323534   0.002014   160.7   <2e-16 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1


**################################################################################**

**# 2. General Effects phisiological costs** 

**################################################################################**



**# 2.1 Phisiological costs over moderate mutualism**



> anova(cp, null, test = "Chisq")

Data: phisio

Models:

null: cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ 1 + , zi=\~0, disp=\~1

null:     (1 | nome\_rede), zi=\~0, disp=\~1

cp: cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ param\_Cp\_multiplier + , zi=\~0, disp=\~1

cp:     (1 | nome\_rede), zi=\~0, disp=\~1

&#x20;    Df     AIC     BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)    

null  2 2226986 2227008 -1113491  2226982                             

cp    5 1657559 1657613  -828775  1657549 569433      3  < 2.2e-16 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> summary(cp)

&#x20;Family: binomial  ( logit )

Formula:          cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ param\_Cp\_multiplier +      (1 | nome\_rede)

Data: phisio



&#x20;     AIC       BIC    logLik -2\*log(L)  df.resid 

1657559.4 1657613.2 -828774.7 1657549.4    343195 



Random effects:



Conditional model:

&#x20;Groups    Name        Variance Std.Dev.

&#x20;nome\_rede (Intercept) 0.6265   0.7915  

Number of obs: 343200, groups:  nome\_rede, 286



Conditional model:

&#x20;                       Estimate Std. Error z value Pr(>|z|)    

(Intercept)             3.720534   0.046892    79.3   <2e-16 \*\*\*

param\_Cp\_multiplier0.1 -0.282337   0.003219   -87.7   <2e-16 \*\*\*

param\_Cp\_multiplier0.3 -0.962342   0.002917  -329.9   <2e-16 \*\*\*

param\_Cp\_multiplier0.5 -1.650360   0.002750  -600.2   <2e-16 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1




**################################################################################**

**# 2. Persistence of Species position at network** 

**################################################################################**



\###CORE



mean = 51.91

sd = 105.70

cv = 2.03



\###PERIPHERY

mean = 19.32

sd = 48.67

cv = 2.51



**#3.2.a Position x persistence core species**


anova(position, null, test = "Chisq")

Data: core\_peri

Models:

null: cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ 1 + , zi=\~0, disp=\~1

null:     (1 | nome\_rede), zi=\~0, disp=\~1

position: cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ service\_providers + , zi=\~0, disp=\~1

position:     (1 | nome\_rede), zi=\~0, disp=\~1

&#x20;        Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    

null      2 1486877 1486898 -743437  1486873                             

position  3 1486859 1486890 -743427  1486853 20.031      1  7.619e-06 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> summary(position)

&#x20;Family: binomial  ( logit )

Formula:          cbind(n\_species\_final, n\_species\_total - n\_species\_final) \~ service\_providers +      (1 | nome\_rede)

Data: core\_peri



&#x20;     AIC       BIC    logLik -2\*log(L)  df.resid 

1486859.1 1486890.1 -743426.6 1486853.1    228797 



Random effects:



Conditional model:

&#x20;Groups    Name        Variance Std.Dev.

&#x20;nome\_rede (Intercept) 0.5814   0.7625  

Number of obs: 228800, groups:  nome\_rede, 286



Conditional model:

&#x20;                           Estimate Std. Error z value Pr(>|z|)    

(Intercept)                 2.822256   0.045149   62.51  < 2e-16 \*\*\*

service\_providersperiphery -0.009170   0.002049   -4.48 7.62e-06 \*\*\*

\---

Signif. codes:  0 ‘\*\*\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

