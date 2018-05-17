
install.packages("survminer")

library(survival);
library(survminer);

data("lung")

lung

res_cox = coxph(Surv(time, status) ~ sex + age, data = lung)
summary(res_cox)

new_df = with(lung,
              data.frame(sex = c(1, 2), 
                         age = rep(mean(age, na.rm = TRUE), 2),
                         wt.loss = rep(mean(wt.loss, na.rm = TRUE), 2)
                        )
             )
new_df

ggsurvplot(survfit(res_cox, newdata = new_df), data = new_df, conf.int = TRUE, censor = TRUE, surv.median.line = "hv")

install.packages("VGAM")

library(VGAM);

data("mtcars")

tobit = VGAM::vglm(mpg ~ cyl +disp + hp + gear, VGAM::tobit(Upper = 34), data = mtcars)
VGAM::summary(tobit)

install.packages("sampleSelection")

library(sampleSelection);

data("Mroz87")

heckman = heckit(lfp ~ age + I(age^2) + kids5 + huswage + educ, log(wage) ~ educ + exper + I(exper^2) + city, data = Mroz87)
summary(heckman)
