rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(foreign)
library(sandwich)
library(stargazer)
library(AER)
#install.packages('gmm')
library(gmm)


data.ingresos <- read_csv("data/ingresos_iv.csv",
                          locale = locale(encoding = "latin1"))


#MCO
mco <- lm(lwage ~ educ + exper + black + south + married + smsa,
          data = data.ingresos)

stargazer(mco,
          type = 'text')


#Variables instrumentales (asume homocedasticidad)
vi <- ivreg(lwage ~  educ + exper + black + south + married + smsa |
               . - educ + nearc4, data = data.ingresos)

stargazer(mco, vi,
          type="text",
          keep=c("educ"))

# Primera etapa
pe_vi <- lm(educ ~  nearc4 + exper + black + south + married + smsa,
            data = data.ingresos)

stargazer(mco, vi, pe_vi,
          type="text",
          keep=c("educ", "nearc"))

#F de los instrumentos
linearHypothesis(pe_vi, c("nearc4=0"))



#GMM

#IV es un caso especial de GMM
gmm_iv <- gmm(lwage ~ educ + exper + black + south + married + smsa, 
                   ~ nearc4 + exper + black + south + married + smsa,
                   vcov = "iid",
                   wmatrix = "optimal", # es igual si usamos optimal
                   data = data.ingresos)


stargazer(vi, gmm_iv,
          type="text",
          keep = c("educ"),
          digits = 4)


#Con GMM podemos lidiar con heterocedasticidad
gmm_iv_het <- gmm(lwage ~ educ + exper + black + south + married + smsa, 
                  ~ nearc4 + exper + black + south + married + smsa,
                  vcov = "HAC",
                  wmatrix = "optimal",
                  type = "twoStep",
                  data = data.ingresos)

stargazer(vi, gmm_iv, gmm_iv_het,
          type="text",
          keep = c("educ"),
          digits = 4)




#Y podemos incluir más de un instrumento
gmm_opt <- gmm(lwage ~ educ + exper + black + south + married + smsa, 
                  ~ nearc4 + nearc2 + exper + black + south + married + smsa,
                  vcov = "HAC",
                  wmatrix = "optimal",
                  type = "twoStep",
                  data = data.ingresos)

stargazer(vi, gmm_iv, gmm_iv_het, gmm_opt,
          type="text",
          keep = c("educ"),
          digits = 4)


#Los tests pueden accederse con la opción diagnostics

vi.ei <- ivreg(lwage ~  educ + exper + black + south + married + smsa |
              . - educ + nearc4, data = data.ingresos)

vi.si <- ivreg(lwage ~  educ + exper + black + south + married + smsa |
                 . - educ + nearc4 + nearc2, data = data.ingresos)

summary(vi.ei, diagnostics=T, vcov = sandwich)

summary(vi.si, diagnostics=T, vcov = sandwich)
