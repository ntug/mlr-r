# mlr-r

The `sat` data frame has 50 rows and 7 columns. Data were collected to study the relationship between expenditures on public education and test results.

This data frame contains the following columns:
* `expend`: Current expenditure per pupil in average daily attendance in public elementary and secondary
schools, 1994-95 (in thousands of dollars)
* `ratio`: Average pupil/teacher ratio in public elementary and secondary schools, Fall 1994
* `salary`: Estimated average annual salary of teachers in public elementary and secondary schools,
1994-95 (in thousands of dollars)
* `takers`: Percentage of all eligible students taking the SAT, 1994-95
* `verbal`: Average verbal SAT score, 1994-95
* `math`: Average math SAT score, 1994-95
* `total`: Average total score on the SAT, 1994-95

### (a)

```{r, fig.height=3}
library(knitr)
require(faraway)
data("sat")
simp.1 <-lm(total ~ expend, sat)
summary(simp.1)
plot(total ~ expend, sat, pch=19, cex=.6)
abline(simp.1, col="red")
```

---

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-2-1.png)

This model tells us that the predicted `total` will decrease by 20.9 for every one thousand dollar increase in `expend`. We obtain a small R-squared of 0.1448 which means that this model doesn't explain most of the variation in the SAT scores around the mean. This is also supported by looking at the graph - the model doesn't seem to fit the data.

---

```{r, fig.height=3}
simp.2 <-lm(total ~ salary, sat)
summary(simp.2)
plot(total ~ salary, sat, pch=19, cex=.6)
abline(simp.2, col="red")
```

---

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-3-1.png)

This model tells us that the predicted `total` will decrease by 5.5 for every one thousand dollar increase in `salary`. An R-squared of 0.1935 is small so the model doesn't explain most of the variation in the SAT scores around the mean. This is also supported by looking at the graph - the model doesn't seem to fit the data.

---

```{r, fig.height=3}
simp.3 <-lm(total ~ takers, sat)
summary(simp.3)
plot(total ~ takers, sat, pch=19, cex=.6)
abline(simp.3, col="red")
```

---

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-4-1.png)

This model tells us that the predicted `total` will decrease by 2.5 for every one percent increase in `takers`. An R-squared of 0.7825 isn't small so the model could explain some of the variation in the SAT scores around the mean. This is also supported by looking at the graph - the model does seem to fit the data.

---

```{r, fig.height=3}
simp.4 <-lm(total ~ ratio, sat)
summary(simp.4)
plot(total ~ ratio, sat, pch=19, cex=.6)
abline(simp.4, col="red")
```

---

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-5-1.png)

This model tells us that the predicted `total` will increase by 2.7 for every one unit increase in `ratio`. An R-squared of 0.006602 is very small so the model doesn't explain any of the variation in the SAT scores around the mean. This is also supported by looking at the graph - the variables don't seem to be correlated whatsoever.

### (b)

```{r}
mult.1 <- lm(total ~ expend+takers, sat)
summary(mult.1)
```

---

When we regress `total` on `expend` and `takers`, the model tells us that the predicted average statewide total SAT score will increase by 12.3 for every one thousand dollar increase in average expenditure per pupil and decrease by 2.9 for every one percentage increase in `takers` (holding covariates constant).  

When we compare the coefficients to part(a) in which we performed simple linear regressions, the estimated regression coefficent as wells as the standard error for `expend` has changed significantly.

---

```{r}
mult.2 <- lm(total ~ salary+takers, sat)
summary(mult.2)
```

---

We see the same effect now when we're regressing on `salary` and `takers`, the `salary` coefficient and standard error has changed significantly.

---

```{r}
mult.3 <- lm(total ~ ratio+takers, sat)
summary(mult.3)
```

---

Once again, the `ratio` slope estimate and standard error has changed significantly from its the SLR model.  

*Through these three multiple regression models, we see that the fraction of eligible test takers has an impact on expenditure per pupil, average anuual teacher salary, and pupil/teacher ratio. In these two-variable models, we see a decrease in standard error which leads to a narrower confidence interval, and hence a more precise slope parameter estimate. This implies that the fraction of eligible students in each state accounts for most of the variation in statewide SAT scores.*

---

```{r}
res.take <- residuals(simp.3)
resexp <- residuals(lm(expend ~ takers, sat))
ressal <- residuals(lm(salary ~ takers, sat))
resrat <- residuals(lm(ratio ~ takers, sat))
c(cor(res.take,resexp), cor(res.take,ressal), cor(res.take,resrat))
```

---

Above are the partial correlations between `total`/`expend`, `total`/`salary`, and `total`/`ratio` (respectively) - all controlling for the fraction of eligible students taking the exam. With the following displayed correlation matrix, we see that when we control `takers`, the correlation changes in strength and direction i.e. a positive correlation between `expend`/`total` and `salary`/`total` and a negative correlation between `ratio`/`total`. This supports our above statement.

### (c)

```{r, fig.height=4}
sat1 <- sat[c(-5,-6)]
round(cor(sat1),3)
mult.4 <- lm(total ~ salary+ratio+takers, sat)
summary(mult.4)
```

---

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-11-1.png)

Upon looking at the correlation matrix, we see that `ratio` and `salary` are strongly uncorrelated. This shows that the addition of the `ratio` or `salary` explanatory variable to the two-variable model (`total~salary+takers`, `total~ratio+takers`, respectively) did not impact the model since the slope estimates and standard errors were very similar. This implies that our three-variable MLR did not lead to any changes in our confidence interval and hence did not improve the precision in our slope parameter estimates.

---

```{r}
pairs(sat1, pch=15, cex=0.5, panel=panel.smooth)
```

---

Through the `pairs` matrix, we note the strong correlation between `expend`/`salary`, `expend`/`takers`, `salary`/`takers`, `takers`/`total`. Observing this matrix, as well as the correlation matrix, we see that there is some correlation between all pairs except for `ratio`/`salary` and `ratio`/`total`. These explain the significant changes in estimates from the SLR to the two-variable MLR (with `takers`), and the lack of significant change in estimates from the two-variable MLRs to the three-variable MLR.

---

```{r}
coplot(total ~ expend|takers, sat, panel=function(x, y, ...){
  panel.smooth(x, y, span=0.8, iter=5, col.smooth ="blue", ...)
  abline(lm(y~x), col="red")
},
rows=1, pch=19, cex=0.3)
coplot(total ~ salary|takers, sat, panel=function(x, y, ...){
  panel.smooth(x, y, span=0.8, iter=5, col.smooth ="blue", ...)
  abline(lm(y~x), col="red")
},
rows=1, pch=19, cex=0.3)
coplot(total ~ ratio|takers, sat, panel=function(x, y, ...){
  panel.smooth(x, y, span=0.8, iter=5, col.smooth ="blue", ...)
  abline(lm(y~x), col="red")
},
rows=1, pch=19, cex=0.3)
```

---

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-12-1.png)

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-12-2.png)

![alt text](https://github.com/ntug/mlr-r/blob/master/README_figs/README-unnamed-chunk-12-3.png)

Upon constructing these conditioning plots of `expend`, `salary`, and `ratio` given an eligible fraction, we now see a clearer relationship between these predictor variables and `total`. When we initially looked at our SLR, it seemed as though an increase in expenditure per pupil and average annual teacher salary correlated to a decrease in the statewide average SAT total scores (with pupil/teacher ratio having almost no correlation to it). However, the partial correlations and the above `coplot` show an opposite relationship when we take the percentage of eligible test takers in each state into account. These changes could be due to the fact that academic participation is not as relevant in some parts of the country as others.
