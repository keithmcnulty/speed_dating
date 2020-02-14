It’s Valentines Day - a day when people think about love and
relationships. How people meet and form relationship works a lot quicker
than in our parent’s or grandparent’s day. I’m sure many of you are told
how it used to be - you met someone, dated them for a while, proposed,
got married. People who grew up in small towns maybe had one shot at
finding love, so they made sure they didn’t mess it up.

Today finding a date is not a challenge - finding a match is probably
the issue. In the last 20 years we’ve gone from traditional dating to
online dating to speed dating to online speed dating. Now you just swipe
left or swipe right, if that’s your thing.

In 2002-2004, Columbia University ran a speed-dating experiment where
they tracked data over 21 speed dating sessions for mostly young adults
meeting people of the opposite sex. I found the dataset and the key to
the data here:
<a href="http://www.stat.columbia.edu/~gelman/arm/examples/speed.dating/" class="uri">http://www.stat.columbia.edu/~gelman/arm/examples/speed.dating/</a>.

I was interested in finding out what it was about someone during that
short interaction that determined whether or not someone viewed them as
a match. This is a great opportunity to practice simple logistic
regression if you’ve never done it before.

The speed dating dataset
------------------------

The dataset at the link above is quite substantial - over 8,000
observations with almost 200 datapoints for each. However, I was only
interested in the speed dates themselves, and so I simplified the data
and uploaded a smaller version of the dataset to my Github account
[here](https://github.com/keithmcnulty/speed_dating). I’m going to pull
this dataset down and do some simple regression analysis on it to
determine what it is about someone that influences whether someone sees
them as a match.

Let’s pull the data and take a quick look:

    library(tidyverse)
    library(corrplot)

    download.file("https://raw.githubusercontent.com/keithmcnulty/speed_dating/master/speed_data_data.RDS", "speed_dating_data.RDS")

    data <- readRDS("speed_dating_data.RDS")

    head(data, 3) %>% 
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: right;">gender</th>
<th style="text-align: right;">age</th>
<th style="text-align: right;">income</th>
<th style="text-align: right;">goal</th>
<th style="text-align: left;">career</th>
<th style="text-align: right;">dec</th>
<th style="text-align: right;">attr</th>
<th style="text-align: right;">sinc</th>
<th style="text-align: right;">intel</th>
<th style="text-align: right;">fun</th>
<th style="text-align: right;">amb</th>
<th style="text-align: right;">shar</th>
<th style="text-align: right;">like</th>
<th style="text-align: right;">prob</th>
<th style="text-align: right;">met</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">0</td>
<td style="text-align: right;">21</td>
<td style="text-align: right;">69487</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">lawyer</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">2</td>
</tr>
<tr class="even">
<td style="text-align: right;">0</td>
<td style="text-align: right;">21</td>
<td style="text-align: right;">69487</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">lawyer</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">6</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="odd">
<td style="text-align: right;">0</td>
<td style="text-align: right;">21</td>
<td style="text-align: right;">69487</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">lawyer</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">9</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">5</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">7</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">1</td>
</tr>
</tbody>
</table>

We can work out from the key that:

1.  The first five columns are demographic - we may want to use them to
    look at subgroups later.
2.  The next seven columns are important. `dec` is the raters decision
    on whether this individual was a match and then follows scores out
    of ten on six characteristics: attractiveness, sincerity,
    intelligence, fun, ambitiousness and shared interests.  
3.  The `like` column is an overall rating. The `prob` column is a
    rating on whether the rater believed that interest would be
    reciprocated and the final column is a binary on whether the two had
    met prior to the speed date, with the lower value indicating that
    they had met before.

We can leave the first four columns out of any analysis we do. Our
outcome variable here is `dec`. I’m interested in the rest as potential
explanatory variables. Before I start to do any analysis, I want to
check if any of these variables are highly colinear - ie, have very high
correlations. If two variables are measuring pretty much the same thing,
I should probably remove one of them.

    corr_matrix <- data %>% 
      dplyr::select(attr, sinc, intel, fun, amb, shar, like, prob, met) %>% 
      as.matrix() 

    M <- cor(corr_matrix, use = "complete.obs")

    corrplot::corrplot(M)

![](speed_dating_files/figure-markdown_strict/corr%20check-1.png)

OK, clearly there’s mini-halo effects running wild when you speed date.
But none of these get up really high (eg past 0.75), so I’m going to
leave them all in because this is just for fun. I might want to spend a
bit more time on this issue if my analysis had serious consequences
here.

Running a logistic regression on the data
-----------------------------------------

The outcome of this process is binary. The respondent decides yes or no.
That’s harsh, I give you. But for a statistician it points straight to a
binomial logistic regression as our primary analytic tool. Let’s run a
logistic regression model on the outcome and potential explanatory
variables I’ve identified above, and take a look at the results.

    model <- glm(dec ~ attr + sinc + intel + fun + amb + shar + like + prob + met, 
                 data = data, 
                 family = "binomial")

    summary(model)

    ## 
    ## Call:
    ## glm(formula = dec ~ attr + sinc + intel + fun + amb + shar + 
    ##     like + prob + met, family = "binomial", data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8210  -0.7602  -0.2311   0.7921   3.6432  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -6.0769718  0.2101454 -28.918  < 2e-16 ***
    ## attr         0.4296537  0.0235843  18.218  < 2e-16 ***
    ## sinc        -0.2088995  0.0273358  -7.642 2.14e-14 ***
    ## intel       -0.0005595  0.0327799  -0.017    0.986    
    ## fun          0.1357729  0.0258310   5.256 1.47e-07 ***
    ## amb         -0.1904715  0.0251996  -7.559 4.08e-14 ***
    ## shar         0.1073134  0.0208044   5.158 2.49e-07 ***
    ## like         0.5607669  0.0325454  17.230  < 2e-16 ***
    ## prob         0.1503168  0.0178518   8.420  < 2e-16 ***
    ## met          0.0021065  0.0310848   0.068    0.946    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 9322.9  on 6818  degrees of freedom
    ## Residual deviance: 6460.5  on 6809  degrees of freedom
    ##   (1559 observations deleted due to missingness)
    ## AIC: 6480.5
    ## 
    ## Number of Fisher Scoring iterations: 5

So, perceived intelligence doesn’t really matter. This could be a factor
of the population being studied, whom I believe were all undergraduates
at Columbia and so would all have a high average SAT I suspect, and so
intelligence might be less of a differentiator. Neither does whether on
not you’d met someone before - not that surprising. Everything else
seems to play a significant role.

More interesting is how *much* of a role each factor plays. The
Coefficients Estimates in the model output above tell us the effect of
each variable, assuming other variables are held still. But in the form
above they are expressed in log odds, and we need to convert them to
regular odds ratios so we can understand them better, so let’s adjust
our results to do that.

    ctable <- coef(summary(model))
    odds_ratio <- exp(coef(summary(model))[ , c("Estimate")])
    (coef_summary <-  cbind(ctable, as.data.frame(odds_ratio, nrow = nrow(ctable), ncol = 1))) %>% 
      knitr::kable()

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">Std. Error</th>
<th style="text-align: right;">z value</th>
<th style="text-align: right;">Pr(&gt;|z|)</th>
<th style="text-align: right;">odds_ratio</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>(Intercept)</td>
<td style="text-align: right;">-6.0769718</td>
<td style="text-align: right;">0.2101454</td>
<td style="text-align: right;">-28.9179377</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.0022951</td>
</tr>
<tr class="even">
<td>attr</td>
<td style="text-align: right;">0.4296537</td>
<td style="text-align: right;">0.0235843</td>
<td style="text-align: right;">18.2178165</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">1.5367252</td>
</tr>
<tr class="odd">
<td>sinc</td>
<td style="text-align: right;">-0.2088995</td>
<td style="text-align: right;">0.0273358</td>
<td style="text-align: right;">-7.6419635</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.8114768</td>
</tr>
<tr class="even">
<td>intel</td>
<td style="text-align: right;">-0.0005595</td>
<td style="text-align: right;">0.0327799</td>
<td style="text-align: right;">-0.0170688</td>
<td style="text-align: right;">0.9863817</td>
<td style="text-align: right;">0.9994406</td>
</tr>
<tr class="odd">
<td>fun</td>
<td style="text-align: right;">0.1357729</td>
<td style="text-align: right;">0.0258310</td>
<td style="text-align: right;">5.2562008</td>
<td style="text-align: right;">0.0000001</td>
<td style="text-align: right;">1.1454217</td>
</tr>
<tr class="even">
<td>amb</td>
<td style="text-align: right;">-0.1904715</td>
<td style="text-align: right;">0.0251996</td>
<td style="text-align: right;">-7.5585158</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.8265693</td>
</tr>
<tr class="odd">
<td>shar</td>
<td style="text-align: right;">0.1073134</td>
<td style="text-align: right;">0.0208044</td>
<td style="text-align: right;">5.1582160</td>
<td style="text-align: right;">0.0000002</td>
<td style="text-align: right;">1.1132831</td>
</tr>
<tr class="even">
<td>like</td>
<td style="text-align: right;">0.5607669</td>
<td style="text-align: right;">0.0325454</td>
<td style="text-align: right;">17.2302765</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">1.7520156</td>
</tr>
<tr class="odd">
<td>prob</td>
<td style="text-align: right;">0.1503168</td>
<td style="text-align: right;">0.0178518</td>
<td style="text-align: right;">8.4202745</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">1.1622023</td>
</tr>
<tr class="even">
<td>met</td>
<td style="text-align: right;">0.0021065</td>
<td style="text-align: right;">0.0310848</td>
<td style="text-align: right;">0.0677678</td>
<td style="text-align: right;">0.9459705</td>
<td style="text-align: right;">1.0021088</td>
</tr>
</tbody>
</table>

So we have some interesting observations:

1.  Unsurprisingly, the respondents overall rating on someone is the
    biggest indicator of whether they decide to match with them.
2.  Attractiveness seems substantially the primary positive indicator of
    a match.
3.  Interestingly, sincerity and ambitiousness **decreased** the
    likelihood of a match - they were seemingly turn-offs for potential
    dates.
4.  Other factors played a minor positive role, including whether or not
    the respondent believed the interest to be reciprocated.

Comparing the genders
---------------------

It’s of course natural to ask whether there are gender differences in
these dynamics. So I’m going to rerun the analysis on the two gender
subsets and then create a chart that illustrates any differences.

    # females only

    model_f <- glm(dec ~ attr + sinc + intel + fun + amb + shar + like + prob + met, 
                   data = data %>% 
                     dplyr::filter(gender == 0), 
                   family = "binomial")

    ctable_f <- coef(summary(model_f))
    odds_ratio_f <- exp(coef(summary(model_f))[ , c("Estimate")])
    coef_summary_f <-  cbind(ctable_f, as.data.frame(odds_ratio_f, nrow = nrow(ctable_f), ncol = 1))


    model_m <- glm(dec ~ attr + sinc + intel + fun + amb + shar + like + prob + met, 
                   data = data %>% 
                     dplyr::filter(gender == 1), 
                   family = "binomial")

    ctable_m <- coef(summary(model_m))
    odds_ratio_m <- exp(coef(summary(model_m))[ , c("Estimate")])
    coef_summary_m <-  cbind(ctable_m, as.data.frame(odds_ratio_m, nrow = nrow(ctable_m), ncol = 1))

    chart_data <- coef_summary_f %>% 
      dplyr::add_rownames() %>% 
      dplyr::left_join(coef_summary_m %>% 
                         dplyr::add_rownames(), by = "rowname") %>% 
      dplyr::select(rowname, odds_ratio_f, odds_ratio_m) %>% 
      tidyr::pivot_longer(cols = c("odds_ratio_f", "odds_ratio_m"), names_to = "odds_ratio") %>% 
      dplyr::mutate(Effect = value - 1,
                    Gender = ifelse(odds_ratio == "odds_ratio_f", "Female", "Male"),
                    Factor = dplyr::recode(rowname, amb = "Ambitious", attr = "Attractive",
                                           fun = "Fun", intel = "Intelligent", like = "Liked",
                                           met = "Never met\nbefore", prob = "Believe\nthey like\nme",
                                           shar = "Shared\nInterests", sinc = "Sincere"))

    ggplot(data=chart_data %>% 
                  dplyr::filter(rowname != "(Intercept)"), 
                aes(x=Factor, y=Effect, fill=Gender)) +
      geom_bar(stat="identity", color="black", position=position_dodge()) +
      theme_minimal() +
      labs(x = "", title = "What matters in speed dating?") +
      scale_fill_manual(values=c('#FFC0CB', '#0000FF'))

![](speed_dating_files/figure-markdown_strict/gender%20analysis-1.png)

We find a couple of interesting differences. As per the long suggested
stereotype, intelligence does matter more to women. It has a significant
positive effect versus men where it doesn’t seem to play a meaningful
role. The other interesting difference is that whether you have met
someone before does have a significant effect on both groups, but we
didn’t see it before because it has the opposite effect for men and
women. Men seemingly prefer new interactions, versus women who like to
see a familiar face.
