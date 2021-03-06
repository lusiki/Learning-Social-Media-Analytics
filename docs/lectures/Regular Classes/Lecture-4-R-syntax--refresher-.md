---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 4: R syntax (refresher)"
author: "Luka Sikic, PhD <br> Faculty of Croatian Studies | [LSMA](https://lusiki.github.io/Learning-Social-Media-Analytics/)"
output:
  html_document:
    code_folding: show
    theme: flatly
    highlight: haddock
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: yes
  pdf_document:
    toc: yes
    toc_depth: '4'
---










# BEFORE WE BEGIN


It is important to type code yourself---**type at your keyboard and see what happens on your screen**---to get the feel of working in **R**. 

Many other similar introductions are exist around the web.
This particular version has similar coverage to the standard [*Introduction to* ***R*** manual](https://cran.r-project.org/doc/manuals/r-release/R-intro.html) and targets students who are neither programmers nor statisticians (yet).



# WHAT IS **R** ?


The original development of R was undertaken by Robert Gentleman and Ross Ihaka at the University of Auckland during the early 1990’s and it is arguably the most popular statistical programming language.1 The continuing development of this open source programming language has since been taken over by an international team of academics, computer programmers, statisticians and mathematicians. One of the reasons for the popularity of R is that it is free of cost, which implies that users can contribute towards a world that values data literacy, irrespective of personal means (to a certain extent). In addition, users are also able to implement what is almost certainly the most extensive range of statistical models, using existing R code.

One of the key features of this open-source language is that users can extend, enhance, and replace any part of it. In addition, such modifications may be made available to other users, in the form of various R packages. For example, there are several thousand packages on the Comprehensive R Archive Network (CRAN), which adhere to a coding standard and are subject to unit testing. In addition, there are also many more R packages that may be found on user repositories, which may be located on GitLab, GitHub, BitBucket, etc. Since one does not need to obtain a user licence to make use of this software, it is relatively painless to move a specific task onto the cloud, which provides users with impressive computing power and storage facilities that may be provided by vendors, such as Amazon Web Services (AWS), Google Cloud Platform (GCP), Microsoft Azure, DigitalOcean, etc.

The R language can be run on almost any operating system and it facilitates object-oriented programming, functional programming, and more. It can be used to work with a wide variety of different types of datasets and there are a number of interfaces that have been developed which allow for R to work with other programs, source code, databases and application programming interfaces (APIs). The list of interfaces is too long to mention here, but would include Excel, Matlab, Stata, Python, Julia, C++, Octave, JavaScript, Fortran, Spark, Hadoop, SQL, Oracle, Bloomberg, Datastream, etc.

Many research institutes, companies, and universities have also migrated to R, so there is a good chance that you will need to be able to work with this software at some point in the future. In my subjective opinion, the R language is particularly well suited for the following three tasks (a) data wrangling, (b) statistical modelling& machine learning, and (c) data visualisation.

**R** is a computing environment that combines: 

- a programming language called **S**, developed by John Chambers at Bell Labs, that implements the idea of *programming with data*,
- an extensive set of functions for classical and modern statistical data analysis and modeling,
- powerful numerical analysis tools for linear algebra, differential equations, and stochastics,
- graphics functions for visualizing data and model output, 
- a modular and extensible structure that supports a vast array of optional add-on packages, and
- extensive help and documentation facilities.

**R** is an [open source software project](https://en.wikipedia.org/wiki/Open-source_software), available for [free download](https://www.r-project.org/) [@R].
Originally a research project in statistical computing, it is now managed by a development team that includes a number of well-regarded statisticians, and is widely used by statistical researchers and working scientists as a platform for making new methods available to users.

There are a number of graphical front-ends (IDE) for **R**.
At present, the best of these appears to be [**RStudio**](http://www.rstudio.com).
Before learning about these, however, you should learn a little about **R** itself.



# GETTING STARTED WITH **R** 

## Installing **R** on your computer

The main source for **R** is the [Comprehensive R Archive Network (CRAN)](http://cran.r-project.org). 

You can get the source code and compile it yourself, but you may prefer at this point to download and install a precompiled version. 

You can download precompiled binaries for most major platforms from any CRAN mirror.
To do so, go to http://cran.r-project.org/mirrors.html and find a mirror site that is geographically somewhat near you.

Find the appropriate page for your operating system, and read and follow the installation instructions. **Be sure to install the latest version.**

**R** should work well on any reasonably recent computer.

Under Windows, **R** is installed by launching the downloaded file and following the on-screen instructions. 
At the end you'll have an **R** icon on your desktop that can be used to launch the program. 

Under Linux, binary versions of **R** are available as packages for the most common Linux distributions.
It is also not difficult to install **R** from source.

Under Mac OS X, **R** is available as a binary package.

The standard distributions of **R** include several *packages*: user-contributed suites of add-on functions.
These notes use some additional packages which you will have to install before using.
&#9420;&nbsp;In the Windows version additional packages can be installed easily from within **R** using the **Packages** menu.
Under all platforms, you can use the commands `install.packages()` and `update.packages()` to install and update packages, respectively.
Most packages are available pre-compiled for MacOS X and Windows;
under Linux, **R** will download and compile the source code for you.

## Starting **R** 

Execute `R` on the command line.

Click on the icon on your desktop, or in the `Start` menu (if you allowed the Setup program to make either or both of these).

## Stopping **R** 

Stop **R** by typing `q()` at the command prompt.
[Note the `()`: if you type `q` by itself, you will get some confusing output which is actually **R** trying to tell you the definition of the `q` function;
more on this later.]

You can also stop **R** from the `File` menu.

When you quit, **R** will ask you if you want to save the workspace (that is, all of the variables you have defined in this session); 
in general, you should say "no" to avoid clutter and unintentional confusion of results from different sessions.
**Note:**  When you say "yes" to saving your workspace, it is saved in a hidden file named `.RData`.
By default, when you open a new **R** session in the same directory, this workspace is loaded and a message informing you so is printed:

```
[Previously saved workspace restored]
```

Should an **R** command seem to be stuck or take longer than you're willing to wait, click on the stop sign on the menu bar or hit the `Esc` key (&#9420;), or `Ctrl-c` (&#9421;).


# INTERACTIVE CALCULATIONS

When you start **R**, a console window is opened.
The console has a few basic menus at the top; check them out on your own. 

The console is where you enter commands for **R** to execute *interactively*, meaning that the command is executed and the result is displayed as soon as you hit the ` Enter` key. 
For example, at the command prompt `>`, type in `2+2` and hit `Enter`; you will see 

```r
2+2 
```

```
## [1] 4
```

The results from calculations can be stored in (*assigned to*) variables. 
For example:

```r
a <- 2+2
```
**R** automatically creates the variable `a` and stores the result (4) in it, but by default doesn't print anything.
To ask **R** to print the value, just type the variable name by itself

```r
a
```

```
## [1] 4
```
The `[1]` at the beginning of the line is just **R** printing an index of element numbers;
if you print a result that displays on multiple lines, **R** will put an index at the beginning of each line.

`print(a)` also works to print the value of a variable.
By default, a variable created this way is a *vector* (an ordered list), and it is *numeric* because we gave **R** a number rather than (e.g.) a character string like `"pxqr"`;
in this case `a` is a numeric vector of length 1;

You could also type
```
a <- 2+2; a
```
using a semicolon to put two or more commands on a single line.
Conversely, you can break lines *anywhere that* **R** *can tell you haven't finished your command* and **R** will give you a "continuation" prompt (`+`) to let you know that it doesn't think you're finished yet: try typing
```
a <- 3*(4+
5)
```
to see what happens (this often happens e.g. if you forget to close parentheses or quotes).
If you get stuck continuing a command you don't want---e.g., you opened the wrong parentheses---just hit `Ctrl-c`, the `Esc` key or the stop icon in the menu bar to get out.

You can assign values to variables in **R** using the `<-` operator.
There are several alternative forms for assignment.
Each of these three commands accomplishes the same thing;
the first is the preferred form, however.
```
a <- 2+2
2+2 -> a
a = 2+2
```

Variable names in **R** must begin with a letter, followed by alphanumeric characters. 
You can break up long names with a period, as in `long.variable.number.3`, an underscore (`very_very_long_variable_name`), or by using camel case (`quiteLongVariableName`); 
you cannot use blank spaces in variable names. 
**R** is case sensitive:
`Abc` and `abc` are different variables.
Good practice is to make variable names long enough to be evocative but short enough to type easily.
`N.per.ha` or `pop.density` are better than `x` and `y` (too generic) or `available.nitrogen.per.hectare` (too long).

Note that `c`, `q`, `t`, `C`, `D`, `F`, `I`, and `T`, are built-in **R** functions.
Using these are variable names may cause confusion or actual errors.

**R** uses `+`, `-`, `*`, `/`, and `^` for addition, subtraction, multiplication, division and exponentiation, respectively.
For example:

```r
x <- 5
y <- 2
z1 <- x*y
z2 <- x/y
z3 <- x^y
z1; z2; z3
```

```
## [1] 10
```

```
## [1] 2.5
```

```
## [1] 25
```

You can retrieve and edit previous commands. 
The up-arrow (\thinspace $\uparrow$ \thinspace) key or `Ctrl-p` recalls previous commands to the prompt. 
For example, you can bring back the second-to-last command and edit it to

```r
z3 <- 2*x^y
```
Experiment with the $\downarrow$, $\rightarrow$, $\leftarrow$, `Home` and `End` keys too (also `Ctrl-n`, `Ctrl-b`, `Ctrl-f`, `Ctrl-a`, `Ctrl-e`).

You can combine several operations in one calculation:

```r
A <- 3
C <- (A+2*sqrt(A))/(A+5*sqrt(A)); C
```

```
## [1] 0.5543706
```

Parentheses specify the order of operations. 
The command

```r
C <- A+2*sqrt(A)/A+5*sqrt(A)
```
is not the same as the one above;
rather, it is equivalent to 

```r
C <- A + 2*(sqrt(A)/A) + 5*sqrt(A)
```
The default order of operations is:
(1) parentheses,
(2) exponentiation,
(3) multiplication and division,
(4) addition and subtraction.
Thus:

`> b <- 12-4/2^3`  ***gives***  `12 - 4/8 = 12 - 0.5 = 11.5`  
`> b <- (12-4)/2^3` ***gives*** `8/8 = 1`  
`> b <- -1^2`   ***gives***  `-(1^2) = -1`  
`> b <- (-1)^2` ***gives***  `1`  

In complicated expressions it's best to **use parentheses to specify explicitly what you want**, such as ` > b <- 12 - (4/(2^3)) ` or at least ` > b <- 12 - 4/(2^3) `; 
a few extra sets of parentheses never hurt anything.

Some of the built-in mathematical functions in **R**.

**R** command             | function
-----------------         | ------------------------------
`abs()`                   | absolute value, $|x|$
`cos()`, `sin()`, `tan()` | cosine, sine, tangent
`acos()`, `asin()`, `atan()` | arc-cosine, arc-sine, arc-tangent
`exp(x)`     | exponential function, $e^x$
`log(x)`     | natural (base-$e$) logarithm, $\log{x}=\ln{x}$
`log10(x)`   | base-10 logarithm, $\log_{10}{x}$
`sqrt(x)`    | square root, $\sqrt{x}$
`atan2(y,x)` | $\arctan(y/x)$
`sum()`      | sum of the entries in a vector
`diff()`     | first differences
`cumsum()`   | cumulative sum
`gamma(x)`   | the Gamma function, $\Gamma(x)$


You can get a more complete list of functions from the help system (check below): 
`?Arithmetic` for arithmetic operations, `?log` for logarithmic functions, `?sin` for trigonometric functions, and `?Special` for special functions.



# THE HELP SYSTEM

**R** has a help system, although it is generally better for reminding you about syntax or details, and for giving cross-references, than for answering basic "how do I ...?" questions.

- You can get help on an **R** function named `foo` by entering

```r
?foo
```
or

```r
help(foo)
```
into the console window (e.g., try `?sin`). 

By default, **R** 's help system only provides information about functions that are in the base system and packages that you have already loaded (see below).

- `??topic` or `help.search("topic")` will list information related to `topic` available in the base system or in any extra installed packages:
then use `?topic` to see the information, perhaps using
`library(pkg)` to load the appropriate package first.
- `help.search` uses fuzzy matching---for example, `help.search("log")` finds more than 800 entries (on my particular system) including lots of functions with "plot", which includes the letters "lot", which are *almost* like "log".
If you can't stand it, you can turn this behavior off with the incantation `help.search("log",agrep=FALSE)` (81 results which still include matches for "logistic", "myelogenous", "phylogeny", \dots).
- `help(package="pkg")` will list all the help pages for a loaded package.
- `example(fun)` will run the examples (if any) given in the help for a particular function `fun`: e.g., `example(log)`
- `RSiteSearch("topic")` does a full-text search of all the **R** documentation and the mailing list archives for information on `topic` (you need an active internet connection).
- the `sos` package is a web-aware help function that searches all of the packages on CRAN; its `findFn` function tries to find and organize functions in any package on CRAN that match a search string (again, you need a network connection for this).

Try out one or more of these aspects of the help system.


# INTERACTIVE SESSION

## Descriptive statistics

Below are some data on the maximum growth rate $r_{\text{max}}$ of laboratory populations of the green alga *Chlorella vulgaris* as a function of light intensity ($\mu\mathrm{E}~\mathrm{m}^{-2}~\mathrm{s}^{-1}$).

| 
------|------------------------------------------------------
Light: | 20,  20,  20,  20,  21,  24,  44,  60,  90,  94, 101
$r_{\text{max}}$: | 1.73, 1.65, 2.02, 1.89, 2.61, 1.36, 2.37, 2.08, 2.69, 2.32, 3.67


To analyze these data in **R** , first enter them as numerical *vectors*: 

```r
Light <- c(20,20,20,20,21,24,44,60,90,94,101)
rmax <- c(1.73,1.65,2.02,1.89,2.61,1.36,2.37,2.08,2.69,2.32,3.67)
```

The function `c()` *combines* the individual numbers into a vector.
Try recalling (with $\uparrow$) and modifying the above command to 

```r
Light <- 20,20,20,20,21,24,44,60,90,94,101
```
and see the error message you get: 
in order to create a vector of specified numbers, you must use the `c()` function.

To see a histogram of the growth rates enter `hist(rmax)`, which opens a graphics window and displays the histogram. 
There are **many** other built-in statistics functions in **R**.
Some of the most commonly used are shown in the table below.
Play around with these functions, and any others you can think of.

---------------------------

| 
--------------------|------------------------------------
`mean(x)`           | the arithmetic mean of the data in `x`
`exp(mean(log(x)))` | the geometric mean of `x`
`1/mean(1/x)`       | the harmonic mean of `x`
`median(x)`         | the median of `x`
`min(x)`, `max(x)`  | the minimum and maximum, respectively, of `x`
`range(x)`          | the range of `x`
`sd(x)`             | the standard deviation of `x`
`var(x)`            | the variance of `x`
`quantile(x, p)`    | the `p`-th quantiles of `x`
`ecdf(x)`        | the empirical cumulative distribution function of `x`

Table: Some simple descriptive statistics in **R**

---------------------------

To see how the algal rate of increase varies with light intensity, type

```r
plot(rmax~Light)
```
to plot `rmax` ($y$) against `Light` ($x$).
You can also do `plot(Light,rmax)`.
Based on what you see, does it seem reasonable to hypothesize a linear relationship between these variables?

## Linear regression

To perform linear regression we create a linear model using the `lm()` function:

```r
fit <- lm(rmax~Light)
```
In the formula `rmax~Light`, `rmax` is the response variable and `Light` is the predictor.

The `lm` command created an *object* we have named `fit`.
In **R** , an *object* is a data structure consisting of multiple parts.
In this case, `fit` holds the results of the linear regression analysis. 
Unlike other statistics packages, **R** rarely summarizes an analysis for you by default.
Statistical analyses in **R** are done by fitting a model to data and then issuing additional commands to extract desired information about the model or display results graphically.

To get a summary of the results, enter the command `summary(fit)`. 
**R** sets up model objects (more on this later) so that the function `summary()` "knows" that `fit` was created by `lm()`, and produces an appropriate summary of results for an `lm()` object: 

```r
summary(fit)
```

```
## 
## Call:
## lm(formula = rmax ~ Light)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.5478 -0.2607 -0.1166  0.1783  0.7431 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.580952   0.244519   6.466 0.000116 ***
## Light       0.013618   0.004317   3.154 0.011654 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4583 on 9 degrees of freedom
## Multiple R-squared:  0.5251,	Adjusted R-squared:  0.4723 
## F-statistic: 9.951 on 1 and 9 DF,  p-value: 0.01165
```

If you've had a statistics course, the output will make sense to you. 
The table of coefficients gives the estimated regression line as
$$\text{rmax}=1.58+0.0136\times\text{Light}.$$
Associated with each coefficient is the standard error of the estimate.
In addition, for each coefficient, a $t$-test is performed testing the hypothesis that the coefficient differs from zero:
the $t$-statistic and the corresponding $p$-values are reported.
Finally, the summary reports $R^2$ values and the results of an $F$-test comparing the model to a simple normal null hypothesis.

You can add the regression line to the plot of the data with a function taking `fit` as its input (if you closed the plot of the data, you will need to create it again in order to add the regression line):

```r
abline(fit)
```
[`abline` is a general-purpose function for adding lines to a plot.
You can specify horizontal or vertical lines, a slope and an intercept, or a regression model: `?abline`.] 

![](Lecture-4-R-syntax--refresher-_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

You can extract the coefficients from the fitted model using `coef`:


```r
coef(fit)
```

```
## (Intercept)       Light 
##  1.58095214  0.01361776
```

There are many other results stored inside `fit`.
Try the following:


```r
residuals(fit)
fitted(fit)
effects(fit)
vcov(fit)
anova(fit)
```

# STATISTICS **R** 

Some of the important functions and packages (collections of functions) for statistical modeling and data analysis are summarized in following table.
Functions are named in `fixed-width font` and packages in **bold face**.
The book *Modern Applied Statistics with S* gives a good practical overview, and a list of available packages and their contents can be found at the main **R** website (http://cran.r-project.org, and click on `Packages`). 

A few of the functions and packages in **R** for statistical modeling and data analysis are listed below.
There are *many* more, but you will have to learn about them somewhere else.

-------------------------------

&nbsp;   | &nbsp;
--------------------------|------------------------
`aov`, `anova`            | Analysis of variance or deviance
`lm`                      | Linear models (regression, ANOVA, ANCOVA) 
`glm`                     | Generalized linear models (e.g. logistic, Poisson regression) 
`gam`                     | Generalized additive models (in package **mgcv**) 
`nls`                     | Nonlinear models via least-squares
`lme`, `nlme`             | Linear and nonlinear mixed-effects models (repeated measures, block effects, spatial models): in package **nlme** 
`princomp`, `manova`, `lda`, `cancor`  | Multivariate analysis (see also packages **vegan**, **ade4**)
**boot**                    | Package: bootstrapping functions 
**splines**                 | Package: nonparametric regression (more in packages **fields**, **KernSmooth**, **logspline**, **sm** and others)
**survival**                | Package: survival analysis 
**tree**, **rpart**         | Package: tree-based regression

Table: Statistical modeling and data analysis functions and packages.

-------------------------------


# THE **R** PACKAGE SYSTEM

**R** has many extra packages that provide extra functions.
You may be able to install new packages from a menu within **R**. 
You can always type, e.g.,

```r
install.packages("ggplot2")
```
This installs the **ggplot2** package.
You can install more than one package at a time:

```r
install.packages(c("plyr","reshape2"))
```
If the machine on which you use **R** is not connected to the Internet, you can download the packages to some other medium (such as a flash drive or CD)
and install them later, using `Install from local zip file` in the menu (&#9420;) or

```r
install.packages("ggplot2",repos=NULL)
```

If you do not have permission to install packages in **R** 's central directory, **R** will may ask whether you want to install the packages in a user-specific directory.
It is safe to answer "yes" here.

You will frequently get a warning message like
```
Warning message: In file.create(f.tg) :
cannot create file '.../packages.html', reason 'Permission denied'.
```
Don't worry about this;
it means the package has been installed successfully, but the main help system index files couldn't be updated because of file permissions problems.


# DATA STRUCTURES IN **R**

## Vectors 

The most basic data-type in **R** is the vector.
A vector is just a 1-dimensional array of values.
Several different kinds of vectors are available:

- numerical vectors,
- logical vectors,
- character-string vectors,
- factors,
- ordered factors, and
- lists.

Lists are a bit different from the other kinds, so we'll postpone talking about them until later.

A vector's defining attributes are its *mode*---which kind of vector it is--- and its **length**.
Vectors can also have a `names` attribute, which allows one to refer to elements by name.

We've already seen how to create vectors in **R** using the `c` function, e.g.,

```r
x <- c(1,3,5,7,9,11)
y <- c(6.5,4.3,9.1,-8.5,0,3.6)
z <- c("dog","cat","dormouse","chinchilla")
w <- c(a=4,b=5.5,c=8.8)

length(x)
```

```
## [1] 6
```

```r
mode(y)
```

```
## [1] "numeric"
```

```r
mode(z)
```

```
## [1] "character"
```

```r
names(w)
```

```
## [1] "a" "b" "c"
```

The nice thing about having vectors as a basic type is that many operations in **R** are efficiently *vectorized*.
That is, the operation acts on the vector as a unit, saving you the trouble of treating each entry individually.
For example:

```r
x <- x+1
xx <- sqrt(x)
x; xx
```

```
## [1]  2  4  6  8 10 12
```

```
## [1] 1.414214 2.000000 2.449490 2.828427 3.162278 3.464102
```

Notice that the operations were applied to every entry in the vector. 
Similarly, commands like `x-5, 2*x, x/10`, and `x^2` apply subtraction, multiplication, division, and square to each element of the vector. 
The same is true for operations involving multiple vectors:


```r
x+y
```

```
## [1]  8.5  8.3 15.1 -0.5 10.0 15.6
```

In **R** the default is to apply functions and operations to vectors
in an *element by element* manner; 
anything else (e.g. matrix multiplication) is done using special notation (discussed below). 

### Element recycling

**R** has a very useful, but unusual and perhaps unexpected, behavior when two vector operands in a vectorized operation are of unequal lengths.
It will effectively extend the shorter vector using element "re-cycling": 
re-using elements of the shorter vector.
Thus

```r
x <- c(1,2,3)
y <- c(10,20,30,40,50,60)
x+y
```

```
## [1] 11 22 33 41 52 63
```

```r
y-x
```

```
## [1]  9 18 27 39 48 57
```

### Functions for creating vectors

A set of regularly spaced values can be created with the `seq` function, whose syntax is `x <- seq(from,to,by)` or `x <- seq(from,to)` or `x <- seq(from,to,length.out)`.
The first form generates a vector `(from,from+by,from+2*by,...)` with the last entry not extending further than `to`;
in the second form the value of `by` is assumed to be 1 or -1, depending on whether `from` or `to` is larger; 
and the third form creates a vector with the desired endpoints and length.
There is also a shortcut for creating vectors with `by=1`:

```r
1:8
```

```
## [1] 1 2 3 4 5 6 7 8
```



A constant vector such as `(1 1 1 1)` can be created with `rep` function, whose basic syntax is `rep(values,lengths)`.  
For example,

```r
rep(3,5)
```

```
## [1] 3 3 3 3 3
```
creates a vector in which the value 3 is repeated 5 times. 
`rep()` will repeat a whole vector multiple times

```r
rep(1:3,3)
```

```
## [1] 1 2 3 1 2 3 1 2 3
```
or will repeat each of the elements in a vector a given number of times:

```r
rep(1:3,each=3)
```

```
## [1] 1 1 1 2 2 2 3 3 3
```
Even more flexibly, you can repeat each element in the vector a different number of times:

```r
rep(c(3,4),c(2,5))
```

```
## [1] 3 3 4 4 4 4 4
```
The value 3 was repeated 2 times, followed by the value 4 repeated 5 times.
`rep()` can be a little bit mind-blowing as you get started, but you'll get used to it---and it will turn out to be useful.


-----------------------------------

|
----------------------------|-----------------------------------
`seq(from,to,by=1)`         | Vector of evenly spaced values (default increment = 1) 
`seq(from,to,length.out)`   | Vector of evenly spaced values, specified length 
`c(u,v,...) `               | Combine a set of numbers and/or vectors into a single vector 
`rep(a,b)`                  | Create vector by repeating elements of `a` `b` times each
`hist(v)`                   | Histogram plot of value in v 
`mean(v),var(v),sd(v)`      | Estimate of population mean, variance, standard deviation based on data values in `v` 
`cov(v,w)`                  | Covariance between two vectors 
`cor(v,w)`                  | Correlation between two vectors 

Table: Some important **R** functions for creating and working with vectors.

Many of these have other optional arguments; use the help system (e.g. `?cor`) for more information. 
The statistical functions such as `var` regard the values as samples from a population and compute the unbiased estimate of the population statistic; 
for example `sd(1:3)=1`.

-----------------------------------

### Vector indexing

It is often necessary to extract a specific entry or other part of a vector. 
This procedure is called *vector indexing*, and uses square brackets ([]):

```r
z <- c(1,3,5,7,9,11); z[3]
```

```
## [1] 5
```
`z[3]` extracts the third element of the vector `z`. 
You can also access a block of elements by giving a vector of indices:

```r
v <- z[c(2,3,4,5)]
```
or

```r
v <- z[2:5]; v
```

```
## [1] 3 5 7 9
```
This has extracted the 2nd through 5th elements in the vector. 

Extracted parts of a vector don't have to be regularly spaced. For example

```r
v <- z[c(1,2,5)]; v
```

```
## [1] 1 3 9
```

Indexing is also used to **set specific values within a vector**. 
For example, 

```r
z[1] <- 12
```
changes the value of the first entry in `z` while leaving all the rest alone, and 

```r
z[c(1,3,5)] <- c(22,33,44)
```
changes the 1st, 3rd, and 5th values.

Elements in a named vector can be accessed and modified by name as well as by position.
Thus

```r
w
```

```
##   a   b   c 
## 4.0 5.5 8.8
```

```r
w["a"]
```

```
## a 
## 4
```

```r
w[c("c","b")]
```

```
##   c   b 
## 8.8 5.5
```

```r
w["b"] <- 0
w
```

```
##   a   b   c 
## 4.0 0.0 8.8
```

ou may be wondering if vectors in **R** are row vectors or column vectors (if you don't know what those are, don't worry).
The answer is "both and neither".
Vectors are printed out as row vectors, but if you use a vector in an operation that succeeds or fails depending on the vector's orientation, **R** will assume that you want the operation to succeed and will proceed as if the vector has the necessary orientation.
For example, **R** will let you add a vector of length 5 to a $5 \times 1$ matrix or to a $1 \times 5$ matrix, in either case yielding a matrix of the same dimensions.


### Logical operations

Some operations return a logical value (i.e., `TRUE` or `FALSE`). 
For example, try:


```r
a <- 1; b <- 3; 
c <- a < b
d <- (a > b)
c; d
```

```
## [1] TRUE
```

```
## [1] FALSE
```

The parentheses around `a > b` above are optional but do make the code easier to read.
Be careful when you make comparisons with negative values:
`a<-1` may surprise you by setting `a` to 1, because `<-` is the assignment operator in **R**.
Use `a< -1` or `a<(-1)` to make this comparison.

-----------------------------------

**R** code | comparison
---------|-----------------------------------
`x < y`  | $x$ strictly less than $y$
`x > y`  | $x$ strictly greater than $y$
`x <= y` | $x$ less than or equal to $y$
`x >= y` | $x$ greater than or equal to $y$
`x == y` | $x$ equal to $y$
`x != y` | $x$ *not* equal to $y$
`identical(x,y)` | $x$ completely identical to $y$
`all.equal(x,y)` | $x$ pretty much equal to $y$

Table: Some comparison operators in **R**. Use `?Comparison` to learn more.

-----------------------------------

When we compare two vectors or matrices, comparisons are done element-by-element (and the recycling rule applies).
For example,

```r
x <- 1:5; b <- (x<=3); b
```

```
## [1]  TRUE  TRUE  TRUE FALSE FALSE
```
So if `x` and `y` are vectors, then `(x==y)` will return a vector of values giving the element-by-element comparisons. 
If you want to know whether `x` and `y` are identical vectors, use `identical(x,y)` or `all.equal(x,y)`. 
You can use `?Logical` to read more about logical operations. 
**Note the difference between = and ==.**
**Can you figure out what happened in the following cautionary tale?**

```r
a=1:3
b=2:4
a==b
```

```
## [1] FALSE FALSE FALSE
```

```r
a=b
a==b
```

```
## [1] TRUE TRUE TRUE
```

**R** can also do arithmetic on logical values, treating `TRUE` as 1 and `FALSE` as 0.
So `sum(x<3)` returns the value 2, telling us that two entries of `x` satisfied the condition (`x<3`). 
This is useful for counting the number of elements of a vector that satisfy a given condition.

More complicated conditions are built by using **logical operators** to combine comparisons.
The most important of these are tabulated here.

-----------------------------------

operator  | meaning
----------|--------------
`!`       |   logical NOT
`&`       |   logical AND, elementwise
`&&`      |  logical AND, first element only
`|`       |   logical OR, elementwise 
`||`      |  logical OR, first element only 
`xor(x,y)`| exclusive OR, elementwise

Table: Logical operators.

-----------------------------------

For example, try

```r
a <- c(1,2,3,4)
b <- c(1,1,5,5)
(a<b) | (a>3)
```

```
## [1] FALSE FALSE  TRUE  TRUE
```

```r
(a<b) || (a>3)
```

```
## [1] FALSE
```
and make sure you understand what happened. 

The two forms of logical OR (`|`and `||`) are *inclusive*, meaning that `x|y` is true if either `x` or `y` or both are true. 
Use `xor` when exclusive OR is needed.
The two forms of AND and OR differ in how they handle vectors. 
The shorter one (`|`, `&`) does element-by-element comparisons; 
the longer one (`||`, `&&`) looks only at the first element in each vector. 

### More on vector indexing 

We can also use *logical* vectors (lists of `TRUE` and `FALSE` values) to pick elements out of vectors.
This is useful, for example, in subsetting data.

As a simple example, we might want to focus on just the low-light values of $r_{\text{max}}$ in the *Chlorella* example:




```r
lowLight <- Light[Light<50]
lowLightrmax <- rmax[Light<50]
lowLight
```

```
## [1] 20 20 20 20 21 24 44
```

```r
lowLightrmax
```

```
## [1] 1.73 1.65 2.02 1.89 2.61 1.36 2.37
```

What is really happening here (think about it for a minute) is that `Light<50` generates a logical vector the same length as `Light` (`TRUE TRUE TRUE ...`) which is then used to select the appropriate values.

If you want the positions at which `Light` is lower than 50, you can use `which`: `which(Light<50)`.
If you wanted the position at which the maximum value of `Light` occurs, you could say `which(Light==max(Light))` or `which.max(Light)`.
Note that, if `Light` has several elements that are maximal, the first will return the positions of them all, while the second will return the position only of the *first* one.


## Matrices and arrays

### Creating matrices

A matrix is a two-dimensional array of items.
Most straightforwardly, we can create a matrix by specifying the number of rows and columns, and specifying the entries.
For example 

```r
X <- matrix(c(1,2,3,4,5,6),nrow=2,ncol=3); X
```

```
##      [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
```
takes the values 1 to 6 and reshapes them into a 2 by 3 matrix. 
Note that the values in the data vector are put into the matrix *column-wise*, by default.
You can change this by using the optional parameter `byrow`: 

```r
A <- matrix(1:9,nrow=3,ncol=3,byrow=TRUE); A
```

```
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    4    5    6
## [3,]    7    8    9
```

**R** will re-cycle through entries in the data vector, if need be, to fill a matrix of the specified size. 
So for example 

```r
matrix(1,nrow=50,ncol=50)
```
creates a $50{\times}50$ matrix, every entry of which is $1$.

**R** will also cause a matrix to behave like a vector whenever it makes sense: 
for example `sum(X)` above is 21.

Another useful function for creating matrices is `diag`.
`diag(v,n)` creates an $n{\times}n$ matrix with data vector $v$ on its diagonal. 
So for example `diag(1,5)` creates the $5{\times}5$ *identity matrix*, which has 1s on the diagonal and 0 everywhere else.

Finally, one can use the `data.entry` function. 
This function can only edit existing matrices, but for example (try this now!)

```r
A <- matrix(0,3,4)
data.entry(A)
```
will create `A` as a $3{\times}4$ matrix, and then call up a spreadsheet-like interface in which the values can be edited directly.
You can further modify `A` with the same primitive interface, using `fix`.

-----------------------------------

**R** code                | purpose
--------------------------|---------------------------------------------
`matrix(v,nrow=m,ncol=n)` | $m \times n$ matrix using the values in `v` 
`t(A)`                    | transpose (exchange rows and columns) of matrix `A` 
`dim(X)`                  | dimensions of matrix X. `dim(X)[1]`=\# rows, `dim(X)[2]`=\# columns 
`data.entry(A)`           | call up a spreadsheet-like interface to edit the values in `A` 
`diag(v,n)`               | diagonal $n \times n$ matrix with $v$ on diagonal, 0 elsewhere (`v` is 1 by default, so `diag(n)` gives an $n \times n$ identity matrix)
`cbind(a,b,c,...)`        | combine compatible objects by attaching them along columns 
`rbind(a,b,c,...)`        | combine compatible objects by attaching them along rows 
`as.matrix(x)`            | convert an object of some other type to a matrix, if possible 
`outer(v,w)`              | "outer product" of vectors `v`, `w`: the matrix whose $(i,j)$-th element is `v[i]*w[j]` 

Table: Some important functions for creating and working with matrices.

Many of these functions have additional optional arguments;
use the help system for full details.

-----------------------------------

### `cbind` and `rbind` 

If their sizes match, vectors can be combined to form matrices, and matrices can be combined with vectors or matrices to form other matrices. 
The functions that do this are `cbind` and `rbind`. 

`cbind` binds together columns of two objects. 
One thing it can do is put vectors together to form a matrix: 

```r
C <- cbind(1:3,4:6,5:7); C
```

```
##      [,1] [,2] [,3]
## [1,]    1    4    5
## [2,]    2    5    6
## [3,]    3    6    7
```
Remember that **R** interprets vectors as row or column vectors according to what you're doing with them. 
Here it treats them as column vectors so that columns exist to be bound together. 
On the other hand, 

```r
D <- rbind(1:3,4:6); D
```

```
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    4    5    6
```
treats them as rows. 
Now we have two matrices that can be combined. 

-----------------------------------


## Matrix indexing

Matrix indexing is like vector indexing except that you have to specify both the row and column, or range of rows and columns. 
For example `z <- A[2,3]` sets `z` equal to 6, which is the (2^nd^ row, 3^rd^ column) entry of the matrix **A** that you recently created, and 

```r
A[2,2:3]; 
```

```
## [1] 5 6
```

```r
B <- A[2:3,1:2]; B
```

```
##      [,1] [,2]
## [1,]    4    5
## [2,]    7    8
```

There is an easy shortcut to extract entire rows or columns: leave out the limits, leaving a blank before or after the comma.

```r
first.row <- A[1,]; first.row
```

```
## [1] 1 2 3
```

```r
second.column <- A[,2]; second.column;
```

```
## [1] 2 5 8
```

(What does `A[,]` do?)

As with vectors, indexing also works in reverse for assigning values to matrix entries. 
For example,

```r
A[1,1] <- 12; A
```

```
##      [,1] [,2] [,3]
## [1,]   12    2    3
## [2,]    4    5    6
## [3,]    7    8    9
```

The same can be done with blocks, rows, or columns, for example

```r
A[1,] <- c(2,4,5); A
```

```
##      [,1] [,2] [,3]
## [1,]    2    4    5
## [2,]    4    5    6
## [3,]    7    8    9
```

If you use `which()` on a matrix, **R** will normally treat the matrix as a vector---so for example `which(A==8)` will give the answer 6 (can you see why?).
However, `which()` does have an option that will treat its argument as a matrix:

```r
which(A>=8,arr.ind=TRUE)
```

```
##      row col
## [1,]   3   2
## [2,]   3   3
```

## Arrays

The generalization of the matrix to more (or less) than 2 dimensions is the array.
In fact, in **R** , a matrix is nothing other than a 2-dimensional array.
How does **R** store arrays?
In the simplest possible way: an array is just a vector plus information on the dimensions of the array.
Most straightforwardly, we can create an array from a vector:


```r
X <- array(1:24,dim=c(3,4,2)); X
```

```
## , , 1
## 
##      [,1] [,2] [,3] [,4]
## [1,]    1    4    7   10
## [2,]    2    5    8   11
## [3,]    3    6    9   12
## 
## , , 2
## 
##      [,1] [,2] [,3] [,4]
## [1,]   13   16   19   22
## [2,]   14   17   20   23
## [3,]   15   18   21   24
```

Note, again, that the arrays are filled in a particular order: the first dimension first, then the second, and so on.
A one-dimensional array is subtly different from a vector:

```r
y <- 1:5; y
```

```
## [1] 1 2 3 4 5
```

```r
z <- array(1:5,dim=5); z
```

```
## [1] 1 2 3 4 5
```

```r
y==z
```

```
## [1] TRUE TRUE TRUE TRUE TRUE
```

```r
identical(y,z)
```

```
## [1] FALSE
```

```r
dim(y); dim(z)
```

```
## NULL
```

```
## [1] 5
```


## Factors

For dealing with measurements on the nominal and ordinal scales [@Stevens1946], **R** provides vectors of type *factor*.
A factor is a variable that can take one of a finite number of distinct *levels*.
To construct a factor, we can apply the `factor` function to a vector of any class:

```r
x <- rep(c(1,2),each=3); factor(x)
```

```
## [1] 1 1 1 2 2 2
## Levels: 1 2
```

```r
trochee <- c("jetpack","ferret","pizza","lawyer")
trochee <- factor(trochee); trochee
```

```
## [1] jetpack ferret  pizza   lawyer 
## Levels: ferret jetpack lawyer pizza
```
By default, `factor` sets the levels to the unique set of values taken by the vector.
To modify that behavior, there is the `levels` argument:

```r
factor(trochee,levels=c("ferret","pizza","cowboy","scrapple"))
```

```
## [1] <NA>   ferret pizza  <NA>  
## Levels: ferret pizza cowboy scrapple
```
Note that the order of the levels is arbitrary, in keeping with the fact that the only operation permissible on the nominal scale is the test for equality.
In particular, the factors created with the `factor` command are un-ordered: there is no sense in which we can ask whether, e.g., `ferret < cowboy`.

To represent variables measured on the ordinal scale, **R** provides *ordered factors*, constructed via the `ordered` function.
An ordered factor is just like an un-ordered factor except that the order of the levels matters:



```r
x <- ordered(sample(x=letters,size=22,replace=TRUE)); x
```

```
##  [1] z m u p a g c d b n x d g n w l c t z p b x
## Levels: a < b < c < d < g < l < m < n < p < t < u < w < x < z
```
Here, we've relied on `ordered`'s default behavior, which is to put the levels in alphabetical order.
It's typically safer to specify explicitly what order we want:

```r
x <- ordered(x,levels=rev(letters))
x[1:5] < x[18:22]
```

```
## [1]  TRUE FALSE  TRUE  TRUE FALSE
```

----------------------------


## Lists

While vectors and matrices may seem familiar, lists may be new to you.
Vectors and matrices have to contain elements that are all the same type: 
lists in **R** can contain anything---vectors, matrices, other lists, arbitrary objects.
Indexing is a little different too:
use `[[ ]]` to extract an element of a list by number or name or `$` to extract an element by name (only).
Given a list like this:

```r
L <- list(A=x,B=trochee,C=c("a","b","c"))
```
Then `L$A`, `L[["A"]]`, and `L[[1]]` will each return the first element of the list.
To extract a sublist, use the ordinary single square brackets: `[]`:

```r
L[c("B","C")]
```

```
## $B
## [1] jetpack ferret  pizza   lawyer 
## Levels: ferret jetpack lawyer pizza
## 
## $C
## [1] "a" "b" "c"
```


## Data frames

Vectors, matrices, and lists of one sort or another are found in just about every programming language.
The *data frame* structure is (or was last time I checked) unique to **R** , and is central to many of **R** 's useful data-analysis features.
It's very natural to want to store data in vectors and matrices.
Thus, in the example above, we stored measurements of two variables ($r_\text{max}$ and light level) in vectors.
This was done in such a way that the observations of the first replicate were stored in the first element of each vector, the second in the second, and so on.
To explicitly bind together observations corresponding to the same replicate, we might join the two vectors into a matrix using `cbind`.
In the resulting data structure, each row would correspond to an observation, each column to a variable.
This is possible, however, only because both variables are of the same type: they're both numerical.
More commonly, a data set is made up of several different kinds of variables.
The data frame is **R** 's solution to this problem.

Data frames are a hybrid of lists and vectors.
Internally, they are a list of vectors which can be of different types but must all be the same length.
However, they behave somewhat like matrices, in that you can do most things to them that you can do with matrices.
You can index them either the way you would index a list, using `[[ ]]` or `$`---where each variable is a different item in the list---or the way you would index a matrix.  
You can turn a data frame into a matrix (using `as.matrix()`, but only if all variables are of the same class) and a matrix into a data frame (using `as.data.frame()`).

When data are read into **R** from an external file using one of the `read.xxx` commands (`read.csv`, `read.table`, `read.xls`, etc.), the object that is created is a data frame.

```r
data.url <- "http://kingaa.github.io/R_Tutorial/ChlorellaGrowth.csv"
dat <- read.csv(data.url,comment.char='#')
dat
```

```
##    light rmax
## 1     20 1.73
## 2     20 1.65
## 3     20 2.02
## 4     20 1.89
## 5     21 2.61
## 6     24 1.36
## 7     44 2.37
## 8     60 2.08
## 9     90 2.69
## 10    94 2.32
## 11   101 3.67
```

# PROBABILITY DISTRIBUTIONS

**R** contains a great deal of distilled knowledge about probability distributions.
In particular, for each of a large class of important distributions, methods to compute probability distribution functions (p.d.f., i.e., density or mass functions), cumulative distribution functions (c.d.f.), and quantile functions are available, as are methods for simulating these distributions (i.e., drawing random deviates with the desired distribution).
Conveniently, these are all named using the same scheme, as shown in the following table.

----------------------------

&nbsp;   | &nbsp;
-------------------|----------------------------------
`dxxx(x, ...)` | probability distribution function
`pxxx(q, ...)` | cumulative distribution function
`qxxx(p, ...)` | quantile function (i.e., inverse of `pxxx`)
`rxxx(n, ...)` | simulator

Table: Functions for working with probability distributions.

In the above `xxx` stands for the abbreviated name of the specific distribution (the "**R** name" as given in following table).
In each case, the `...` indicates that additional, distribution-specific, parameters are to be supplied.
The following table lists some of the more common distributions built in to **R**.
A complete list of distributions provided by the base **stats** package can be viewed by executing `?Distributions`.

----------------------------

Distribution | **R** name | Parameters | Range
-------------|------------|------------|---------
Discrete distributions|||
Binomial | `binom` | size, prob | $0,1,\dots,\mathrm{size}$
Bernoulli | `binom` (`size=1`) | prob | $0,1$
Poisson | `pois` | lambda | $0,1,\dots,\infty$
Negative binomial | `nbinom` | size, (prob or mu) | $0,1,\dots,\infty$
Geometric | `geom` | rate | $0,1,\dots,\infty$
Hypergeometric | `hyper` | m, n, k | $0,1,\dots,m$
Multinomial | `multinom` | size, prob | $\{(x_1,\dots,x_m): \sum\!x_i=\mathrm{size}\}$
Continuous distributions | &nbsp; | &nbsp; | &nbsp;
Uniform | `unif` | min, max | $[\mathrm{min},\mathrm{max}]$
Normal | `norm` | mean, sd | $(-\infty,\infty)$
Gamma | `gamma` | shape, (scale or rate) | $[0,\infty)$
Exponential | `exp` | mu | $[0,\infty)$
Beta | `beta` | shape1, shape2 | $[0,1]$
Lognormal | `lnorm` | meanlog, sdlog | $[0,\infty)$
Cauchy | `cauchy` | location, scale | $(\infty,\infty)$
$\chi^2$ | `chisq` | df | $[0,\infty)$
Student's T | `t` | df | $[0,\infty)$
Weibull | `weibull` | shape, scale | $[0,\infty)$

Table: The most commonly used built-in probability distributions.

For complete documentation, execute e.g., `?dbinom` in an **R** session.


# SCRIPTS AND DATA FILES

Modeling and complicated data analysis are accomplished more efficiently using *scripts*, which are a series of  commands stored in a text file. 
The Windows and MacOS versions of **R** both have basic script editors, but any text editor can be used, e.g. **emacs** (with ESS, "emacs speaks statistics").
You **should never** use MS Word or other word processors!

Most programs for working with models or analyzing data follow a simple pattern of program parts:

1. Setup statements.
2. Data input.
3. Computations on the data.
4. Output text and/or graphics.

For example, a script file might

1. Load some packages, or "source" another script file that creates some functions (more on functions later). 
2. Read in data from a text file.
3. Fit several statistical models to the data and compare them.
4. Plot the results and save them to disk for inclusion in a paper. 

Even for relatively simple tasks, script files are useful for building up a calculation step-by-step, making sure that each part works before adding on to it.
They are also **essential** for making research reproducible.

Tips for working with data and script files (sounding slightly scary but just trying to help you avoid common pitfalls):

- To let **R** know where data and script files are located, you have a few choices:
	(1) change your working directory to wherever the file(s) are located before running **R**.
	(2) change your working directory within an **R** session to wherever the file(s) are located using the `setwd()` (**set** **w**orking **d**irectory) function, e.g. `setwd("c:/temp")`.
	(3) spell out the path to the file explicitly.  

The first option is far preferable, since it makes your codes portable:
you can copy the whole directory wherever you like and **R** will always find what it needs.

- Use a single forward slash to separate folders (e.g., `"c:/My~Documents/R/script.R"` or `"/home/kingaa/projects/hurricanes"`): this works on all platforms.

- It's important that script files be preserved as *plain text* and data files also as plain text, using comma- or tab-separated format.

There are several common situations that lead to files saved in bad formats:

(1) if you use a web browser to download files, be careful that it doesn't automatically append some weird suffix to the files or translate them into something other than plain text.
(2) if your web browser uses "file associations" (e.g., it thinks that all files ending in `.dat` are Excel files), make sure to save the file as plain text, and without any extra extensions; 
(3) **never use Microsoft Word or other word-processors to edit your data and script files**; 
MS Word will try very hard to get you to save them as Word (rather than text) files, which will screw them up!

(4) If you send script files by e-mail, even if you are careful to send them as plain text, lines will occasionally get broken in different places, which can lead to confusion.
Beware.

As a first example, the file `Intro1.R` has the commands from the interactive regression analysis. 
Download `Intro1.R` and save it to your computer:


```r
course.url <- "http://kingaa.github.io/R_Tutorial/"
download.file(paste0(course.url,"Intro1.R"),destfile="Intro1.R",mode="w")
```
Open **your copy** of `Intro1.R`. 
In your editor, select and Copy the entire text of the file, and then Paste the text into the **R** console window. 
This has the same effect as entering the commands by hand into the console: they will be executed and so a graph is displayed with the results. 
Cut-and-Paste allows you to execute script files one piece at a time (which is useful for finding and fixing errors). 
The `source` function allows you to run an entire script file, e.g., 

```r
source("Intro1.R")
```

Another important time-saver is loading data from a text file. 
Grab copies of `Intro2.R` and `ChlorellaGrowth.csv` from the dropsite to see how this is done. 

```r
download.file(paste0(course.url,"Intro2.R"),destfile="Intro2.R",mode="w")
download.file(paste0(course.url,"ChlorellaGrowth.csv"),destfile="ChlorellaGrowth.csv",mode="w")
```
In `ChlorellaGrowth.csv` the two variables are entered as columns of a data matrix. 
Then instead of typing these in by hand, the command

```r
X <- read.csv("ChlorellaGrowth.csv",comment.char='#')
```
reads the file (from the current directory) and puts the data values into the variable `X`.
**Note** that as specified above you need to make sure that **R** is looking for the data file in the right place:
make sure to download the data file into your current working directory.
Note also the `comment.char` option in the call to `read.csv`; 
this tells **R** to ignore lines that begin with a `#` and allows us to use self-documenting data files (a very good practice).

Extract the variables from `X` with the commands

```r
Light <- X[,1]
rmax <- X[,2]
```
Think of these as shorthand for "`Light` = everything in column 1 of `X`", and "`rmax` = everything in column 2 of `X`" (we'll learn about working with data frames later).
From there on out it's the same as before, with some additions that set the axis labels and add a title.

-----------------------------

# LOOPING IN **R** 

Very frequently, a computation involves iterating some procedure across a range of cases, and every computer language I've ever come across has one or more facilities for producing such *loops*.
**R** is no exception, though judging by their code, many **R** programmers look down their noses at loops.
The fact remains that, at some point in life, one simply has to write a `for` loop.
Here, we'll look at the looping constructs available in **R**.

## `for` loops

Execute the following code.

```r
phi <- 1
for (k in 1:100) {
  phi <- 1+1/phi
  print(c(k,phi))
}
```
What does it do?
Sequentially, for each value of `k` between 1 and 100, `phi` is modified.
More specifically, at the beginning of the `for` loop, a vector containing all the integers from 1 to 100 in order is created.
Then, `k` is set to the first element in that vector, i.e., 1.
Then the **R** expression from the `{` to the `}` is evaluated.
When that expression has been evaluated, `k` is set to the next value in the vector.
The process is repeated until, at the last evaluation, `k` has value 100.

As an aside, note that the final value of `phi` is the Golden Ratio, 1.618034.


As an example of a situation where a loop of some sort is really needed, suppose we wish to iterate the Beverton-Holt map (one of the simplest discrete-time models of population growth),
\begin{equation*}
N_{t+1} = \frac{a\,N_t}{1+b\,N_t}.
\end{equation*}
We simply have no option but to do the calculation one step at a time.
Here's an **R** code that does this

```r
a <- 1.1
b <- 0.001
T <- seq(from=1,to=200,by=1)
N <- numeric(length(T))
n <- 2
for (t in T) {
  n <- a*n/(1+b*n)
  N[t] <- n
}
```
Spend some time to make sure you understand what happens at each line of the above.
We can plot the population sizes $N_t$ through time via

```r
plot(T,N)
```

![](Lecture-4-R-syntax--refresher-_files/figure-html/unnamed-chunk-71-1.png)<!-- -->

##### Gotcha:
An alternative way to do the above might be something like

```r
N <- numeric(length(T))
for (t in 1:length(T)) {
  n <- a*n/(1+b*n)
  N[t] <- n
}
```

-----------------------------


## `while` loops

A second looping construct is the `while` loop.
Using `while`, we can compute the Golden Ratio as before:

```r
phi <- 20
k <- 1
while (k <= 100) {
  phi <- 1+1/phi
  print(c(k,phi))
  k <- k+1
}
```
What's going on here?
First, `phi` and `k` are initialized.
Then the `while` loop is started.
At each iteration of the loop, `phi` is modified, and intermediate results printed, as before.
In addition, `k` is incremented.
The `while` loop continues to iterate until the condition `k <= 100` is no longer `TRUE`, at which point, the `while` loop terminates.

Note that here we've chosen a large number (100) of iterations.
Perhaps we could get by with fewer.
If we wanted to terminate the iterations as soon as the value of `phi` stopped changing, we could do:

```r
phi <- 20
conv <- FALSE
while (!conv) {
  phi.new <- 1+1/phi
  conv <- phi==phi.new
  phi <- phi.new
}
```


Another way to accomplish this would be to use `break` to stop the iteration when a condition is met.
For example

```r
phi <- 20
while (TRUE) {
  phi.new <- 1+1/phi
  if (phi==phi.new) break
  phi <- phi.new
}
```
While this `while` loop is equivalent to the one before, it does have the drawback that, if the `break` condition is never met, the loop will go on indefinitely.
An alternative that avoids this is to use a `for` loop with a large (but finite) number of iterations, together with `break`:

```r
phi <- 3
for (k in seq_len(1000)) {
  phi.new <- 1+1/phi
  if (phi==phi.new) break
  phi <- phi.new
}
```


## `repeat` loops

A third looping construct in **R** involves the `repeat` keyword.
For example,

```r
phi <- 12
repeat {
  phi.new <- 1/(1+phi)
  if (phi==phi.new) break
  phi <- phi.new
}
```

In addition, **R** provides the `next` keyword, which, like `break`, is used in the body of a looping construct.
Rather than terminating the iteration, however, it aborts the current iteration and leads to the immediate execution of the next iteration.

For more information on looping and other control-flow constructs, execute `?Control`.


# FUNCTIONS AND ENVIRONMENTS

## Definitions and examples

An extremely useful feature in **R** is the ability to write arbitrary *functions*.
A function, in this context, is an algorithm that performs a specific computation that depends on inputs (the function's *arguments*) and produces some output (the function's *value*) and/or has some *side effects*.
Let's see how this is done.

Here is a function that squares a number.

```r
sq <- function (x) x^2
```
The syntax is `function (arglist) expr`.
The one argument in this case is `x`.
When a particular value of `x` is supplied, **R** performs the squaring operation.
The function then *returns* the value `x^2`:

```r
sq(3); sq(9); sq(-2);
```

```
## [1] 9
```

```
## [1] 81
```

```
## [1] 4
```

Here is a function with two arguments and a more complex *body*, as we call the expression that is evaluated when the function is called.

```r
f <- function (x, y = 3) {
  a <- sq(x)
  a+y
}
```
Here, the body is the **R** expression from `{` to `}`. 
Unless the `return` codeword is used elsewhere in the function body, the value returned is always the last expression evaluated.
Thus:

```r
f(3,0); f(2,2); f(3);
```

```
## [1] 9
```

```
## [1] 6
```

```
## [1] 12
```
Note that in the last case, only one argument was supplied.
In this case, `y` assumed its default value, 3.

Note that functions need not be assigned to symbols; they can be *anonymous*:

```r
function (x) x^5
```

```
## function (x) x^5
```

```r
(function (x) x^5)(2)
```

```
## [1] 32
```

A function can also have side effects, e.g.,

```r
hat <- "hat"
hattrick <- function (y) {
  hat <<- "rabbit"
  2*y
}
hat; hattrick(5); hat
```

```
## [1] "hat"
```

```
## [1] 10
```

```
## [1] "rabbit"
```
However, the very idea of a function insists that we should never experience *unintentional* side effects.
We'll see how **R** realizes this imperative below.

##### An aside 

If we want the function not to automatically print, we can wrap the return value in `invisible()`:

```r
hattrick <- function (y) {
  hat <<- "rabbit"
  invisible(2*y)
}
hattrick(5)
print(hattrick(5))
```

```
## [1] 10
```

A function in **R** is defined by three components:

- its *formal parameters*, i.e., its argument list,
- its body, and
- its *environment*, i.e., the context in which the function was defined.

**R** provides simple functions to interrogate these function components:

```r
formals(hattrick)
```

```
## $y
```

```r
body(hattrick)
```

```
## {
##     hat <<- "rabbit"
##     invisible(2 * y)
## }
```

```r
environment(hattrick)
```

```
## <environment: R_GlobalEnv>
```

## Function scope

As noted above, a paramount consideration in the implementation of functions in any programming language is that unintentional side effects should never occur.
In particular, I should be free to write a function that creates temporary variables as an aid to its computations, and be able to rest assured that no variables I create temporarily will interfere with any other variables I've defined anywhere else.
To accomplish this, **R** has a specific set of *scoping rules*.

Consider the function


```r
f <- function (x) {
  y <- 2*x
  print(x)
  print(y)
  print(z)
}
```

In this function's body, `x` is a formal parameter, `y` is a local variable, and `z` is a free, or *unbound* variable.
When `f` is evaluated, each of these variables must be *bound* to some value.
In **R** , the free variable bindings are resolved---each time the function is evaluated---by first looking in the environment where the function was created.
This is called *lexical scope*.
Thus if we execute



```r
f(3)
```
we get an error, because no object named `z` can be found.
If, however, we do

```r
z <- 10
f(3)
```

```
## [1] 3
## [1] 6
## [1] 10
```
we don't get an error, because `z` is defined in the environment, `<environment: R_GlobalEnv>`, of `f`.
Similarly, when we do

```r
z <- 13
g <- function (x) {
  2*x+z
}
f <- function (x) {
  z <- -100
  g(x)
}
f(5)  
```

```
## [1] 23
```
The relevant value of `z` is the one in the environment where `g` was *defined*, not the one in the environment wherein it is *called*.

## Nested functions and environments

In each of the following examples, make sure you understand exactly what has happened.

Consider this:

```r
y <- 11
f <- function (x) {
  y <- 2*x
  y+x
}
f(1); y
```

```
## [1] 3
```

```
## [1] 11
```

As mentioned above, each function is associated with an environment: the environment within which it was defined.
When a function is evaluated, a new temporary environment is created, within which the function's calculations are performed.
Every new environment has a parent, the environment wherein it was created.
The parent of this new environment is the function's environment.
To see this, try


```r
f <- function () {
  g <- function () {
    h <- function () {
      cat("inside function h:\n")
      cat("current env: ")
      print(environment())
      cat("parent env: ")
      print(parent.frame(1))
      cat("grandparent env: ")
      print(parent.frame(2))
      cat("great-grandparent env: ")
      print(parent.frame(3))
      invisible(NULL)
    }
    cat("inside function g:\n")
    cat("environment of h: ")
    print(environment(h))
    cat("current env: ")
    print(environment())
    cat("parent env: ")
    print(parent.frame(1))
    cat("grandparent env: ")
    print(parent.frame(2))
    h()
  }
  cat("inside function f:\n")
  cat("environment of g: ")
  print(environment(g))
  cat("current env: ")
  print(environment())
  cat("parent env: ")
  print(parent.frame(1))
  g()
}
cat("environment of f: "); print(environment(f))
cat("global env: "); print(environment())
f()
```

```
## environment of f: <environment: R_GlobalEnv>
## global env: <environment: R_GlobalEnv>
## inside function f:
## environment of g: <environment: 0x00000000390b3bd0>
## current env: <environment: 0x00000000390b3bd0>
## parent env: <environment: R_GlobalEnv>
## inside function g:
## environment of h: <environment: 0x000000001ca94520>
## current env: <environment: 0x000000001ca94520>
## parent env: <environment: 0x00000000390b3bd0>
## grandparent env: <environment: R_GlobalEnv>
## inside function h:
## current env: <environment: 0x000000001ca8da20>
## parent env: <environment: 0x000000001ca94520>
## grandparent env: <environment: 0x00000000390b3bd0>
## great-grandparent env: <environment: R_GlobalEnv>
```

Each variable referenced in the function's body is bound, first, to a formal argument if possible.
If a local variable of that name has previously been created (via one of the assignment operators `<-`, `->`, or `=`, this is the variable that is affected by any subsequent assignments.
If the variable is neither a formal parameter nor a local variable, then the parent environment of the function is searched for that variable.
If the variable has not been found in the parent environment, then the grand-parent environment is searched, and so on.

If the assignment operators `<<-` or `->>` are used, a more extensive search for the referenced assignee is made.
If the variable does not exist in the local environment, the parent environment is searched.
If it does not exist in the parent environment, then the grand-parent environment is searched, and so on.
Finally, if the variable cannot be found anywhere along the lineage of environments, a new global variable is created, with the assigned value.


# THE `apply` FAMILY OF FUNCTIONS

As mentioned above, there are circumstances under which looping constructs are really necessary.
Very often, however, we wish to perform some operation across all the elements of a vector, array, or dataset.
In such cases, it is faster and more elegant (to the **R** afficiando's eye) to use the `apply` family of functions.

## List apply: `lapply`

`lapply` applies a function to each element of a list or vector, returning a list.

```r
x <- list("teenage","mutant","ninja","turtle",
          "hamster","plumber","pickle","baby")
lapply(x,nchar)
```

```
## [[1]]
## [1] 7
## 
## [[2]]
## [1] 6
## 
## [[3]]
## [1] 5
## 
## [[4]]
## [1] 6
## 
## [[5]]
## [1] 7
## 
## [[6]]
## [1] 7
## 
## [[7]]
## [1] 6
## 
## [[8]]
## [1] 4
```

```r
y <- c("teenage","mutant","ninja","turtle",
       "hamster","plumber","pickle","baby")
lapply(y,nchar)
```

```
## [[1]]
## [1] 7
## 
## [[2]]
## [1] 6
## 
## [[3]]
## [1] 5
## 
## [[4]]
## [1] 6
## 
## [[5]]
## [1] 7
## 
## [[6]]
## [1] 7
## 
## [[7]]
## [1] 6
## 
## [[8]]
## [1] 4
```

## Sloppy list apply: `sapply`

`sapply` isn't content to always return a list:
it attempts to simplify the results into a non-list vector if possible.

```r
x <- list("pizza","monster","jigsaw","puddle",
          "hamster","plumber","pickle","baby")
sapply(x,nchar)
```

```
## [1] 5 7 6 6 7 7 6 4
```

```r
y <- c("pizza","monster","jigsaw","puddle",
       "hamster","plumber","pickle","baby")
sapply(y,nchar)
```

```
##   pizza monster  jigsaw  puddle hamster plumber  pickle    baby 
##       5       7       6       6       7       7       6       4
```

## Multiple-list apply: `mapply`

`mapply` is a multiple-argument version of `sapply`:

```r
x <- c("pizza","monster","jigsaw","puddle")
y <- c("cowboy","barbie","slumber","party")
mapply(paste,x,y,sep="/")
```

```
##            pizza          monster           jigsaw           puddle 
##   "pizza/cowboy" "monster/barbie" "jigsaw/slumber"   "puddle/party"
```
As usual, the recycling rule applies:

```r
mapply(paste,x,y[2:3])
```

```
##             pizza           monster            jigsaw            puddle 
##    "pizza barbie" "monster slumber"   "jigsaw barbie"  "puddle slumber"
```

```r
mapply(paste,x[c(1,3)],y)
```

```
##           pizza          jigsaw            <NA>            <NA> 
##  "pizza cowboy" "jigsaw barbie" "pizza slumber"  "jigsaw party"
```

## Array apply: `apply`

`apply` is very powerful and a bit more complex.
It allows an arbitrary function to applied to each slice of an array, where the slices can be defined in all possible ways.
Let's create a matrix:

```r
A <- array(data=seq_len(15),dim=c(3,5)); A
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    4    7   10   13
## [2,]    2    5    8   11   14
## [3,]    3    6    9   12   15
```
To apply an operation to each row, we *marginalize* over the first dimension (rows).
For example, to sum the rows, we'd do

```r
apply(A,1,sum)
```

```
## [1] 35 40 45
```
To sum the columns (the second dimension), we'd do

```r
apply(A,2,sum)
```

```
## [1]  6 15 24 33 42
```

Now suppose we have a 3-dimensional array:

```r
A <- array(data=seq_len(30),dim=c(3,5,2)); A
```

```
## , , 1
## 
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    4    7   10   13
## [2,]    2    5    8   11   14
## [3,]    3    6    9   12   15
## 
## , , 2
## 
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   16   19   22   25   28
## [2,]   17   20   23   26   29
## [3,]   18   21   24   27   30
```
To sum the rows within each slice, we'd do

```r
apply(A,c(1,3),sum)
```

```
##      [,1] [,2]
## [1,]   35  110
## [2,]   40  115
## [3,]   45  120
```
while to sum the slices, we'd do

```r
apply(A,3,sum)
```

```
## [1] 120 345
```
For each of the above, make sure you understand exactly what has happened.

Of course, we can apply an anonymous function wherever we apply a named function:

```r
apply(A,c(2,3),function (x) sd(x)/sqrt(length(x)))
```

```
##           [,1]      [,2]
## [1,] 0.5773503 0.5773503
## [2,] 0.5773503 0.5773503
## [3,] 0.5773503 0.5773503
## [4,] 0.5773503 0.5773503
## [5,] 0.5773503 0.5773503
```
If any additional arguments are given to `apply`, these are passed to the function.
For example, make sure you understand what is happening in the lines:

```r
apply(A,c(1,2),function (x, y) sum(x>y),y=8)
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    1    1    2    2
## [2,]    1    1    1    2    2
## [3,]    1    1    2    2    2
```

```r
apply(A,c(1,2),function (x, y) sum(x>y),y=-1)
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    2    2    2    2    2
## [2,]    2    2    2    2    2
## [3,]    2    2    2    2    2
```

## Table apply: `tapply`

`tapply` is, in a way, an extension of `table`.
The syntax is `tapply(X,INDEX,FUN,...)`,
where `X` is a vector, `INDEX` is a list of one or more factors, each the same length as `X`, and `FUN` is a function.
The vector `X` will be split into subvectors according to `INDEX`, and `FUN` will be applied to each of the subvectors.
By default, the result is simplified into an array if possible.
Some examples:

```r
x <- seq(1,30,by=1)
b <- rep(letters[1:10],times=3)
data.frame(x,b)
```

```
##     x b
## 1   1 a
## 2   2 b
## 3   3 c
## 4   4 d
## 5   5 e
## 6   6 f
## 7   7 g
## 8   8 h
## 9   9 i
## 10 10 j
## 11 11 a
## 12 12 b
## 13 13 c
## 14 14 d
## 15 15 e
## 16 16 f
## 17 17 g
## 18 18 h
## 19 19 i
## 20 20 j
## 21 21 a
## 22 22 b
## 23 23 c
## 24 24 d
## 25 25 e
## 26 26 f
## 27 27 g
## 28 28 h
## 29 29 i
## 30 30 j
```

```r
tapply(x,b,sum)
```

```
##  a  b  c  d  e  f  g  h  i  j 
## 33 36 39 42 45 48 51 54 57 60
```

```r
b <- rep(letters[1:10],each=3)
data.frame(x,b)
```

```
##     x b
## 1   1 a
## 2   2 a
## 3   3 a
## 4   4 b
## 5   5 b
## 6   6 b
## 7   7 c
## 8   8 c
## 9   9 c
## 10 10 d
## 11 11 d
## 12 12 d
## 13 13 e
## 14 14 e
## 15 15 e
## 16 16 f
## 17 17 f
## 18 18 f
## 19 19 g
## 20 20 g
## 21 21 g
## 22 22 h
## 23 23 h
## 24 24 h
## 25 25 i
## 26 26 i
## 27 27 i
## 28 28 j
## 29 29 j
## 30 30 j
```

```r
tapply(x,b,sum)
```

```
##  a  b  c  d  e  f  g  h  i  j 
##  6 15 24 33 42 51 60 69 78 87
```


```r
datafile <- "http://kingaa.github.io/R_Tutorial/seedpred.dat"
seeds <- read.table(datafile,header=TRUE,
                    colClasses=c(station='factor',dist='factor',date='Date'))
x <- subset(seeds,available>0)
with(x, tapply(tcum,list(dist,station),max,na.rm=TRUE))
```

```
##     1  10 100 101 102 103 104 105 106 107 108 109 11 110 111 112 113 114 115
## 10 24 248 122 248  10  17  39  17  46  10   3  17 NA  10  10  17 248 248  67
## 25 18 123  11 249 249  11  11 249  81  11 123 249  4  25 249  47 249 249  40
##    116 117 118 119  12 120 121 122 123 124 125 126 127 128 129  13 130 131 132
## 10 248 248  24  10 248  17 248 248   3 248 248  10  67  17 248 248 248   3 248
## 25  68  11  32  18 249  32  11 249  68 249 249  18  NA  18 249 249  18  18  40
##    133 134 135 136 137 138 139 14 140 141 142 143 144 145 146 147 148 149 15
## 10 248  10  10   3 248 248   3 24 248 248 248 248 248 248 248  24 248 248  3
## 25  54  18   4  40  61 249   4 61 249  68  11  NA   4 249 209   4 249  81 11
##    150 151 152 153 154 155 156 157 158 159  16 160  17  18  19   2  20  21 22
## 10 248  39 248 248 248  NA  53 248 248  53 248  NA 248 136  24 248 248 248 24
## 25 249 249 249 249 249 249 123 249 249  32  25 249   4 249 123  47  54 249 61
##    23  24  25  26 27  28  29  3 30 31  32 33 34  35 36 37 38  39   4  40  41
## 10 24 248  24 248 53  31  80 31 NA NA 248 17 24 248 39 17  3  10  31 248 122
## 25 40 249 249 249  4 249 249 11  4 NA  18  4  4 159  4 11 NA 249 249   4  NA
##     42 43 44  45 46 47 48 49   5 50  51 52 53  54 55 56 57  58 59  6 60  61 62
## 10 248 NA  3  17 NA NA NA NA 248  3 248  3 10 248  3  3 24   3 NA 10  3   3  3
## 25 123 NA  4 249 11 18  4 11 249 40  18 11 11 249  4 18 11 159 25 11 11 209  4
##    63 64  65  66 67 68 69  7 70 71 72  73  74 75  76 77 78 79   8 80  81 82 83
## 10  3  3 248 248 NA 10 17 10  3 NA 10 248 248 NA 248  3 10 NA  NA  3 248 80 NA
## 25  4 11 249  11 40 11 61 25  4  4  4 249 159 32 249 47 18  4 249 11  NA 11 11
##     84  85 86 87 88 89   9 90 91 92 93 94 95 96 97 98 99
## 10 241 122 24 10 10 24 115 17 NA 17 10  3  3 10 10 10 17
## 25  11 249 11 NA 11 11 123 11 11 25 11  4 11 18 18 11  4
```

## sapply with expected result: `vapply`

When we could use `sapply` and we know exactly what the size and class of the value of the function will be, it is sometimes faster to use `vapply`.
The syntax is like that of `sapply`: `vapply(X,FUN,FUN.VALUE,...)`, where `X` and `FUN` are as in `sapply`, but we specify the size and class of the value of `FUN` via the `FUN.VALUE` argument.
For example, suppose we define a function that, given a number between 1 and 26 will return the corresponding letter of the alphabet:

```r
alph <- function (x) {
  stopifnot(x >= 1 && x <= 26)
  LETTERS[as.integer(x)]
}
```
This function will return a vector of length 1 and class `character`.
To apply it to a randomly sampled set of integers, we might do



```r
x <- sample(1:26,50,replace=TRUE)
y <- vapply(x,alph,character(1))
paste(y,collapse="")
```

```
## [1] "ORIFWKBWAMEJBZUMLKRJVEOIKQXEYRNTGAXXXUVQTZIHQGANTI"
```

# VECTORIZED FUNCTIONS vs LOOPS

The idea that one should avoid loops wherever possible in **R**, using instead vectorized functions like those in the `apply` family, is quite widespread in some quarters.
The belief, which probably dates back to infelicities in early versions of **S** and **Splus** but is remarkably persistent, is that loops are very slow in **R**.
Let's have a critical look at this.

Consider the following loop code that can be vectorized:

```r
x <- runif(n=1e6,min=0,max=2*pi)
y <- numeric(length(x))
for (k in seq_along(x)) {
  y[k] <- sin(x[k])
}
```
To time this, we can wrap the vectorizable parts in a call to `system.time`:

```r
x <- runif(n=1e6,min=0,max=2*pi)
system.time({
  y <- numeric(length(x))
  for (k in seq_along(x)) {
    y[k] <- sin(x[k])
  }
})
```

```
##    user  system elapsed 
##    0.13    0.00    0.13
```
We can compare this with a simple call to `sin` (which is vectorized):

```r
system.time(z <- sin(x))
```

```
##    user  system elapsed 
##    0.03    0.00    0.03
```
Clearly, calling `sin` directly is much faster.
What about using `sapply`?





The above example is very simple in that there is a builtin function (`sin` in this case) which is capable of the fully vectorized computation.
In such a case, it is clearly preferable to use it.
Frequently, however, no such builtin function exists, i.e., we have a custom function of our own we want to apply to a set of data.
Let's compare the relative speeds of loops and `sapply` in this case.

```r
x <- seq.int(from=20,to=1e6,by=10)
f <- function (x) {
  (((x+1)*x+1)*x+1)*x+1
}
system.time({
  res1 <- numeric(length(x))
  for (k in seq_along(x)) {
    res1[k] <- f(x[k])
  }
})
```

```
##    user  system elapsed 
##    0.07    0.00    0.08
```

```r
system.time(res2 <- sapply(x,f))
```

```
##    user  system elapsed 
##    0.09    0.00    0.10
```
[Actually, in this case, `f` is vectorized automatically.
Why is this?]

```r
system.time(f(x))
```

```
##    user  system elapsed 
##       0       0       0
```

Another example: in this case function `g` is not vectorized.

```r
g <- function (x) {
  if ((x[1] > 30) && (x[1] < 5000)) 1 else 0
}
system.time({
  res1 <- numeric(length(x))
  for (k in seq_along(x)) {
    res1[k] <- g(x[k])
  }
})
```

```
##    user  system elapsed 
##    0.07    0.00    0.06
```

```r
system.time(res2 <- sapply(x,g))
```

```
##    user  system elapsed 
##    0.10    0.00    0.09
```



# REPRODUCIBLE RESEARCH

One of the major advantages of R is that there are a number of extensions that allow for it to interface with other languages. One of these extension involves the use of R Markdown files that weave together narrative text and code to produce elegantly formatted output. This would also facilitate reproducible research, where you can write your entire paper, book, dashboard or slides within the RStudio environment. Two of the important packages that allow you to do this are rmarkdown and knitr, where the knitr package may be used to knit the RMarkdown documents to create HTML, PDF, MS Word, LaTeX Beamer presentations, HTML5 slides, Tufte-style handouts, books, dashboards, shiny applications, scientific articles, websites, and many more. For more on the use of rmarkdown and knitr, see [Xie (2013)](https://bookdown.org/yihui/rmarkdown-cookbook/), and the [website](http://rmarkdown.rstudio.com/.)

The original idea behind these forms of reproducible research programs is that the text for your report (or article) would be written in standard LaTeX format and the model would be included as a R chunk of code. This file could then be compiled within R to generate a .md (which is useful for web documents), .pdf or .tex file. An example of a basic R *Markdown** document is included under separate file and is named T1_Markdown_Paper.Rmd.

Of course, these RMarkdown documents can also be used to generate HTML output and the growth of this part of the ecosystem allows for the creation of relatively complex documents and dashboards with the aid of various R extensions. For example, more complex documents, such as books, could be written with the bookdown package, which is what I’ve used for your notes. And with the aid of Shiny one is able to create impressive interactive dashboards. For more on this [see](https://shiny.rstudio.com/).

Again, there is a useful cheatsheet that you should take a look at, which can be freely [downloaded](https://github.com/rstudio/cheatsheets/raw/master/rmarkdown-2.0.pdf).

# SPECIFIC SUBJECT AREAS

Specific modelling aspects are usually covered within a dedicated package. For a list of the major [econometric](http://cran.r-project.org/web/views/Econometrics.html) type of packages, including those that have been developed for cross sectional analysis, panel models, [machine learning](https://cran.r-project.org/web/views/MachineLearning.html), [time series](https://cran.r-project.org/web/views/TimeSeries.html), etc.


Remember also that the total number of packages on the CRAN repository represent only a small fraction of those that are available. So if you don’t find what you like on this particular repository then it may be available elsewhere, possibly on a GitHub or GitLab repository, you just need to search.
4.2 Obtaining assistance

A further useful resource is the Journal of Statistical Software, which contains journal articles that seek to explain how to use many of the popular R packages. These articles are usually similar to the vignettes that accompany most packages that describe the desired functionality.

In addition, if you are struggling with a particular function in R, then you may want to check out the StackOverFlow website, which was originally developed for computer programmers (many of which make use of R for data science applications). The probability that your query has been previously answered is usually quite high and what is particularly nice about this website is that it has a number of methods that avoid the duplication of queries. You can check it [out](http://stackoverflow.com/).

# CLOUD BASED APPLICATIONS R

For those of you who are looking to make use of a cloud based platform to deploy an R application, then the [website](https://cloudyr.github.io/) provides a useful list of packages that make it easier to work with the various services that are provided by AWS, GCP, or Azure. In addition, there is also a particularly helpful Docker container called Rocker. Further information is also [available](https://www.rocker-project.org/).

# DEVELOPING WITH  GitLab, BitBucket or GitHub

GitLab, BitBucket and GitHub are web-based repositories that allow for the source code management (SCM) functionality of Git as well as many other features. It can be used to track all the changes to programming files by any permitted user, so that you can see who may have made a change to a particular file. In addition, if it turns out that a particular branch of code did not provide the intended result, then you can always go back to a previous version of the code base. Most R developers make use of either GitLab or GitHub for the development of their packages and it is particularly useful when you have many developers working on a single project.

For your own personal work you should consider opening a GitLab account, which will allow you to store public and private repositories for free. As an alternative, you can make use of GitHub for a free public repository, or BitBucket, which provides users with free private repositories.

After installing the Rtools software one is also able to download and install packages that are located on GitLab, BitBucket or GitHub accounts, directly from within R with the aid of the devtools package. For example, the following code would find the package on GitHub, provided you are connected to the internet:

Then lastly, if you are developing an R package or want to create a your own repository then you will want to create a R-project in RStudio so that you can control the uploads and downloads to your Git repository from within RStudio. For a relatively comprehensive tutorial on how to set this up, [see.](https://happygitwithr.com/)

# WORKING WITH DATABASES

If you need to connect to a particular database, then the [website](https://db.rstudio.com/) is a wonderful resource, which describes how one is able to work with a large variety of different databases to import or export data into R.

5 TIDYVERSE

For those who would like to invest more time on programming in R, then you should take a look at the work of Hadley Wickham and the tidyverse suite of packages, which are used for various data science applications. His website may be found at http://hadley.nz/ and details about the tidyverse can be found at: https://www.tidyverse.org/. The books R for data science and Advanced R are quintessential aids for venturing into this area and may be found at https://r4ds.had.co.nz/ and http://adv-r.had.co.nz/, respectively. For a brief summary of the main features of the tidyverse, you may want to consider reading the booklet by Locke (2017a), which is available for download at: https://itsalocke.com/files/DataManipulationinR.pdf. During what remains of this semester we will make use of a few tidyverse functions, although the objective of this course is not to provide an exhaustive overview of these functions.

Since we will be using a few dplyr and ggplot functions throughout the course, you may want to take a look at the following cheatsheets:

    https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf
    https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf

In addition, to the suite of tidyverse packages, which assist with a number of basic data science tasks, there is a collection of packages that seek to apply tidyverse principles to modelling and machine learning tools. These form part of the tidymodels ecosystem, which offers a consistent, flexible framework for the storing of model output, etc. You can get more information about this ecosystem from the website https://www.tidymodels.org/. A pre-publication draft of a book that describes the use of this new ecosystem is available at https://www.tmwr.org/.

The data.table package is also one that is growing in popularity and can be used in conjunction with tidyverse functions. In many respects it provides equivalent functionality to what is contained in the dplyr package, which is part of the tidyverse, but makes use of more efficient ways to manipulate data. When working with large data this is particularly important. I have not made use of this package in this course as we are not going to be dealing with particularly large datasets, and the data.table syntax takes a little longer to learn (in my opinion). For a useful cheatsheet that lists dplyr and data.table equivalent functions next to each other, take a look at: https://atrebas.github.io/post/2019-03-03-datatable-dplyr/. In addition, the vignettes that accompany the data.table are excellent.

And if you start working with extreme big-data then you probably want to make use Spark, for which the sparklyr package provides an accessible interface that is simialr to dplyr. Details regarding this package is available at https://spark.rstudio.com/ and in the ebook https://therinspark.com/.
Working with databases

If you need to connect to a particular database, then the https://db.rstudio.com/ website is a wonderful resource, which describes how one is able to work with a large variety of different databases to import or export data into R.
5 Tidyverse

For those who would like to invest more time on programming in R, then you should take a look at the work of Hadley Wickham and the tidyverse suite of packages, which are used for various data science applications. His website may be found at http://hadley.nz/ and details about the tidyverse can be found at: https://www.tidyverse.org/. The books R for data science and Advanced R are quintessential aids for venturing into this area and may be found at https://r4ds.had.co.nz/ and http://adv-r.had.co.nz/, respectively. For a brief summary of the main features of the tidyverse, you may want to consider reading the booklet by Locke (2017a), which is available for download at: https://itsalocke.com/files/DataManipulationinR.pdf. During what remains of this semester we will make use of a few tidyverse functions, although the objective of this course is not to provide an exhaustive overview of these functions.

Since we will be using a few dplyr and ggplot functions throughout the course, you may want to take a look at the following cheatsheets:

    https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf
    https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf

In addition, to the suite of tidyverse packages, which assist with a number of basic data science tasks, there is a collection of packages that seek to apply tidyverse principles to modelling and machine learning tools. These form part of the tidymodels ecosystem, which offers a consistent, flexible framework for the storing of model output, etc. You can get more information about this ecosystem from the website https://www.tidymodels.org/. A pre-publication draft of a book that describes the use of this new ecosystem is available at https://www.tmwr.org/.

The data.table package is also one that is growing in popularity and can be used in conjunction with tidyverse functions. In many respects it provides equivalent functionality to what is contained in the dplyr package, which is part of the tidyverse, but makes use of more efficient ways to manipulate data. When working with large data this is particularly important. I have not made use of this package in this course as we are not going to be dealing with particularly large datasets, and the data.table syntax takes a little longer to learn (in my opinion). For a useful cheatsheet that lists dplyr and data.table equivalent functions next to each other, take a look at: https://atrebas.github.io/post/2019-03-03-datatable-dplyr/. In addition, the vignettes that accompany the data.table are excellent.

And if you start working with extreme big-data then you probably want to make use Spark, for which the sparklyr package provides an accessible interface that is simialr to dplyr. Details regarding this package is available at https://spark.rstudio.com/ and in the ebook https://therinspark.com/.

# RESOURCES

There are potentially more resources about learning various features of R than one could read in a lifetime. Therefore, what follows is a subjective view of selected resources that I have found helpful.

If you are new to R and would like a comprehensive introduction, then I would suggest that you may want to go through Grolemund (2020), which may be read in the form of an [ebook](https://rstudio-education.github.io/hopr/).

Alternatively, you may find that Locke (2017b) provides a reassuring introduction to the modern R environment. A PDF version of the book can be downloaded from the authors [website](https://itsalocke.com/files/workingwithr.pdf).

If you find that neither of these are helpful then you can look at CRAN’s [link](https://cran.r-project.org/other-docs.html) to Contributed Documentation.

Or alternatively, you may find an alternative ebook that has been written from within R at the sites [I](https://bookdown.org/) and [II](https://bookdown.org/home/archive/).

If you prefer to learn through online lectures, then there are also a number of [online courses](https://www.datacamp.com/courses/free-introduction-to-r) that provide a great introduction.

Other options include learning by doing, where you can get to grips with the functionality of R from within R, with the aid of the [swirl package](http://swirlstats.com/). 

For those students who would like to implement the models from Wooldridge (2020), which is titled “Introductory Econometrics: A Modern Approach,” with R code then you can make use of the free [ebook](http://www.urfie.net/).

The academic publisher Springer has released a relatively inexpensive and focused series of books, titled [Use R!](http://www.springer.com/series/6991?detailsPage=titles), which discuss the use of R in a various subject areas. The series now includes over seventy titles and they all contain source code. 

The publisher CRC Press (which is part of the Taylor & Francis Group) have a [similar series of books](https://www.crcpress.com/Chapman--HallCRC-The-R-Series/book-series/CRCTHERSER). 
Both of these series include introductory texts.

https://kevin-kotze.gitlab.io/tsm/ts-1-tut/

Thank you for your attention!


