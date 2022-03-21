---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 4: R syntax (refresher)"
author: "Luka Sikic, PhD"
date: "Faculty of Croatian Studies | [LSMA](https://lusiki.github.io/Learning-Social-Media-Analytics/)" #"21 ožujak 2022"
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

**R** also has many **built-in mathematical functions** that operate on variables (see below). 





















