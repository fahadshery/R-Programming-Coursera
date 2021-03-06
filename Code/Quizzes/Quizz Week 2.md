Quizzes
========================================================

## Week 2 Quiz

**Question 1**

Suppose I define the following function in R


```r
cube = function(x, n) {
    x^3
}
```


What is the result of running 


```r
cube(3)
```

```
## [1] 27
```


in R after defining this function?
1. An error is returned because 'n' is not specified in the call to 'cube'
2. The number 27 is returned
3. A warning is given with no value returned.
4. The users is prompted to specify the value of 'n'.


**Answer**

2

**Question 2**

The following code will produce a warning in R. 


```r
x <- 1:10
if (x > 5) {
    x <- 0
}
```

```
## Warning: the condition has length > 1 and only the first element will be
## used
```


Why?

1. There are no elements in 'x' that are greater than 5
2. The syntax of this R expression is incorrect.
3. You cannot set 'x' to be 0 because 'x' is a vector and 0 is a scalar.
4. The expression uses curly braces.
5. 'x' is a vector of length 10 and 'if' can only test a single logical statement. 

**Answer**

5

**Question 3**

Consider the following function:


```r
f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}
```

If I then run in R:


```r
z <- 10
f(3)
```

```
## [1] 10
```


What value is returned?

1. 4
2. 7
3. 16
4. 10 

**Answer**

4


**Question 4**

Consider the following expression: 


```r
x <- 5
y <- if (x < 3) {
    NA
} else {
    10
}
```

What is the value of 'y' after evaluating this expression?

1. NA
2. 10
3. 3
4. 5 

**Answer**

2

**Question 5**

Consider the following R function:


```r
h <- function(x, y = NULL, d = 3L) {
    z <- cbind(x, d)
    if (!is.null(y)) 
        z <- z + y else z <- z + f
    g <- x + y/z
    if (d == 3L) 
        return(g)
    g <- g + 10
    g
}
```

Which symbol in the above function is a free variable?

1. f
2. z
3. d
4. L
5. g 

**Answer**

1


**Question 6**

What is an environment in R?

1. a list whose elements are all functions
2. an R package that only contains data
3. a special type of function
4. a collection of symbol/value pairs 

**Answer**

4

**Question 7**

The R language uses what type of scoping rule for resolving free variables?

1. global scoping
2. dynamic scoping
3. compilation scoping
4. lexical scoping 

**Answer**

4

**Question 8**

How are free variables in R functions resolved?

1. The values of free variables are searched for in the working directory
2. The values of free variables are searched for in the environment in which the function was defined
3. The values of free variables are searched for in the environment in which the function was called
4. The values of free variables are searched for in the global environment 

**Answer**

2

**Question 9**

What is one of the consequences of the scoping rules used in R?

1. Functions cannot be nested
2. All objects must be stored in memory
3. All objects can be stored on the disk
4. R objects cannot be larger than 100 MB 

**Answer**

2

**Question 10**

In R, what is the parent frame?

1. It is the package search list
2. It is the environment in which a function was called
3. It is always the global environment
4. It is the environment in which a function was defined 

**Answer**

2
