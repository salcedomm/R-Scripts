# Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
fact <- function(x) {
  y <- 1
  for (i in 1:x)  {
  y <- y*((1:x)[i])
  }
  print(y)
}
fact(1)
fact(10)
fact(18)



# Create a function to compute for your net pay at work.
#Assuming that gross pay per month is more than 15750.
#Input Gross Pay
Monthly_NetPay <- function(s) {
  sss <- 581.30
  pagibig <- 100
  philhealth <- ((0.0275 * s)/2)
  taxable_income <- s - (sss + philhealth + pagibig)
  income_tax <- taxable_income
  if (taxable_income > 0 & taxable_income <= 20833) {
    income_tax <- 0
  } else if (taxable_income > 20833 & taxable_income <= 33332) {
    income_tax <- (taxable_income - 20833) * .20
  } else if (taxable_income > 33333 & taxable_income <= 66666) {
    income_tax <- 2500 + (taxable_income - 33333) * .25
  } else if (taxable_income > 66667 & taxable_income <= 166666) {
    income_tax <- 10833.33 + (taxable_income - 66667) * .30
  } else if (taxable_income > 66667 & taxable_income <= 166666) {
    income_tax <- 40833.33 + (taxable_income - 166667) * .32
  } else if (taxable_income > 666667) {
    income_tax <- 200833.33 + (taxable_income - 666667) * .35
  }
  print(taxable_income - income_tax)
}

Monthly_NetPay(30000)
Monthly_NetPay(50000)




# Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
compound_interest <- function() {
  p <- readline(prompt="Enter principal: ")
  r <- readline(prompt="Enter annual rate of interest (in decimal form): ")
  t <- readline(prompt="Enter time (in years): ")

  principal <- as.numeric(p)
  rate <- as.numeric(r)
  time <- as.numeric(t)

  print (principal * ((1+rate)^time)-principal)
}

compound_interest()




# Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.
isPrime <- function(number) {
  if (number == 2) {
    TRUE
  } else if (any(number %% 2:(number-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

isPrime(2)
isPrime(13)
isPrime(237)


#Define an R function that removes NA values from a vector
remove_na <- function (r) {
  r <- r[!is.na(r)]
return(r)
}

remove_na(c(1,10,NA,5,234,NA))

