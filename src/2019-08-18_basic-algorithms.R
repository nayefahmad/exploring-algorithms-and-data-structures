

# Fibonacci sequence: ------------------------------
fib <- function(n){
    
    if (n == 0 | n == 1){
        return(1)
        
    } else {
        return(fib(n-1) + fib(n-2))
    }
}

# test: 
test_vector <- 1:10
sapply(test_vector, fib)


# Iterative binary search ------------------------------
binary <- function(A, n, x) {
    p <- 1
    r <- n
    
    while (p <= r) {
        q <- (p +r) %/% 2  # floor division 
        
        if (A[q] == x) {  # base case 1
            return(q)
            
        } else if (A[q] > x) {
            r <- q - 1
            
        } else if (A[q] < x) {
            p <- q + 1
        }
        
    }
    
    return("not found")
}


# test: 
size <- 1000
A <- c(rnorm(size), 1234)
binary(A, size + 1, 1234)



# Recursive binary search ------------------------------
r_binary <- function(A, p, r, x) {
    # A is a vector (array)
    # p and r are indexes into the vector 
    # x is the value we are searching for 
    
    # output: index of the point where the result is located 
    
    if (p > r) {  # base case 1
        return("not found")
        
    } else {
        q <- (p + r) %/% 2  # floor division
        
        if (A[q] == x) {  # base case 2
            return(q)
            
        } else if (A[q] > x) {
            return(r_binary(A, p, q-1, x))
            
        } else if (A[q] < x) {
            return(r_binary(A, q+1, r, x))
            
        }
    }
    
}

# test: 
size <- 100000
A <- c(rnorm(size), 1234)
r_binary(A, 1, size + 1, 1234)



# Greatest common divisor: while loop ------------------------------
gcd <- function(a, b) {
    while (b != 0) {
        temp <- b
        b <- a %% b
        a <- temp
    }
    
    return(a)
    
}


# test: 
gcd(12, 68) == 4
gcd(12, 68) != 6

gcd(12, 54) == 6



# Greatest common divisor: recursion ------------------------------
gcd_recur <- function(a, b) {
    if (b == 0) {
        return(a)
    } else {
        return(gcd(b, a %% b))
    }
}

# test: 
gcd_recur(12, 68) == 4
gcd_recur(12, 68) != 5
gcd_recur(12, 54) == 6


