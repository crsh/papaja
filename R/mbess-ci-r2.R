# We ship a temporary copy of MBESS::ci.R2 here to avoid a dependency on MBESS, which has bumped it's requirements to R 4.5.1.

ci.R2 <- function(R2=NULL, df.1=NULL, df.2=NULL, conf.level=.95, Random.Predictors=TRUE, 
Random.Regressors, F.value=NULL, N=NULL, p=NULL, K, alpha.lower=NULL, alpha.upper=NULL, tol=1e-9)
{

# So that k or p can be specified.
tmp <- try(is.null(K), silent=TRUE)
if(tmp==TRUE | tmp==FALSE)
{

if(!is.null(p))
{
if(p != K) stop("Specificy 'p' or 'K', but not both (your 'p' and 'K' are different)")
}

p <- K
}


if(!missing(Random.Regressors)) Random.Predictors <- Random.Regressors

if((!is.null(N) | !is.null(p)) & (!is.null(df.1) | !is.null(df.2))) stop("Either specify \'df.1\' and \'df.2\' or \'N\' and \'p,\' but not both combinations.")

if(!is.null(N) & !is.null(p) & is.null(df.1) & is.null(df.2))
{
df.1 <- p
df.2 <- N-p-1
}

if(!is.null(df.1) & !is.null(df.2) & is.null(N) & is.null(p))
{
N <- df.1 + df.2 + 1
p <- df.1
}

if(!is.null(conf.level) & is.null(alpha.lower) & is.null(alpha.upper))
{
#if(conf.level >=1 | conf.level <= 0) stop("Your confidence level (\'conf.level\') must be between 0 and 1.")
if(!is.null(alpha.lower) | !is.null(alpha.upper)) stop("Since conf.level has been specified (which is done by default), you cannot specifiy \'alpha.lower\' and \'alpha.upper\'. If you want to specify \'alpha.lower\' or \'alpha.upper\', set \'conf.level=NULL\'")
alpha.lower <- alpha.upper <- (1-conf.level)/2
conf.level <- NULL
}

if(is.null(F.value))
{
F.value <- Rsquare2F(R2=R2, df.1=df.1, df.2=df.2, p=p, N=N)
}

if(is.null(R2))
{
R2 <- F2Rsquare(F.value=F.value, df.1=df.1, df.2=df.2)
}

if(Random.Predictors==FALSE)
{
Limits <- conf.limits.ncf(F.value=F.value, df.1=df.1, df.2=df.2, conf.level=NULL, tol=tol, alpha.lower=alpha.lower, alpha.upper=alpha.upper)

if(length(Limits)==4)
{
LL <- Lambda2Rsquare(Limits$Lower.Limit, N=N)
Prob.LL <- Limits$Prob.Less.Lower
UL <- Lambda2Rsquare(Limits$Upper.Limit, N=N)
Prob.UL <- Limits$Prob.Greater.Upper

if(is.na(Limits$Lower.Limit))
{
LL <- 0
Prob.LL <-0
}

if(is.na(Limits$Upper.Limit))
{
UL <- 1
Prob.UL <- 0
}

return(list(Lower.Conf.Limit.R2=LL, Prob.Less.Lower=Prob.LL, Upper.Conf.Limit.R2=UL, Prob.Greater.Upper=Prob.UL))
}

if(length(Limits)==2 & is.null(Limits$Upper.Limit))
{
return(list(Lower.Conf.Limit.R2=Lambda2Rsquare(Limits$Lower.Limit, N=N), Prob.Less.Lower=Limits$Prob.Less.Lower, Upper.Conf.Limit.R2=1, Prob.Greater.Upper=0))
}

if(length(Limits)==2 & is.null(Limits$Lower.Limit))
{
return(list(Lower.Conf.Limit.R2=0, Prob.Less.Lower=0, Upper.Conf.Limit.R2=Lambda2Rsquare(Limits$Upper.Limit, N=N), Prob.Greater.Upper=Limits$Prob.Greater.Upper))
}
}


if(Random.Predictors==TRUE)
{
pul <- alpha.upper
pll <- 1-alpha.lower

df1 <- N-1
df2 <- N-p-1

R2.Tilda <- R2/(1-R2)

x2 <- .999999
x1 <- .000001
x3 <- .5
diff3 <- 1

ulrhosq <- .5
llrhosq <- .5

if(pul!=0)
{
while((abs(diff3) > .00001) & (round(ulrhosq, 5) <1))
{
x3 <- (x1 + x2)/2
yy <- x3/(1-x3)
GAMMA <- sqrt(1+yy)
PHI.1 <- df1*(GAMMA^2 - 1) + p
PHI.2 <- df1*(GAMMA^4 - 1) + p
PHI.3 <- df1*(GAMMA^6 - 1) + p
g <- (PHI.2-sqrt(PHI.2^2 - PHI.1*PHI.3))/PHI.1
nu <- (PHI.2 - 2*yy*GAMMA*(sqrt(df1*df2)))/(g^2)
LAMBDA.U <- yy*GAMMA*(sqrt(df1*df2))/(g^2)
limit <- df2*R2.Tilda/(nu*g)
diff3 <- stats::pf(limit, nu, df2, ncp=LAMBDA.U) - pul
yy <- x1/(1-x1)
GAMMA <- sqrt(1+yy)
PHI.1 <- df1*(GAMMA^2 - 1) + p
PHI.2 <- df1*(GAMMA^4 - 1) + p
PHI.3 <- df1*(GAMMA^6 - 1) + p
g <- (PHI.2-sqrt(PHI.2^2 - PHI.1*PHI.3))/PHI.1
nu <- (PHI.2 - 2*yy*GAMMA*(sqrt(df1*df2)))/(g^2)
LAMBDA.U <- yy*GAMMA*(sqrt(df1*df2))/(g^2)
limit <- df2*R2.Tilda/(nu*g)
diff1 <- stats::pf(limit, nu, df2, ncp=LAMBDA.U) - pul
ifelse((diff1*diff3 < 0), (x2 <- x3), (x1 <- x3))
ulrhosq <- x3
}
}

###################################################

x2 <- .999999
x1 <- .000001
x3 <- .5

if(pll!=1)
{
yy <- x3/(1-x3)
GAMMA <- sqrt(1+yy)
PHI.1 <- df1*(GAMMA^2 - 1) + p
PHI.2 <- df1*(GAMMA^4 - 1) + p
PHI.3 <- df1*(GAMMA^6 - 1) + p
g <- (PHI.2-sqrt(PHI.2^2 - PHI.1*PHI.3))/PHI.1
nu <- (PHI.2 - 2*yy*GAMMA*(sqrt(df1*df2)))/(g^2)
LAMBDA.U <- yy*GAMMA*(sqrt(df1*df2))/(g^2)
limit <- df2*R2.Tilda/(nu*g)
diff3 <- stats::pf(limit, nu, df2, ncp=LAMBDA.U) - pll

while((abs(diff3) > .00001) & (round(llrhosq, 5) < 1))
{
x3 <- (x1 + x2)/2
yy <- x3/(1-x3)
GAMMA <- sqrt(1+yy)
PHI.1 <- df1*(GAMMA^2 - 1) + p
PHI.2 <- df1*(GAMMA^4 - 1) + p
PHI.3 <- df1*(GAMMA^6 - 1) + p
g <- (PHI.2-sqrt(PHI.2^2 - PHI.1*PHI.3))/PHI.1
nu <- (PHI.2 - 2*yy*GAMMA*(sqrt(df1*df2)))/(g^2)
LAMBDA.U <- yy*GAMMA*(sqrt(df1*df2))/(g^2)
limit <- df2*R2.Tilda/(nu*g)
diff3 <- stats::pf(limit, nu, df2, ncp=LAMBDA.U) - pll
yy <- x1/(1-x1)
GAMMA <- sqrt(1+yy)
PHI.1 <- df1*(GAMMA^2 - 1) + p
PHI.2 <- df1*(GAMMA^4 - 1) + p
PHI.3 <- df1*(GAMMA^6 - 1) + p
g <- (PHI.2-sqrt(PHI.2^2 - PHI.1*PHI.3))/PHI.1
nu <- (PHI.2 - 2*yy*GAMMA*(sqrt(df1*df2)))/(g^2)
LAMBDA.U <- yy*GAMMA*(sqrt(df1*df2))/(g^2)
limit <- df2*R2.Tilda/(nu*g)
diff1 <- stats::pf(limit, nu, df2, ncp=LAMBDA.U) - pll
ifelse((diff1*diff3 < 0), (x2 <- x3), (x1 <- x3))
llrhosq <- x3
}
}

if(round(llrhosq, 5)==1 & llrhosq>R2) llrhosq <- 0

if(pll==1) llrhosq <- 0
if(pul==0) ulrhosq <- 1
if(llrhosq > ulrhosq) warning("There is a problem; the lower limit is greater than the upper limit (Are you at one of the boundries of Rho^2 or is alpha very large?).")

if(pll==1) return(list(Lower.Conf.Limit.R2=0, Prob.Less.Lower=0, Upper.Conf.Limit.R2=ulrhosq, Prob.Greater.Upper=pul))
if(pul==0) return(list(Lower.Conf.Limit.R2=llrhosq, Prob.Less.Lower=1-pll, Upper.Conf.Limit.R2=1, Prob.Greater.Upper=0))
return(list(Lower.Conf.Limit.R2=llrhosq, Prob.Less.Lower=1-pll, Upper.Conf.Limit.R2=ulrhosq, Prob.Greater.Upper=pul))
}
}

Rsquare2F <- function(R2=NULL, df.1=NULL, df.2=NULL, p=NULL, N=NULL)
{
if(is.null(df.1) & is.null(df.2) & !is.null(N) & !is.null(p))
{
df.1 <- p
df.2 <- N-p-1
}
if(is.null(df.1) | is.null(df.2)) stop("You have not specified \'df.1\', \'df.2\', \'N\', and/or \'p\' correctly.")
return((R2/df.1)/((1-R2)/df.2))
}

conf.limits.ncf <-
function(F.value=NULL, conf.level=.95, df.1=NULL, df.2=NULL, alpha.lower=NULL, alpha.upper=NULL, tol=1e-9, Jumping.Prop=.10)
{
if(Jumping.Prop <=0 | Jumping.Prop >= 1) stop("The Jumping Proportion (\'Jumping.Prop\') must be between zero and one.")
if(is.null(F.value)) stop("Your \'F.value\' is not correctly specified.")
if(F.value < 0) stop("Your \'F.value\' is not correctly specified.")
if(is.null(df.1) | is.null(df.2)) stop("You must specify the degrees of freedom (\'df.1\' and \'df.2\').")
if(is.null(alpha.lower) & is.null(alpha.upper) & is.null(conf.level)) stop("You need to specify the confidence interval parameters.")
if((!is.null(alpha.lower) | !is.null(alpha.upper)) & !is.null(conf.level)) stop("You must specify only one method of defining the confidence limits.")

if(!is.null(conf.level))
{
if(conf.level >=1 | conf.level <= 0) stop("Your confidence level (\'conf.level\') must be between 0 and 1.")
alpha.lower <- alpha.upper <- (1-conf.level)/2
}

if(alpha.lower==0) alpha.lower <- NULL
if(alpha.upper==0) alpha.upper <- NULL

# Critical value for lower tail.
################################################################################################
FAILED <- NULL
if(!is.null(alpha.lower))
{
LL.0 <- stats::qf(p=alpha.lower*.0005, df1=df.1, df2=df.2) # Obtain a lower value by using the central F distribution
Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.0) - (1-alpha.lower)

if(stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.0) < (1-alpha.lower))
{
FAILED <- if(stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=0) < 1-alpha.lower) 
LL.0 <- .00000001
if(stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.0) < 1-alpha.lower) FAILED <- TRUE
#if(FAILED==TRUE) warning("The size of the effect combined with the degrees of freedom is too small to determine a lower confidence limit for the \'alpha.lower\' (or the (1/2)(1-\'conf.level\') symmetric) value specified (set to zero).", call. = FALSE)
}

if(is.null(FAILED))
{
LL.1 <- LL.2 <- LL.0 # Define both in case there is no need for the while loop (LL.2 is overwritten later if the while loop is used).

while(Diff > tol) # Find a value that is too small and one that is too big.
{
LL.2 <- LL.1*(1+Jumping.Prop) 
Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.2) - (1-alpha.lower)
LL.1 <- LL.2
}
LL.1 <- LL.2/(1+Jumping.Prop) # Produces the value directly before failure (a Lambda value that is too small.)

LL.Bounds <- c(LL.1, (LL.1+LL.2)/2, LL.2) # The middle value is in the middle.

Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.Bounds[2])-(1-alpha.lower)
while(abs(Diff) > tol) # Run the while loop to home in on the value satisfying the conditions (i.e., the lower limit).
{
Diff.1 <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.Bounds[1])-(1-alpha.lower) > tol
Diff.2 <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.Bounds[2])-(1-alpha.lower) > tol
Diff.3 <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.Bounds[3])-(1-alpha.lower) > tol

if(Diff.1==TRUE & Diff.2==TRUE & Diff.3==FALSE)
{
LL.Bounds <- c(LL.Bounds[2], (LL.Bounds[2]+LL.Bounds[3])/2, LL.Bounds[3])
}

if(Diff.1==TRUE & Diff.2==FALSE & Diff.3==FALSE)
{
LL.Bounds <- c(LL.Bounds[1], (LL.Bounds[1]+LL.Bounds[2])/2, LL.Bounds[2])
}

Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL.Bounds[2])-(1-alpha.lower)

}
LL <- LL.Bounds[2] # Confidence limit.
}
}
if(!is.null(FAILED)) LL <- NA
################################################################################################

# Critical value for upper tail.
################################################################################################
if(!is.null(alpha.upper))
{
FAILED.Up <- NULL
UL.0 <- stats::qf(p=1-alpha.upper*.0005, df1=df.1, df2=df.2)
Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.0)-alpha.upper

if(Diff < 0) UL.0 <- .00000001

Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.0)-alpha.upper
if(Diff < 0) 
{
FAILED.Up <- TRUE
# warning("The size of the effect combined with the degrees of freedom is too small to determine an upper confidence limit for the \'alpha.upper\' (or (1/2)(1-\'conf.level\') symmetric) value specified.", call. = FALSE)
}

if(is.null(FAILED.Up))
{
UL.1 <- UL.2 <- UL.0
while(Diff > tol)
{
UL.2 <- UL.1*(1+Jumping.Prop) 
Diff <-  stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.2) - alpha.upper
UL.1 <- UL.2
}
UL.1 <- UL.2/(1+Jumping.Prop) 

UL.Bounds <- c(UL.1, (UL.1+UL.2)/2, UL.2)

Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.Bounds[2])-alpha.upper
while(abs(Diff) > tol)
{
Diff.1 <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.Bounds[1])-alpha.upper > tol
Diff.2 <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.Bounds[2])-alpha.upper > tol
Diff.3 <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.Bounds[3])-alpha.upper > tol

if(Diff.1==TRUE & Diff.2==TRUE & Diff.3==FALSE)
{
UL.Bounds <- c(UL.Bounds[2], (UL.Bounds[2]+UL.Bounds[3])/2, UL.Bounds[3])
}

if(Diff.1==TRUE & Diff.2==FALSE & Diff.3==FALSE)
{
UL.Bounds <- c(UL.Bounds[1], (UL.Bounds[1]+UL.Bounds[2])/2, UL.Bounds[2])
}

Diff <- stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL.Bounds[2])-alpha.upper

}
UL <- UL.Bounds[2] # Confidence limit.
}
if(!is.null(FAILED.Up)) UL <- NA
}
################################################################################################
if(!is.null(alpha.lower) & !is.null(alpha.upper)) return(list(Lower.Limit=LL, Prob.Less.Lower=1-stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL), Upper.Limit=UL, Prob.Greater.Upper=stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL)))
if(is.null(alpha.lower) & !is.null(alpha.upper)) return(list(Upper.Limit=UL, Prob.Greater.Upper=stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=UL)))
if(!is.null(alpha.lower) & is.null(alpha.upper)) return(list(Lower.Limit=LL, Prob.Less.Lower=1-stats::pf(q=F.value, df1=df.1, df2=df.2, ncp=LL)))
}

Lambda2Rsquare <- function(Lambda=NULL, N=NULL)
{
if(is.null(Lambda) | is.null(N)) stop("You must specify \'Lambda\' (i.e., a noncentral parameter) and \'N\' (i.e., sample size) in order to calculate the noncentraility paramter.")
return(Lambda/(Lambda + N))
}

F2Rsquare <- function(F.value=NULL, df.1=NULL, df.2=NULL)
{
if(is.null(df.1) | is.null(df.2)) stop("You have not specified \'df.1\' and/or \'df.2\'.")
return(F.value*df.1/(F.value*df.1 + df.2))
}
