# Benford’s Law describes the expected distribution of leading digits in naturally occurring datasets. According to it, the probability that the first digit is d (1 through 9) is:
#   P(d)=log⁡10(1+/1d)
   
#   So:
#   1 appears as the first digit about 30.1% of the time
#   2 appears about 17.6%
#   3 about 12.5%
#   
#   …and so on, down to 9 (about 4.6%)

log10(1 + 1/1)
log10(1 + 1/2)
log10(1 + 1/(1:9))

#This distribution is counterintuitive but holds for many naturally occurring datasets: e.g., population numbers, financial data, physical constants, etc.

d<-read.csv("Suicide-Toxo.csv",sep=";")

benford_test <- function(x) {
  x <- na.omit(x)
  x <- x[x > 0]
  first_digits <- as.numeric(substr(as.character(x), 1, 1))
  observed <- table(factor(first_digits, levels=1:9))
  expected <- log10(1 + 1/(1:9)) * length(first_digits)
  barplot(rbind(observed, expected), beside=TRUE, col=c("skyblue", "orange"),
          legend.text=c("Observed", "Expected"), main="Benford's Law")
}

benford_test(d$IL6)
benford_test(d$TNF)
benford_test(d$IFN)

#Something similar can be chacked for all digits
digits <- function(x, which=1) {
  x <- na.omit(x[x > 0])
  digs <- gsub("\\D", "", as.character(x))
  out <- if (which == "all") unlist(strsplit(digs, "")) else substr(digs, which, which)
  barplot(table(factor(out, levels=0:9)), main=paste("Digit position:", which))
}

digits(d$IL6,which=1)
digits(d$IL6,which="all")
digits(d$TNF,which="all")
digits(d$IFN,which="all")

digits(d$IFN,which="all")

#But now data that follow the correct distribution are just too easy to generate
newIFN<-round(exp(rnorm(nrow(d),mean(log(d$IFN)),sd(log(d$IFN)))))
digits(newIFN,which="all")

benford_test(newIFN)


