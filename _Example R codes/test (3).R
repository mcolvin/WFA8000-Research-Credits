# R Conditional Probability Tree Diagram
 
# The Rgraphviz graphing package must be installed to do this
require("Rgraphviz")
 
# Change the three variables below to match your actual values
# These are the values that you can change for your own probability tree
# From these three values, other probabilities (e.g. prob(b)) will be calculated 
 
# Probability of a
a<-.01
 
# Probability (b | a)
bGivena<-.99
 
# Probability (b | ¬a)
bGivenNota<-.10
 
###################### Everything below here will be calculated
 
# Calculate the rest of the values based upon the 3 variables above
notbGivena<-1-bGivena
notA<-1-a
notbGivenNota<-1-bGivenNota
 
#Joint Probabilities of a and B, a and notb, nota and b, nota and notb
aANDb<-a*bGivena
aANDnotb<-a*notbGivena
notaANDb <- notA*bGivenNota
notaANDnotb <- notA*notbGivenNota
sum(aANDb,aANDnotb,notaANDb,notaANDnotb)

# Probability of B
b<- aANDb + notaANDb
notB <- 1-b
 
# Bayes theorum - probabiliyt of A | B
# (a | b) = Prob (a AND b) / prob (b)
aGivenb <- aANDb / b