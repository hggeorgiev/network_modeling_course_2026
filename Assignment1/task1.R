# task 1:

seed = 161
set.seed(seed)
num_perm = 5000

# (1)
# 22 pupils -> total number of possible dyads = n*(n-1)/2 = 22*21/2 = 231
# for (3) to make sense though, we will use "probability p of observing a tie if you choose a directed edge at random"
# number of ties m = 134, number of directed edges = 2 * 231 = 462
num_directed = 462
p = 134 / num_directed
print(paste("task1.1: p =", p))

# (2)
# M = 42, A = 50, N = 139. sum = 231
p_M = 42 / 231
p_A = 50 / 231
p_N = 139 / 231
print(paste("task1.2: p_M =", p_M))
print(paste("task1.2: p_A =", p_A))
print(paste("task1.2: p_N =", p_N))

# (3)
# it means we assume for a mutal tie for example that p(mutual) = p(i->j)*p(j->i) = p^2
p_M_ind = p*p
p_A_ind = 2*p*(1-p)
p_N_ind = (p-1)*(p-1)
print(paste("task1.3: p_M_ind = p^2 = ",p_M_ind))
print(paste("task1.3: p_A_ind = 2*p*(1-p) = ",p_A_ind))
print(paste("task1.3: p_N_ind = (1-p)^2 = ",p_N_ind))

# (4)
print("task1.4:")
print("The results show: p_M_ind < p_M, p_A < p_A_ind, p_N_ind < p_N.")
print("From that we can conclude that the ties are not independent, since mutual connection as well as no connection is much more probable compared to the independent case, assymmetric dyads are half as probable compared")

# (5)
# we have 22 pupils, if each pupil now is assumed to make 9 outward connections we get p = 9 / 21
p_new = 9/21
p_M_ind_new = p_new*p_new
print(paste("task1.5: p_new = ",p_new))
print(paste("task1.5: p_M_ind_new = ",p_M_ind_new))
print("task1.5: compared to the random chance independent p_M we get a very close estimate of p_M with our assumption of 9 outgoing edges. The model is very reasonable since pupils might be friends with on average 9 other pupils.")


# (6)
install.packages("sna")
install.packages("network")
library(sna)
library(network)

obsMat = read.csv2("./Lintner/10_W1.csv", header = TRUE, row.names = 1)
print(dim(obsMat))
print(obsMat)
obsMat_clean = obsMat[!is.na(rowSums(obsMat)),!is.na(rowSums(obsMat))]
print(dim(obsMat_clean))  
print(obsMat_clean)
obsMat_clean = as.matrix(obsMat_clean)
print(obsMat_clean)

# cug.test(dat, FUN, mode = c("digraph", "graph"), cmode = c("size", 
#     "edges", "dyad.census"), diag = FALSE, reps = 1000, 
#      ignore.eval = TRUE, FUN.args = list())

# FUN = function generating the test statistic (grecip -> the dyadic reciprocity of a graph is the proportion of dyads which are symmetric; 
#                                               centralization -> returns centralization GLI given a node centrality measure
#                                               gtrans -> computes the transitivity of a graph)

# cmode = string indicating the type of conditioning to be applied (edges -> holds no. of edges fixes; dyad.census -> holds no. of dyad fixed as observed)


cguRec <- cug.test(obsMat_clean, grecip, cmode = "edges", reps=num_perm)
cugInd <- cug.test(obsMat_clean, centralization, cmode="edges", FUN.arg=list(FUN=degree, cmode="indegree"), reps=num_perm)
cugTrans <- cug.test(obsMat_clean, gtrans, cmode = "dyad.census", reps=num_perm)

# (6.1)
# 
print("cguRec: hypotheses -> The observed reciprocity is higher than expected compared to chance; conditional feature -> number of edges is fixed")
print("cugInd: hypotheses -> The observed indegree centralization is higher than expected compared to chance; conditional feature -> number of edges is fixed")
print("cugTrans: hypotheses -> The observed transisitivity is higher than expected compared to chance; conditional feature -> Dyad census (observed M/A/N counts) is fixed")

# (6.2)
print(cguRec)
print(cugInd)
print(cugTrans)

print("task1.6.2: cguRec -> Pr(X>=Obs) = 0, Pr(X<=Obs) = 1, non of the random graphs had a higher observed reciprocity (0.77), therefore the data strongly suggests pupils are friends mutually instead of just one sided")
print("task1.6.2: cugInd -> Pr(X>=Obs) =  0.7746, Pr(X<=Obs) = 0.6152, data does not show a pupil being overly popular, pipularity is spread evenly")
print("task1.6.2: cugTrans -> Pr(X>=Obs) = 0, Pr(X<=Obs) = 1, non of the random graphs had a higher observed transistivity (0.536), therefore the data strongly suggests puils are friends with the friends of friends")

