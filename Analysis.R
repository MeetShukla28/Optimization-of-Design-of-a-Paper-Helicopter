setwd("C:/Meet College/616/Project")
df = read.csv(file="Data(randomized).csv") #Insert the Directory in the CSV format
#install.packages("faraway")
library(olsrr)
library(faraway)
na.omit(df)
# For Location
x = halfnorm(df$locationvalues, nlab = 10, labs = as.character(df$locationmaineffects), ylab = "Sorted Data for Location",) 
# For Dispersion
y = halfnorm(df$dispersionvalues, nlab = 10, labs = as.character(df$dispersionmaineffects), ylab = "Sorted Data for Dispersion",) 

# Hamada-Wu
# For Location Effects

#for factor l
m1 = lm(ybar~l+lw+lL+lW+ld+lF ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m1)
# for factor w
m2 = lm(ybar~w+wL+wW+wd+wF+lw ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m2)
# for factor L
m3 = lm(ybar~L+LW+Ld+LF+wL+lL ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m3)
# for factor W
m4 = lm(ybar~W+Wd+WF+LW+wW+lW ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m4)
# for factor d
m5 = lm(ybar~d+dF+Wd+Ld+wd+ld ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m5)
# for factor F
m6 = lm(ybar~F+dF+WF+LF+wF+lF ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m6)


#Step 2
#Considering all significant factors obtained from step1
m7 = lm(ybar~l+w+L+W+F+d+lF+LW+lw+Wd ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m7)


#Step 3
#Considering all significant factors obtained from step2
m8 = lm(ybar~l+W+WF+LW+wW+Wd+lW+lw+lL+lW+lF ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m8)

#Step 4 
#Considering all significant factors obtained from step1
m9 = lm(ybar~l+w+L+W+F+d+Wd+wW+WF ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m9)



# Thus as the model does not change any further we consider it as the best model obtained by step wise regression.


# For Dispersion Effects

#for factor l
m1 = lm(lns2~l+lw+lL+lW+ld+lF ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m1)
# for factor w
m2 = lm(lns2~w+wL+wW+wd+wF+lw ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m2)
# for factor L
m3 = lm(lns2~L+LW+Ld+LF+wL+lL ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m3)
# for factor W
m4 = lm(lns2~W+Wd+WF+LW+wW+lW ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m4)
# for factor d
m5 = lm(lns2~d+dF+Wd+Ld+wd+ld ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m5)
# for factor F
m6 = lm(lns2~F+dF+WF+LF+wF+lF ,data=df) # Using stepwise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m6)


#Step 2
#Considering all significant factors obtained from step1
m7 = lm(lns2~ l+w+L+W+F+d+lw+wL+LW+Wd+dF ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m7)


#Step 3
#Considering all significant factors obtained from step2
m8 = lm(lns2~ w+wW+wL+wd+wF+lw ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m8)

#Step 4 
#Considering all significant factors obtained from step1
m9 = lm(lns2~l+w+L+W+F+d+lw+wL ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m9)

#Step 5
#Considering all significant factors obtained from step2
m10 = lm(lns2~ W+w+d+Wd+WF+LW+wW+lW+dF+Ld+wd+ld+wL+wF+lw ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m10)

#Step 6 
#Considering all significant factors obtained from step1
m11 = lm(lns2~l+w+L+W+F+d+wL+LW+lw ,data=df) # Using step wise method by adding and deleting factors to determine the optimum solution
ols_step_both_p(m11)


# Thus as the model does not change any further we consider it as the best model obtained by step wise regression.
