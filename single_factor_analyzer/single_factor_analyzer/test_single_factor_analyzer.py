### This file is to test the single_factor_analyzer model works correctly
## Author: Genwei Zhang
## Date 07/28/2018

from single_factor_analyzer import *
import numpy as np 
import matplotlib.pyplot as plt  

# pick an data vector, 5x3 
x= np.array([[3, -0.001, 0.3],
            [5, 0.01, 3],
            [6, -0.001, 0.007,],
            [0.03, -6, 4],
            [0.5, -0.02, 5]])

ps, lam, lik=single_factor_analyzer(x)

print (lik)

## make a plot to show the likelihood changes

# adjust some font sizes
plt.rc('axes', titlesize=8)     # fontsize of the axes title
plt.rc('axes', labelsize=12)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=12)    # fontsize of the tick labels
plt.rc('ytick', labelsize=12)    # fontsize of the tick labels

# plot
fig = plt.figure()
fig.patch.set_facecolor('white')
plt.xlim (0, 100)
plt.xlabel('Epoch #', labelpad=10)
plt.ylabel('The log likelihood values', labelpad=10)
plt.plot(np.arange(len(lik)),lik, 'b*' )  
plt.title ('Single Factor Analysis with EM', fontsize=20)
plt.show()






