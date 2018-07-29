### This script is for BCM ML position programming task only--single factor analysis using EM
## Math equations come from http://www.cs.toronto.edu/~fritz/absps/tr-96-1.pdf
## Author: Genwei Zhang
## Date 07/28/2018


import numpy as np 

def single_factor_analyzer(x, k=3, iter=100, cvgen=0.00002):

    '''
     --------
    |  Input |
     --------
    x: matrix, the real-valued data vector
    k: the number of factors to model x, default 3
    iter: the number of iterations that the client wants to try, default 100
    cvgen: the convergence criteria, default 0.00001

     ---------
    |  Output |
     ---------
    psi: a diagonal matrix
    lambda_: the factor loading matrix
    likhd: track of the likelihood values during iteration 

    '''

    # first of all, retrieve the dimensions of the data matrix x 
    num_row,num_col=x.shape
    
    # x should distributed with 0 mean, so shift by subtracting the column mean
    x=x-np.mean(x,axis=0)
    
    # randomly generate psi and lamda
    psi=np.diag(np.random.rand(num_col, num_col))
    lambda_=np.random.rand(num_col, k)

    # unit matrix, k dimension
    I=np.eye(k)

    # constant for computing the log likelihood
    const=100

    # initialize the likelihood to be zero.
    indvidual_lik=0
    likhd=[]   # list to store likelihood values


    for i in range (iter):

        #### E-step

        psi_inv=np.diag(1/psi) # inverse
        psi_inv_lambda_=psi_inv.dot(lambda_)
        lambda_T_psi_inv=lambda_.T.dot(psi_inv)
        mid=np.linalg.inv(I+ lambda_.T.dot(psi_inv_lambda_))
        beta=lambda_.T.dot(psi_inv-psi_inv_lambda_.dot(mid).dot(lambda_T_psi_inv))
        xx=x.T.dot(x)/(num_col-1)  # x'x
        
        EZX=xx.dot(beta.T) # the first moment of the factors
        EZZX=I-beta.dot(lambda_)+beta.dot(EZX) # the second moment of the factors

        #### M-step

        lambda_=EZX.dot(np.linalg.inv(EZZX))  # update lambda
        psi=np.diag(xx)-np.diag(lambda_.dot(EZX.T)) # update psi

        ### calculate the likelihood
        prev_lik=indvidual_lik    # save the last one
        term1=0.5*np.sum(xx.T.dot(psi_inv).dot(xx)-xx.T.dot(psi_inv_lambda_).dot(EZX))
        term2=0.5*np.sum(np.diag(lambda_.T.dot(psi_inv_lambda_).dot(EZZX))) 
        indvidual_lik=const-0.5*num_col*np.log(np.linalg.det(np.diag(psi)))-term1+ term2
        likhd.append(indvidual_lik)  # add to likelihood list

        ## check convergence
        if i==0:
            lik_1st=indvidual_lik  # record the first likelihood value
        elif (indvidual_lik<prev_lik):
            print('Attention!!! Decreasing log likelihood was found!')
        elif (indvidual_lik-prev_lik<cvgen*(prev_lik-lik_1st)):
            print("Convergence achieved!!!")
            break 
        if i==iter-1:
            print ('Designated iterations completed without convergence, need to consider increasing the iteration cycles!' )
    
    return psi, lambda_, likhd 




