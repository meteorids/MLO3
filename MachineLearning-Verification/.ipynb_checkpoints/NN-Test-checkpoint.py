
#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import random
import math
import h5py
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import r2_score
import matplotlib.pyplot as plt
import warnings

# disable FutureWarning
warnings.simplefilter(action='ignore', category=FutureWarning)

train_data_path='./AccuRT-Data/'
output_path='./net/'

nnstr='Ozone' #NN name string
hiddenlayers=(100,90,75) #set MLNN hidden layers (hidden layers will be reset for aaNN based on number of bands)

#radoption=2 #TOA rad data type, options: 1=Lt, 2=Lrc 
addnoise=0 #Flag for adding Gaussian noise NOTE: do not add noise for forward training
noiselevel=0 #Gaussian noise level, e.g. 1=1%

#sensor band infos
band=['320/302',380,320,340,380]
trainband=np.arange(5)
nrrs=5
aodidx=np.arange(5)

#number of sample in the scatter plot, if total number of data points is less than this, half of the data will be used for plotting
nplotsample=5000 
	
nband=len(band)
ntrainband=len(trainband)
naod=len(aodidx) 
      
#read in training data
print('Loading training dataset...')
for i in np.arange(5):
    if i==0:       
       par=np.loadtxt(train_data_path +'validation'+str(i+1)+'.txt')
    else:
       par=np.append(par,np.loadtxt(train_data_path +'validation'+str(i+1)+'.txt'),0)

print('Training dataset loaded.')



#remove negative values
idx=np.where(np.sum(par<0,axis=1)==0)[0]
par=par[idx,:]

#total number of training cases
ncase=len(par)
rad=np.zeros((ncase,1))

#rad[:,0] = par[:,3]
#rad[:,1] = par[:,4]

#generate the gaussian noise for each band
if addnoise==1:
    fname_prefix=nnstr+'WiGN_p'+str(noiselevel)
    noise=np.random.normal(1,noiselevel/100,(ncase,1))    # only use 2 ratio
    rad=np.multiply(rad,noise) 
else:
    fname_prefix=nnstr+'WoGN' 
     
# NN training: Ozone & Cloud Optical Depth   
#trainingoption = itrain + 1

nnlayer = hiddenlayers
            
nlayer=len(nnlayer) # number of hidden layers
    
#create layer string for file name
layerstr=''
for i in np.arange(nlayer):
    if i<nlayer-1:
       layerstr=layerstr+str(nnlayer[i])+'X'
    else:
       layerstr=layerstr+str(nnlayer[i])

trainx=np.zeros((ncase,3))
trainy=np.zeros((ncase,2))

trainx[:,0]=np.cos(np.deg2rad(par[:,0])) # geometry: cos[Solar Zenith Angle]
trainx[:,1]=np.log10(par[:,1]) # Irradiance 380
trainx[:,2]=np.log10(par[:,2]) # Ratio

trainy[:,0]=np.log10(par[:,1]) # Ozone
trainy[:,1]=np.log10(par[:,2]) # Cloud. Vol. Frac.




#make prediction using trained MLNN
nnoutput=mlnn.predict(trainx)
#converting data
for i in range(noutput):
    nnoutput[:,i]=(nnoutput[:,i]+1)/2*(train_out[i,1]-train_out[i,0])+train_out[i,0]
    trainy[:,i]=(trainy[:,i]+1)/2*(train_out[i,1]-train_out[i,0])+train_out[i,0]
	
nnoutput=10 ** nnoutput
trainy=10 ** trainy
    
#compute average percentage error
diff=(nnoutput-trainy)/trainy*100
ape=np.mean(np.absolute(diff), axis=0)
bias=np.mean(diff, axis=0)
    
lim=np.amax(trainy,axis=0)*1.2 #set limit
r2=np.zeros(noutput)
if ncase>nplotsample:
    idx=random.sample(list(range(ncase)),nplotsample)
else:
    idx=random.sample(list(range(ncase)),int(ncase/2))
if noutput<5:
    plt.figure(figsize=(18,5),dpi=150)
else:
    plt.figure(figsize=(18,9),dpi=150)
for i in np.arange(noutput):
    r2[i]=r2_score(trainy[:,i],nnoutput[:,i])
    print(r2[i])
    plt.subplot(2,4,i+1)           
    plt.scatter(trainy[idx,i],nnoutput[idx,i],s=2,c='red')
    plt.xlim(0, lim[i])
    plt.ylim(0, lim[i])
    plt.xlabel('Model '+labelparam[i])    
    plt.ylabel('MLNN '+labelparam[i])   
    plt.plot([0,lim[i]],[0,lim[i]],'k')
    plt.text(lim[i]*0.05,lim[i]*0.94,('R$^2$ = %0.3f' % (r2[i])))
    plt.text(lim[i]*0.05,lim[i]*0.88,('APE = %0.2f' % (ape[i])+'%'))
    plt.text(lim[i]*0.05,lim[i]*0.82,('Bias = %0.2f' % (bias[i])+'%'))
plt.tight_layout()
plt.savefig(output_path+net_name+'_Training.png')


