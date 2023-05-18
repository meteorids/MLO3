
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

train_data_path='./Training_Data/'
output_path='./net/'

nnstr='Ozone' #NN name string
hiddenlayers=(100,90,75) #set MLNN hidden layers (hidden layers will be reset for aaNN based on number of bands)

#radoption=2 #TOA rad data type, options: 1=Lt, 2=Lrc 
addnoise=0 #Flag for adding Gaussian noise NOTE: do not add noise for forward training
noiselevel=0 #Gaussian noise level, e.g. 1=1%

#sensor band infos
band=[302,312,320,340,380]
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
       par=np.loadtxt(train_data_path +'ML-input'+str(i+1)+'.txt')
    else:
       par=np.append(par,np.loadtxt(train_data_path +'ML-input'+str(i+1)+'.txt'),0)

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
trainx[:,1]=np.log10(par[:,3]) # Irradiance 380
trainx[:,2]=np.log10(par[:,4]) # Ratio

trainy[:,0]=np.log10(par[:,1]) # Ozone
trainy[:,1]=np.log10(par[:,2]) # Cloud. Vol. Frac.

net_name='net_'+fname_prefix+'_ozone'+str(nrrs)+'_'+layerstr+'.h5'	  
for i in np.arange(nrrs):
    if i==0:
       labelparam=['Rrs'+str(band[i])+'nm']
    else:
       labelparam=np.append(labelparam,['Rrs'+str(band[i])+'nm'],0)			  

ninput=len(trainx[0])
train_in=np.zeros((ninput,2))
for i in range(ninput):
    train_in[i,0]=trainx[:,i].min()
    train_in[i,1]=trainx[:,i].max()
noutput=len(trainy[0])	
train_out=np.zeros((noutput,2))
for i in range(noutput):
    train_out[i,0]=trainy[:,i].min()
    train_out[i,1]=trainy[:,i].max()
#normalize the trainx and trainy to [-1,1]	
for i in range(ninput):
    trainx[:,i]=2*(trainx[:,i]-train_in[i,0])/(train_in[i,1]-train_in[i,0])-1
for i in range(noutput):
    trainy[:,i]=2*(trainy[:,i]-train_out[i,0])/(train_out[i,1]-train_out[i,0])-1
	
#Build MLNN
mlnn=MLPRegressor(hidden_layer_sizes=nnlayer,
     activation='tanh',
	  solver='adam',
    	  batch_size='auto',
    	  learning_rate='adaptive',
    	  learning_rate_init=0.001,
    	  max_iter=1000,
    	  random_state=5,
    	  tol=1.0e-8,
    	  verbose=True,
    	  early_stopping=True,  
    	  validation_fraction=0.1)
#traing MLNN
mlnn=mlnn.fit(trainx,trainy)
    
#save trained NN to HDF5
nn_structure=np.zeros([nlayer+2])
nn_structure[0]=ninput
for i in range(nlayer):
    nn_structure[i+1]=nnlayer[i]
nn_structure[nlayer+1]=noutput
    
hf = h5py.File('./net/'+net_name,'w')
hf.create_dataset('Layers',dtype='int8',data = nn_structure)
hf.create_dataset('Norm_in',dtype='float64',data = train_in)
hf.create_dataset('Norm_out',dtype='float64',data = train_out)
gw = hf.create_group('Weights')
for i in range(nlayer+1):    
    gw.create_dataset('Layer'+str(i+1),dtype='float64',data=np.transpose(mlnn.coefs_[i]))
gb = hf.create_group('Bias')
for i in range(nlayer+1):
    gb.create_dataset('Layer'+str(i+1),dtype='float64',data=mlnn.intercepts_[i].reshape(int(nn_structure[i+1]),1))
hf.close()   

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
    plt.xlabel('Model '+labelparam[i]) #original line:   plt.xlabel('Model '+labelparam[i])
    plt.ylabel('MLNN '+labelparam[i])  #original line: plt.ylabel('MLNN '+labelparam[i])
    plt.plot([0,lim[i]],[0,lim[i]],'k')
    plt.text(lim[i]*0.05,lim[i]*0.94,('R$^2$ = %0.3f' % (r2[i])))
    plt.text(lim[i]*0.05,lim[i]*0.88,('APE = %0.2f' % (ape[i])+'%'))
    plt.text(lim[i]*0.05,lim[i]*0.82,('Bias = %0.2f' % (bias[i])+'%'))
plt.tight_layout()
plt.savefig(output_path+net_name+'_Training.png')


