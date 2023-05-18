# -*- coding: utf-8 -*-

"""

Created on Thu Jan 17 19:40:49 2019


@author: Yongzhen Fan

"""


import numpy as np
import h5py


class MLNN(object):


    def __init__(self, sensorinfo=None):

        print('Loading Multilayer Neural Networks (MLNNs) ... ')
        self.sensor = sensorinfo.sensor
        self.path = './net'
        self.band = sensorinfo.band
        self.vgaino = sensorinfo.vgaino
        self.vgainc = sensorinfo.vgainc

        #read aann network
#        aann_fname=self.path+self.sensor+'/'+self.sensor+'_Lt_aaNN.h5'

        aann_fname='./net/net_OzoneWoGN_ozone5_100X90X75.h5'
        #aann_fname=self.path+self.sensor+'/'+self.sensor+'Ozone.h5'
        
        f = h5py.File(aann_fname, 'r')
        
        self.aann_layers=np.array(f['Layers'])
        
        self.aann_nlayers=len(self.aann_layers)

        self.aann_norm_in=np.array(f['Norm_in'])

        self.aann_norm_out=np.array(f['Norm_out'])

        self.aann_weights=[]

        self.aann_bias=[]

        for i in range(self.aann_nlayers-1):

            self.aann_weights.append(np.array(f['Weights/Layer'+str(i+1)]))

            self.aann_bias.append(np.array(f['Bias/Layer'+str(i+1)]))


    def compute_aann(self, sza, irrad, ratio):

        print('Checking Rayleigh corrected radiance (Lrc) spectral shape ... ')

        #rebuild aaNN and compute data

        ncase=len(sza) 

        nlayers=len(self.aann_layers)

        aainput=np.zeros((ncase,int(self.aann_layers[0])))
        
        
        #INPUTS: 

        aainput[:,0]=np.cos(np.deg2rad(solz))

        aainput[:,1]= irrad

        aainput[:,2]= ratio

       # aainput[:,range(3,int(self.aann_layers[0])-1)]=np.log10(lrc)

       # aainput[:,int(self.aann_layers[0])-1]=np.log10(rh)

        #normalize

        for i in range(int(self.aann_layers[0])):

            aainput[:,i]=2*(aainput[:,i]-self.aann_norm_in[i,0])/(self.aann_norm_in[i,1]-self.aann_norm_in[i,0])-1

        #compute using aaNN

        for i in np.arange(nlayers-1):

            if i == 0:

                lastlayer = np.tanh(np.matmul(self.aann_weights[i],aainput.transpose())+self.aann_bias[i])

            elif i < nlayers-2:

                currentlayer = np.tanh(np.matmul(self.aann_weights[i],lastlayer)+self.aann_bias[i])

                lastlayer = currentlayer

            else:

                currentlayer = np.matmul(self.aann_weights[i],lastlayer)+self.aann_bias[i]

        aann_output = currentlayer.transpose()


        #denormlize

        for i in range(int(self.aann_layers[-1])):

            aann_output[:,i]=(aann_output[:,i]+1)/2*(self.aann_norm_out[i,1]-self.aann_norm_out[i,0])+self.aann_norm_out[i,0]

        aann_output=np.power(10, aann_output)

        #set flags for out of scope 

        #pe=(aann_output-lrc)/lrc*100.0

        ratio=np.mean(np.absolute(aann_output/lrc-1),axis=1)

        oos=np.zeros(ncase,dtype='bool')

        oos = ratio >0.07

        return oos,aann_output