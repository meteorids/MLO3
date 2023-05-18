% This script plots the inherent optical properties (IOPs) for a 
% particular run in several ways:
% a) IOPs for a particular layer (userLayerNo)
%    a, b, g, tau
% b) IOPs for a particular layer (userLayerNo)
%    tau, ssa, g, a & b
% c) IOPs at a particular wavelength (userWavelengthNo)
%    cumulative tau, ssa, g, a & b
% d) the cumulative optical depths for the atmosphere/ocean vs wavelength

clear all;

set(0,'DefaultFigurePosition',[360 258 900 900])
set(0,'DefaultAxesFontSize',24)
set(0,'DefaultLineLineWidth',1)

fileName = 'iops.txt';
iops = readIops(fileName);

% Begin user input
runNumber = 1;
userLayerNo  = length(iops(runNumber).layerDepths)-1;
atmLayerNo   = find(iops.layerDepths==1e5);
userWavelengthNo = 3;
% End user input

numLayers           = length(iops(runNumber).layerDepths);
numWavelengths      = length(iops(runNumber).wavelengths);
atmTauCumUnscaled   = 0;
atmTauCumScaled     = 0;
oceanTauCumUnscaled = 0;
oceanTauCumScaled   = 0;
lwt                 = 'linewidth';
lw1                 = 3.5;
lw2                 = 2.0;

% a)
wl            = iops(runNumber).wavelengths;
a             = iops(runNumber).absorptionCoefficients(userLayerNo,:);
bScaled       = iops(runNumber).scatteringCoefficients(userLayerNo,:);
scalingFactor = iops(runNumber).scatteringScalingFactor(userLayerNo,:);
bUnscaled     = bScaled./scalingFactor;
gScaled       = iops(runNumber).phaseMoments(userLayerNo,:,2);
z             = iops(runNumber).layerDepths(userLayerNo) ...
               - iops(runNumber).layerDepths(userLayerNo-1);
dtauUnscaled  = (a + bUnscaled) * z;
dtauScaled    = (a + bScaled) * z;

% b)
ssaUnscaled = bUnscaled ./ (a + bUnscaled);
ssaScaled   = bScaled ./ (a + bScaled);
% extinction coefficients (not currently plotted)
cUnscaled   = bUnscaled + a;
cScaled     = bScaled + a;

% c)
wl2            = iops(runNumber).wavelengths(userWavelengthNo);
z2             = iops(runNumber).layerDepths(1:atmLayerNo);
zl             = length(iops(runNumber).layerDepths(1:atmLayerNo));
for i=1:zl
  if(i==1)
    dz2(i) = iops(runNumber).layerDepths(1);
  else
    dz2(i) = iops(runNumber).layerDepths(i)-iops(runNumber).layerDepths(i-1);
  end
end
a2             = iops(runNumber).absorptionCoefficients(1:zl,userWavelengthNo);
bScaled2       = iops(runNumber).scatteringCoefficients(1:zl,userWavelengthNo);
scalingFactor2 = iops(runNumber).scatteringScalingFactor(1:zl,userWavelengthNo);
bUnscaled2     = bScaled2./scalingFactor2;
gScaled2       = iops(runNumber).phaseMoments(1:zl,userWavelengthNo,2);
tauUnscaled2   = (a2 + bUnscaled2) .* dz2';
tauScaled2     = (a2 + bScaled2) .* dz2';
ssaUnscaled2   = bUnscaled2 ./ (a2 + bUnscaled2);
ssaScaled2     = bScaled2 ./ (a2 + bScaled2);
cUnscaled2     = bUnscaled2 + a2;
cScaled2       = bScaled2 + a2;
for i=1:length(tauUnscaled2)
  if(i==1)
    tauCumUnscaled2(i) = tauUnscaled2(i);
    tauCumScaled2(i)   = tauScaled2(i);
  else
    tauCumUnscaled2(i) = tauCumUnscaled2(i-1)+tauUnscaled2(i);
    tauCumScaled2(i)   = tauCumScaled2(i-1)+tauScaled2(i);
  end
end
tauCumUnscaled2;
tauCumScaled2;

% d)
for i=1:atmLayerNo
  a_tmp             = iops(runNumber).absorptionCoefficients(i,:);
  bScaled_tmp       = iops(runNumber).scatteringCoefficients(i,:);
  scalingFactor_tmp = iops(runNumber).scatteringScalingFactor(i,:);
  bUnscaled_tmp     = bScaled_tmp./scalingFactor_tmp;
  gScaled3       = iops(runNumber).phaseMoments(i,:,2);

  if(i==1)
    dz = iops(runNumber).layerDepths(1);
  else
    dz = iops(runNumber).layerDepths(i)-iops(runNumber).layerDepths(i-1);
  end
  atmTauCumUnscaled   = atmTauCumUnscaled + (a_tmp + bUnscaled_tmp) * dz;
  atmTauCumScaled     = atmTauCumScaled + (a_tmp + bScaled_tmp) * dz;
end
% d) continued
for i=atmLayerNo+1:numLayers
  a_tmp             = iops(runNumber).absorptionCoefficients(i,:);
  bScaled_tmp       = iops(runNumber).scatteringCoefficients(i,:);
  scalingFactor_tmp = iops(runNumber).scatteringScalingFactor(i,:);
  bUnscaled_tmp     = bScaled_tmp./scalingFactor_tmp;
  gScaled3       = iops(runNumber).phaseMoments(i,:,2);

  dz = iops(runNumber).layerDepths(i)-iops(runNumber).layerDepths(i-1);
  oceanTauCumUnscaled   = oceanTauCumUnscaled + (a_tmp + bUnscaled_tmp) * dz;
  oceanTauCumScaled     = oceanTauCumScaled + (a_tmp + bScaled_tmp) * dz;
end

clear *_tmp

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a) IOPs for a particular layer (userLayerNo) a, b, g, dtau
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
subplot(2,2,1)
plot(wl,a,'r',lwt,lw2)
title(['Layer = ',num2str(userLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Absorption coefficient [1/m]')
hl=legend('absorption');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%
subplot(2,2,2)
plot(wl,bUnscaled,'c',lwt,lw1)
hold on;
plot(wl,bScaled,'b',lwt,2)
%title(['Layer = ',num2str(userLayerNo),' '])
xlabel('Wavelength [nm]')
ylabel('Scattering coefficient [1/m]')
hl=legend('scattering unscaled','delta-fit scaled');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%
subplot(2,2,3)
plot(wl,gScaled,'k',lwt,lw2)
hold on;
plot(wl,scalingFactor,'g',lwt,lw2)
%title(['Layer = ',num2str(userLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Asymmetry factor')
hl=legend('asymmetry factor scaled','scaling factor');
set(hl,'fontsize',10)
legend boxoff
grid on
set(gca,'ylim',[0,1.2])
%%%%%%%
subplot(2,2,4)
plot(wl,dtauUnscaled,'c',lwt,lw1)
hold on;
plot(wl,dtauScaled,'k',lwt,lw2)
%title(['Layer = ',num2str(userLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Optical depth')
hl=legend('tau','delta-fit scaled');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% b) IOPs for a particular layer (userLayerNo)  tau, ssa, g, a & b
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
%%%%%%%
subplot(2,2,1)
plot(wl,dtauUnscaled,'c',lwt,lw1)
hold on;
plot(wl,dtauScaled,'k',lwt,lw2)
title(['Layer = ',num2str(userLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Optical depth')
hl=legend('tau','delta-fit scaled');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%
subplot(2,2,2)
plot(wl,ssaUnscaled,'c',lwt,lw1)
hold on;
plot(wl,ssaScaled,'m',lwt,lw2)
%title(['Layer = ',num2str(userLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Single-scattering albedo')
hl=legend('SSA','delta-fit scaled');
set(hl,'fontsize',10)
set(gca,'ylim',[0,1.2])
legend boxoff
grid on
%%%%%%%
subplot(2,2,3)
plot(wl,gScaled,'k',lwt,lw2)
hold on;
plot(wl,scalingFactor,'g',lwt,lw2)
%title(['Layer = ',num2str(userLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Asymmetry factor')
hl=legend('asymmetry factor scaled','scaling factor');
set(hl,'fontsize',10)
legend boxoff
grid on
set(gca,'ylim',[0,1.2])
%%%%%%%
subplot(2,2,4)
plot(wl,a,'r',lwt,lw2)
%title(['User Layer = ',num2str(userLayerNo)])
hold on;
plot(wl,bUnscaled,'c',lwt,lw1)
hold on;
plot(wl,bScaled,'b',lwt,2)
%title(['Layer = ',num2str(userLayerNo),' '])
xlabel('Wavelength [nm]')
ylabel('Coefficient [1/m]')
hl=legend('absorption','scattering','scattering scaled');
%%plot(wl,cUnscaled,'c',lwt,lw1)
%%hold on;
%%plot(wl,cScaled,'k',lwt,2)
%%ylabel('Extinction Coefficient [1/m]')
%%hl=legend('unscaled','delta-fit scaled');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c) IOPs as a function of depth in the atmosphere for a single userWavelengthNo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
%%%%%%%
subplot(2,2,1)
%plot(z2,tauUnscaled2,'c',lwt,lw1)
%hold on;
%plot(z2,tauScaled2,'k',lwt,lw2)
plot(z2,tauCumUnscaled2,'c',lwt,lw1)
hold on;
plot(z2,tauCumScaled2,'k',lwt,lw2)
title(['Wavelength = ',num2str(wl2),' nm'])
xlabel('Depth [m]')
ylabel('Cumulative optical depth')
hl=legend('cum. tau','delta-fit scaled');
set(hl,'fontsize',10)
xlim([0 100000])
legend boxoff
grid on
%%%%%%%
subplot(2,2,2)
%plot(z2,bUnscaled2,'c',lwt,lw1)
%hold on;
%plot(z2,bScaled2,'b',lwt,lw2)
plot(z2,ssaUnscaled2,'c',lwt,lw1)
hold on;
plot(z2,ssaScaled2,'m',lwt,lw2)
%title(['Wavelength = ',num2str(wl2),' nm'])
xlabel('Depth [m]')
%ylabel('Scattering coefficient [1/m]')
%hl=legend('scattering unscaled','delta-fit scaled');
ylabel('Single-scattering albedo')
hl=legend('SSA','delta-fit scaled');
set(gca,'ylim',[0,1.2])
%xlim([0 100000])
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%
subplot(2,2,3)
plot(z2,gScaled2,'k',lwt,lw1)
hold on;
plot(z2,scalingFactor2,'g',lwt,lw2)
%title(['Wavelength = ',num2str(wl2),' nm'])
xlabel('Depth [m]')
ylabel('Asymmetry factor')
hl=legend('asymmetry factor scaled','scaling factor');
set(hl,'fontsize',10)
%xlim([0 100000])
legend boxoff
grid on
set(gca,'ylim',[0,1.2])
%%%%%%%
subplot(2,2,4)
plot(z2,a2,'r',lwt,lw2)
hold on;
plot(z2,bUnscaled2,'c',lwt,lw1)
hold on;
plot(z2,bScaled2,'b',lwt,lw2)
%title(['Wavelength = ',num2str(wl2),' nm'])
xlabel('Depth [m]')
ylabel('Coefficient [1/m]')
hl=legend('absorption','scattering','scattering scaled');
set(hl,'fontsize',10)
xlim([0 100000])
legend boxoff
grid on
%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% d) Cumulative optical depths for atmosphere/ocean at all wavelengths
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
subplot(2,1,1)
plot(wl,atmTauCumUnscaled,'c',lwt,lw1)
hold on;
plot(wl,atmTauCumScaled,'k',lwt,lw2)
title(['Atmosphere cumulative optical depth up to layer ',num2str(atmLayerNo)])
xlabel('Wavelength [nm]')
ylabel('Optical depth')
hl=legend('tau cum unscaled','delta-fit scaled');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%
subplot(2,1,2)
plot(wl,oceanTauCumUnscaled,'c',lwt,lw1)
hold on;
plot(wl,oceanTauCumScaled,'k',lwt,lw2)
title(['Ocean cumulative optical depth up to layer ',num2str(numLayers)])
xlabel('Wavelength [nm]')
ylabel('Optical depth')
hl=legend('tau cum unscaled','delta-fit scaled');
set(hl,'fontsize',10)
legend boxoff
grid on
%%%%%%%
