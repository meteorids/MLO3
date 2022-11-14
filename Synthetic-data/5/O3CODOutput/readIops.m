% iops = readIops() reads stored inherent optical properites (IOPs) for each
% computed lower boundary layer geometrical depth [m] and each wavelength
% [nm]. The IOPs are: total optical depth, scattering coefficient [1/m],
% absorption coefficient [1/m], and phase moments for all wavelengths and
% all layers. See also plotIops.m

function iops = readIops(fileName)

  fid = fopen(fileName);

  % AccuRT may have been run repeatedly with varying configurations 
  nRuns = fscanf(fid,'%i',1);
  
  for rNo = 1:nRuns
    nLayerDepths  = fscanf(fid,'%i',1);
    nWavelengths  = fscanf(fid,'%i',1);
    nPhaseMoments = fscanf(fid,'%i',1);
  
    for i = 1:nLayerDepths
      iops(rNo).layerDepths(i) = fscanf(fid,'%g',1);
    end

    for i = 1:nWavelengths
      iops(rNo).wavelengths(i) = fscanf(fid,'%g',1);
    end

  
    for i = 1:nLayerDepths
      for j = 1:nWavelengths
	iops(rNo).totalOpticalDepths(i,j) = fscanf(fid,'%g',1);
	iops(rNo).absorptionCoefficients(i,j) = fscanf(fid,'%g',1);
	iops(rNo).scatteringCoefficients(i,j) = fscanf(fid,'%g',1);
	iops(rNo).scatteringScalingFactor(i,j) = fscanf(fid,'%g',1);
	for k=1:nPhaseMoments
	  iops(rNo).phaseMoments(i,j,k) = fscanf(fid,'%g',1);
	end
      end
    end
  end
  fclose(fid);
  
