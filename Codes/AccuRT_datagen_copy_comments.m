clear
close all
clc;

input=load('Random_input.txt');  %my input file folders should have input and this matlab code.
ncase=size(input,1);  %in my case this is 1000000 automatic

% manual parallel setting
% manually make copies of the folder and change 'ipara' in each folder
npara=10;   %this is number of AccuRT runing at the same time, create 10 identical good to have numbers 1-10 for folder names. and put his file into each folder and the input file 
seg=ncase/npara;
ipara=1;  %change this to run number 1-10 for folder name

%open files to save data
fid_output=fopen(['Input_Irradiance_p' sprintf('%i',ipara) '.txt'],'w');  %

tic;    
for icase=1:2  %(ipara-1)*seg+1:ipara*seg  %now it is set two 2 cases to just test-run.  %sequence for the columns of my random input file.
    solz=input(icase,1);
    vf=input(icase,2);
    af=input(icase,3);
    rh=input(icase,4);
    ozone=input(icase,5);
    %################################################################################################################
%  modify configuration files
    % Step 1: modify main configuration
    fid = fopen('./main_configuration');  %change to my config file name
    n=1;
    while(1)
        lin = fgetl(fid);
        if ~ischar(lin), break, end
        l{n} = lin;
        n = n + 1;
    end
    fclose(fid);

    l{36}=['SOURCE_ZENITH_ANGLE = ' sprintf('%0.6f ',solz)]; % modify solz  %check if line number is correct
    fid = fopen('./main_configuration','w');
    for k=1:length(l)
        fprintf(fid,'%s\n',l{k});
    end
    fclose(fid);
    clear l
 
   % Step 2: modify atmosphere profile configuration
    fid = fopen('./MainMaterials/earth_atmospheric_gases');  %change file name
    n=1;
    while(1)
        lin = fgetl(fid);
        if ~ischar(lin), break, end
        l{n} = lin;
        n = n + 1;
    end
    fclose(fid);
    l{53}=['F_O3        = ' sprintf('%0.6f',ozone) '# Both for gasIop and air']; % modify vf  %check line mnumber
    fid = fopen('./MainMaterials/earth_atmospheric_gases','w');
    for k=1:length(l)
        fprintf(fid,'%s\n',l{k});
    end
    fclose(fid);
    clear l

    % step 3: modify aerosol configure file
    fid = fopen('./aerosolMaterials/aerosols');  %change file name
    n=1;
    while(1)
        lin = fgetl(fid);
        if ~ischar(lin), break, end
        l{n} = lin;
        n = n + 1;
    end
    fclose(fid);
    l{58}=['MATERIAL_PROFILE = 14 ' sprintf('%0.6f',vf) 'e-11']; % modify vf    only the number before e-11
    l{193}=['FINE_MODE_FRACTION = ' sprintf('%0.6f',af)]; % modify af     check line numbers
    l{206}=['RELATIVE_HUMIDITY = ' sprintf('%0.6f',rh/100)]; % modify rh
    fid = fopen('./MainMaterials/aerosols','w');
    for k=1:length(l)
        fprintf(fid,'%s\n',l{k});
    end
    fclose(fid);
    clear l
    
    % run AccuRT
    unix('AccuRT Main');  %change to my main config file name
    
    % read iop, aod and irradiance
   % rad=readRadiance('./MainOutput/radiance.txt');
    irrad=readIrradiance('./MainOutput/cosine_irradiance_total_downward.txt');  %change file name

    iop=readIops('./MainOutput/iops.txt');    %cshange file name
    aod=(iop.absorptionCoefficients(14)+iop.scatteringCoefficients(14))*(iop.layerDepths(14)-iop.layerDepths(13)); 
    
    %###################################################################################################
    write out aod irradiance
    %###################################################################################################

    fprintf(fid_output,repmat('%16.8e ',1,12),solz,aod,af,fv,rh,ozone,irrad(?,?);   %check what? output layer and how many channel where 12 is.
    fprintf(fid_output,'\n');

toc;
fclose(fid_output);





