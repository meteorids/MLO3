clear all;
close all;
clc;

setenv('ACCURT_PATH', '/usr/local/AccuRT')
ldpath=getenv('LD_LIBRARY_PATH');
if(contains(ldpath(1:100), ':/usr/local/AccuRT/lib')==0)
    setenv('LD_LIBRARY_PATH', ['/usr/lib' ':/usr/local/lib' ':/usr/local/AccuRT/lib:' getenv('LD_LIBRARY_PATH')]);
end

input=load('generated_input.txt');
ncase=size(input,1);

% manual parallel setting
% manually make copies of the folder and change 'ipara' in each folder
npara=5;
seg=ncase/npara;
ipara=1;

%open files to save data
fid_output=fopen(['Input_Irradiance_p' sprintf('%i',ipara) '.txt'],'w');

tic;
for icase=(ipara-1)*seg+1:ipara*seg  
    solz=input(icase,1);
    ozone=input(icase,2);
    vf=input(icase,3);
    %rh=input(icase,4);
    %ozone=input(icase,5);
    %fprintf('%f %f %f %f %f /n',solz,vf,af,rh,ozone)   
    %################################################################################################################
%  modify configuration files
    % Step 1: modify main configuration
    fid = fopen('./O3COD');
    n=1;
    while(1)
        lin = fgetl(fid);
        if ~ischar(lin), break, end
        l{n} = lin;
        n = n + 1;
    end
    fclose(fid);

    l{36}=['SOURCE_ZENITH_ANGLE = ' sprintf('%0.6f ',solz)]; % modify solz
    fid = fopen('./O3COD','w');
    for k=1:length(l)
        fprintf(fid,'%s\n',l{k});
    end
    fclose(fid);
    clear l

   % Step 2: modify atmosphere profile configuration
    fid = fopen('./O3CODMaterials/earth_atmospheric_gases');
    n=1;
    while(1)
        lin = fgetl(fid);
        if ~ischar(lin), break, end
        l{n} = lin;
        n = n + 1;
    end
    fclose(fid);
    l{53}=['F_O3        = ' sprintf('%12.6e',ozone) '# Both for gasIop and air']; % modify vf
    fid = fopen('./O3CODMaterials/earth_atmospheric_gases','w');
    for k=1:length(l)
        fprintf(fid,'%s\n',l{k});
    end
    fclose(fid);
    clear l

    % step 3: modify cloud file
    fid = fopen('./O3CODMaterials/cloud');
    n=1;
    while(1)
        lin = fgetl(fid);
        if ~ischar(lin), break, end
        l{n} = lin;
        n = n + 1;
    end
    fclose(fid);
    l{47}=['CLOUD_PROFILE = 13 ' sprintf('%6.6f',vf) 'e-7']; % modify vf
    fid = fopen('./O3CODMaterials/cloud','w');
    for k=1:length(l)
        fprintf(fid,'%s\n',l{k});
    end
    fclose(fid);
    clear l    

    % step 3: modify aerosol configure file
    %fid = fopen('./NNMaterials/aerosols');
    %n=1;
    %while(1)
    %    lin = fgetl(fid);
    %    if ~ischar(lin), break, end
    %    l{n} = lin;
    %    n = n + 1;
    %end
    %fclose(fid);
    %l{58}=['MATERIAL_PROFILE = 14 ' sprintf('%0.6f',vf) 'e-11']; % modify vf
    %l{193}=['FINE_MODE_FRACTION = ' sprintf('%0.6f',af)]; % modify af
    %l{206}=['RELATIVE_HUMIDITY = ' sprintf('%0.6f',rh)]; % modify rh
    %fid = fopen('./NNMaterials/aerosols','w');
    %for k=1:length(l)
    %    fprintf(fid,'%s\n',l{k});
    %end
    %fclose(fid);
    %clear l


    % run AccuRT
    system('AccuRT O3COD');

    % read iop, aod and irradiance
    % rad=readRadiance('./NNOutput/radiance.txt');
    irrad=readIrradiance('./O3CODOutput/cosine_irradiance_total_downward.txt');

    iop=readIops('./O3CODOutput/iops.txt');
    %aod=(iop.absorptionCoefficients(14,6)+iop.scatteringCoefficients(14,6))*(iop.layerDepths(14)-iop.layerDepths(13));

    %###################################################################################################
    %write out aod irradiance
    %###################################################################################################

    irrad1=[solz ozone vf irrad.irradiance(2,:)];
    fprintf(fid_output,repmat('%16.8e ',1,4),irrad1);
    %fprintf(fid_output,repmat('%16.8e ',1,13),solz,aod,af,vf,rh,ozone,irrad1);
    fprintf(fid_output,'\n');

end
toc;
 fclose(fid_output);
