%Default AAL atlas:
AALnii=load_nii('aal.nii');

%Number of regions:
Naal=90;

centr = load('../data/centroids_LEiDa_5.csv');
centr = transpose(centr);

centr1 = centr(:,1)/max(abs(centr(:,1)));
centr2 = centr(:,2)/max(abs(centr(:,2)));
centr3 = centr(:,3)/max(abs(centr(:,3)));
centr4 = centr(:,4)/max(abs(centr(:,4)));
centr5 = centr(:,5)/max(abs(centr(:,5)));

aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k5_1.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr1(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k5_2.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr2(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k5_3.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr3(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k5_4.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr4(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k5_5.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr5(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Default AAL atlas:
AALnii=load_nii('aal.nii');

%Number of regions:
Naal=90;

centr = load('../data/centroids_LEiDa_4.csv');
centr = transpose(centr);

centr1 = centr(:,1)/max(abs(centr(:,1)));
centr2 = centr(:,2)/max(abs(centr(:,2)));
centr3 = centr(:,3)/max(abs(centr(:,3)));
centr4 = centr(:,4)/max(abs(centr(:,4)));

aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k4_1.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr1(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k4_2.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr2(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k4_3.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr3(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k4_4.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr4(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Default AAL atlas:
AALnii=load_nii('aal.nii');

%Number of regions:
Naal=90;

centr = load('../data/centroids_LEiDa_6.csv');
centr = transpose(centr);

centr1 = centr(:,1)/max(abs(centr(:,1)));
centr2 = centr(:,2)/max(abs(centr(:,2)));
centr3 = centr(:,3)/max(abs(centr(:,3)));
centr4 = centr(:,4)/max(abs(centr(:,4)));
centr5 = centr(:,5)/max(abs(centr(:,5)));
centr6 = centr(:,6)/max(abs(centr(:,6)));

aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k6_1.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr1(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k6_2.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr2(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k6_3.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr3(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k6_4.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr4(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k6_5.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr5(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k6_6.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr6(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Default AAL atlas:
AALnii=load_nii('aal.nii');

%Number of regions:
Naal=90;

centr = load('../data/centroids_LEiDa_7.csv');
centr = transpose(centr);

centr1 = centr(:,1)/max(abs(centr(:,1)));
centr2 = centr(:,2)/max(abs(centr(:,2)));
centr3 = centr(:,3)/max(abs(centr(:,3)));
centr4 = centr(:,4)/max(abs(centr(:,4)));
centr5 = centr(:,5)/max(abs(centr(:,5)));
centr6 = centr(:,6)/max(abs(centr(:,6)));
centr7 = centr(:,7)/max(abs(centr(:,7)));

aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_1.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr1(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_2.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr2(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_3.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr3(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_4.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr4(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_5.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr5(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_6.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr6(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k7_7.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr7(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Default AAL atlas:
AALnii=load_nii('aal.nii');

%Number of regions:
Naal=90;

centr = load('../data/centroids_LEiDa_8.csv');
centr = transpose(centr);

centr1 = centr(:,1)/max(abs(centr(:,1)));
centr2 = centr(:,2)/max(abs(centr(:,2)));
centr3 = centr(:,3)/max(abs(centr(:,3)));
centr4 = centr(:,4)/max(abs(centr(:,4)));
centr5 = centr(:,5)/max(abs(centr(:,5)));
centr6 = centr(:,6)/max(abs(centr(:,6)));
centr7 = centr(:,7)/max(abs(centr(:,7)));
centr8 = centr(:,8)/max(abs(centr(:,8)));

aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_1.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr1(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_2.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr2(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_3.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr3(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_4.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr4(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_5.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr5(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);




aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_6.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr6(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_7.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr7(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);


aux=double(AALnii.img); %make a copy to edit

aux(aux>90)=0; %Put to 0 cerebellar and other areas

% Name of the file to plot:
auxfile='../data/centr_k8_8.nii';

for n=1:Naal % for each region 
    
    aux(AALnii.img == n) = double(centr8(n)); %assign value to specific region

end

%Saving the data:
AALaux=AALnii;
AALaux.img=aux;

% A few changes in headers (probably not needed):
% change the range if needed:

% AALaux.hdr.dime.glmax=1;
% AALaux.hdr.dime.glmin=-1;
% AALaux.original.hdr.dime.glmax=1;
% AALaux.original.hdr.dime.glmin=-1;
AALaux.hdr.dime.datatype=64;
AALaux.hdr.dime.bitpix=64;

save_nii(AALaux,auxfile);

