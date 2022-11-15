clear

load('../data/2020_07_connectomes90_AllFC.mat')

rng(42)

ss = readtable('../data/subs_pop.csv');
ss = table2cell(ss);

is_sub = zeros(1,size(sub,2));
tes = zeros(1,size(ss,1));

for i = 1:size(sub,2)
    
    if any(strcmp(ss(:,1),sub{i})) && any(strcmp(ss(:,2),ses{i}))
        
        
        tes(find(strcmp(ss(:,1),sub{i}))) = 1;
        is_sub(1,i) = 1;

    end
    
end

sub = sub(logical(is_sub));
ses = ses(logical(is_sub));
meanBOLDAll = meanBOLDAll(:,:,logical(is_sub));
HR = HR(logical(is_sub));
ga = ga(logical(is_sub));
pma = pma(logical(is_sub));
sex = sex(logical(is_sub));
dvarsAll = dvarsAll(logical(is_sub));
dvarsAllOutliers = dvarsAllOutliers(logical(is_sub));

save('../data/2020_04_MASTER_connectomes90_All_select')

for k=1:size(meanBOLDAll,3)
    k
    for j = 1:size(meanBOLDAll,2)
        
        ix = randperm(length(meanBOLDAll(:,j,k)));
        meanBOLDAll(:,j,k) = meanBOLDAll(ix,j,k);
          
    end
end

save('../data/2020_04_MASTER_connectomes90_All_surr')
