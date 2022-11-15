clear

load('../data/2020_07_connectomes90_AllFC.mat')
lowVol = sum(volBOLDAll<5);
volBOLD = sum(volBOLDAll);

volBOLD(810) = [];
lowVol(810) = [];

writecell(sub','../data/full_subs.csv')
writecell(ses','../data/full_ses.csv')
writematrix(lowVol','../data/lowVol_subs.csv')
writematrix(volBOLD','../data/volBOLD.csv')