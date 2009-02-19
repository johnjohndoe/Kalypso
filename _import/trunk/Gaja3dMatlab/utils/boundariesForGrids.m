%basedir = 'd:\Daten\Laserscan_2007';
%basedir = 'D:\Temp\DGM10X10_Koordinaten';
basedir = 'd:\Daten\laser_only';
%basedir = 'P:\FE_Projekte\2006_Neufelder_Watt\17_GIS\shape\Hoehen_final\vor_merged';
%zieldir = 'P:\FE_Projekte\2006_Neufelder_Watt\17_GIS\shape\Hoehen_final\merged';

%peil1file = 'peilungen_2004.dbf';
%peil2file = 'peilungen_-240.dbf';

% dbf = loadDbf([basedir filesep peil1file]);
% xs = strvcat(dbf{:,1});
% ys = strvcat(dbf{:,2});
% zs = strvcat(dbf{:,3});
% dgm = zeros(size(xs, 1),3);
% dgm(:,1) = str2double(xs);
% dgm(:,2) = str2double(ys);
% dgm(:,3) = str2double(zs);
% punkte_peil1 = dgm; 

% dbf = loadDbf([basedir filesep peil2file]);
% xs = strvcat(dbf{:,1});
% ys = strvcat(dbf{:,2});
% zs = strvcat(dbf{:,3});
% dgm = zeros(size(xs, 1),3);
% dgm(:,1) = str2num(xs);
% dgm(:,2) = str2num(ys);
% dgm(:,3) = str2num(zs);
% punkte_peil2 = dgm;
%load('peil1.mat');
%load('peil2.mat');

files = dir([basedir '\dgm-*.shp']);
%dgms = cell(numel(files),1);
bc = 0;
for i=1:numel(files);
    file = files(i);
    filename = [basedir filesep file.name];
    disp(filename);
    [pathstr, name, ext] = fileparts(filename);
    basename = [pathstr filesep name];
    %zieldatei = [zieldir filesep name '_merged'];
    %if(exist([zieldatei '.shp'],'file'))
    %    continue;
    %end
    
    %dgm = loadPointData(filename);
    %dbf = loadDbf(filename);
    %punkte_kachel = loadPointShape(filename);
    
%     minx = min(punkte_kachel(:,1));
%     maxx = max(punkte_kachel(:,1));
%     miny = min(punkte_kachel(:,2));
%     maxy = max(punkte_kachel(:,2));
    
    minxstr = name(14:17);
    minystr = name(18:21);
    
    minx = str2num(minxstr) * 1000 - 0.5;
    maxx = minx + 2000;
    
    miny = str2num(minystr) * 1000 - 0.5;
    maxy = miny + 2000;
    
    X = [minx maxx maxx minx minx];
    Y = [miny miny maxy maxy miny];
    boundary = org.kalypso.gaja3d.matlab.Polygon(X, Y);
    boundary = boundary.clip(outer);
    if(~isempty(boundary.getX()))
        bc = bc + 1;
        boundaries(bc) = boundary;
    else
        disp('empty');
    end
    
    %r = org.kalypso.gaja3d.matlab.RectifiedGridCoverage.fromPoints(loadPointData(filename), 5, 5);
            
%     xcond = punkte_peil1(:,1) >= minx & punkte_peil1(:,1) <= maxx;
%     ycond = punkte_peil1(:,2) >= miny & punkte_peil1(:,2) <= maxy;
%     punkte_peil1_k = punkte_peil1(xcond & ycond,:);
%     
%     xcond = punkte_peil2(:,1) >= minx & punkte_peil2(:,1) <= maxx;
%     ycond = punkte_peil2(:,2) >= miny & punkte_peil2(:,2) <= maxy;
%     punkte_peil2_k = punkte_peil2(xcond & ycond,:);
%     
%     punkte_k = [punkte_kachel; punkte_peil1_k; punkte_peil2_k];
    
    
%     savePointShape(zieldatei, punkte_k);
    
%     fid = fopen([zieldatei '.txt'], 'w');
%     fprintf(fid, '%-20.7f %-20.7f %-20.7f\n', punkte_k');
%     fclose(fid);
    
    %xs = vertcat(dbf{:,1});
    %ys = vertcat(dbf{:,2});
    %zs = strvcat(dbf{:,3});
    %dgm = zeros(size(xs, 1),3);
    %dgm(:,1) = str2num(xs);
    %dgm(:,2) = str2num(ys);
    %dgm(:,3) = str2num(zs);
    %dgms{i} = dgm;

%     try
%         grid = org.kalypso.gaja3d.matlab.RectifiedGridCoverage.fromPoints(dgm);
%         saveAsciiGrid(grid, [basename '.asc']);
%     catch e
%         disp(sprintf('Could not construct grid for %s', basename));
%         savePointShape(basename, dgm(:,1:3));
%     end
end
