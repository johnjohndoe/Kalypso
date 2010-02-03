%% contour detection
gaja3d = kalypso.Gaja3D();
gaja3d.setBoundaries('P:\transfer\stefan\TIN-Punkte-HH-Hafen\NEU\area_00_Grenze_pr50cm.shp');
gaja3d.setElevationPoints('P:\transfer\stefan\TIN-Punkte-HH-Hafen\NEU\tin_solltiefen_linien_bk_wei.shp');

% create grid with low resolution and make it smooth
res = 5;
smooth = 4.5;
gaja3d.createGrid('gridx',res,'gridy',res);
grid = gaja3d.demGrid(1);
grid.setSmoothFilter('smoothFilter','bilateral','smooth',smooth);

% detect contours in smoothed grid
interval = -16:2:8;
[rangex, rangey] = grid.getRangeXY();
c = contourc(rangex,rangey,grid.smoothed,interval);
cline = contour2curve(c);

% remove short lines
minlength = 50;
cline_rm50 = cline.discardSmall(minlength);

% clip on buffered boundary (2m)
bound = gaja3d.boundaries;
boundbuf = bound.buffer(2);
clineclip = cline_rm50.clip(boundbuf);

% simplify (point remove)
tolerance = 1.5;
clinesimp = clineclip.simplify(tolerance);

% snap to boundary
snapDist = 2;
[clinesnap, boundsnap] = clinesimp.snap(bound, snapDist);

% optionally save contours to file
% saveContoursAsShape('clinesnap',clinesnap);

%% tin creation based on contours and snapped boundary
gaja3d2 = kalypso.Gaja3D();
gaja3d2.setBoundaries(boundsnap);
gaja3d2.setElevationPoints('P:\transfer\stefan\TIN-Punkte-HH-Hafen\NEU\tin_solltiefen_linien_bk_wei.shp');
gaja3d2.setBreaklines({clinesnap});
gaja3d2.createTin('minAngle',15,'maxArea',1000);
% use contour elevations in model tin
gaja3d2.assignElevations('source','tin','useContour','true');