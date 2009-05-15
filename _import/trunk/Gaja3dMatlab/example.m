% set up Gaja3D workspace
set_classpath
gaja3d = kalypso.Gaja3D();

% load data
gaja3d.setBoundaries('example/boundaries.shp');
gaja3d.setElevationPoints('example/points.shp');

% create grid from points with dx=dy=5
gaja3d.createGrid('gridx', 1, 'gridy', 1);
figure(1);
mapshow(gaja3d.demGrid.Z, gaja3d.demGrid.refmat, 'DisplayType', 'texturemap');

% set breakline detection parameters
gaja3d.setEdgeFilter('edgeFilter', 'ood');
gaja3d.setSmoothFilter('smoothFilter', 'gaussian', 'smooth', 8);
gaja3d.setFeatureDetector('featureDetector', 'simple', 'lowThresh', 17, 'highThresh', 18, 'morph_ops', 'clean,~bridge,~diag,thicken(7),~thicken(4),thicken(1),~spur(5),spur(Inf)', 'bp_remove', 400, 'bp_hole_remove', 0);
figure(2);
mapshow(gaja3d.demGrid.breakpoints, gaja3d.demGrid.refmat, 'DisplayType', 'image');

% detect breaklines
gaja3d.detectBreaklines('distanceTolerance', 2.5);
mapshow(gaja3d.boundaries.asGeostruct, 'FaceColor', 'none', 'EdgeColor', 'red');
mapshow(gaja3d.breaklinesMerged.asGeostruct, 'Color', 'red');

% create mesh
gaja3d.createTin('maxArea', 3000, 'minAngle', 15);
points = gaja3d.modelTin.points;
elements = gaja3d.modelTin.elements;
figure(1);
hold on
trimesh(elements, points(:,1), points(:,2), 'Color', 'black');

% assign elevations and display
gaja3d.assignElevations();
points = gaja3d.modelTin.points;
figure(3);
trisurf(elements, points(:,1), points(:,2), points(:,3), 'FaceColor', 'interp', 'EdgeColor', 'black');