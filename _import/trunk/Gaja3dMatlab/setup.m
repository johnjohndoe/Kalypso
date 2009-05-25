function gaja3d = setup(boundaries, grid)
    % set up Gaja3D workspace
    gaja3d = kalypso.Gaja3D();

    % load data
    gaja3d.setBoundaries(boundaries);
    gaja3d.setElevationGrid(grid);
end