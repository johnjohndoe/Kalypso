function [ grid ] = loadGrid( file )
    if(~exist('file', 'var'))
        error('Must specify a filename.');
    elseif(~exist(file, 'file'))
        error('%s is not a valid filename or file does not exist.', file);
    end
    
    TIFEXT = '.tif';
    TIFFEXT = '.tiff';
    ASCEXT = '.asc';
    
    [pathstr, name, ext] = fileparts(file);

    switch(ext)
        case {TIFEXT, TIFFEXT}
            % read image file
            grid.Z = double(imread(file));
            % load world file (referencing matrix)
            worldfile = getworldfilename(file);
            grid.refmat = worldfileread(worldfile);
        case {ASCEXT}
            % read arcview ascii grid file
            [grid.Z, grid.refmat] = arcgridread(file);            
        otherwise
            error('File extension %s not recognized: ', ext);
    end    
    
    % instead of using meshgrid
    x = 1:size(grid.Z, 1);
    y = 1:size(grid.Z, 2);
    for i=x
        for j=y
            xy = [i j 1] * grid.refmat;
            grid.X(i,j) = xy(1);
            grid.Y(i,j) = xy(2);
        end
    end
    
    p11 = [1 1 1] * grid.refmat; % upper left pixel
    p22 = [2 2 1] * grid.refmat; % second pixel
    step =  p22 - p11;
    
    % some statistics
    grid.minx = min(min(grid.X));
    grid.miny = min(min(grid.Y));
    grid.maxx = max(max(grid.X));
    grid.maxy = max(max(grid.Y));
    grid.size.X = abs(step(1));
    grid.size.Y = abs(step(2));
%%
end %LOADGRID