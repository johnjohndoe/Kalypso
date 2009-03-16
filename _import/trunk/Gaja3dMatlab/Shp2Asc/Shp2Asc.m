function Shp2Asc(varargin)
    %set Java classpath
    javaaddpath jts-1.8.jar; %Java Topology Suite
    javaaddpath gaja3d-1.0.jar; %Java helper functions
    javaaddpath deegree2.jar; %deegree2 API
    javaaddpath jai_codec.jar;
    javaaddpath jai_core.jar;
    javaaddpath geotiff-jai.jar;
    javaaddpath vecmath.jar;
    
    if(nargin < 1)
        error('Usage: Shp2Asc <files> [-hasHeader]');
    end
    
    shpFileSpec = varargin{1};
    if(~isempty(shpFileSpec))
        disp(sprintf('Specified files %s', shpFileSpec));
        shpFiles = dir(shpFileSpec);
        if(~isempty(shpFiles))
            disp(sprintf('%d files matched:', numel(shpFiles)));
            disp(char({shpFiles.name}));
        else
            error('No files found.');
        end
    end
    
    % default to no headers
    hasHeader = 0;
    if(nargin > 1)
        headerArg = varargin{2};
        if(strcmpi(headerArg,'-hasHeader'))
            disp('Assuming files have a header line.')
            hasHeader = 1;
        end
    end
    
    for i=1:numel(shpFiles)
        file = shpFiles(i);
        disp(sprintf('Loading points from %s...', file.name));
        pointFileName = [fileparts(shpFileSpec) filesep file.name];
        points = loadPointData(pointFileName, hasHeader);
        grid = org.kalypso.gaja3d.matlab.RectifiedGridCoverage.fromPoints(points);
        [path, name] = fileparts(pointFileName);
        gridFile = [path filesep name '.asc'];
        gridsize = size(grid.Z);
        disp(sprintf('Grid (%d x %d) resolution %d.', gridsize(1), gridsize(2), grid.dx));
        disp(['Saving ascii grid file ' gridFile])
        saveAsciiGrid(grid, gridFile);
    end
end