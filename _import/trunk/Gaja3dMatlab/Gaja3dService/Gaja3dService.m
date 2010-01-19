function Gaja3dService(varargin)
    %set Java classpath
    javaaddpath jts-1.10.jar; %Java Topology Suite
    javaaddpath gaja3d-1.0.jar; %Java helper functions
    javaaddpath deegree2.jar; %deegree2 API
    javaaddpath jai_codec.jar;
    javaaddpath jai_core.jar;
    javaaddpath geotiff-jai.jar;
    javaaddpath vecmath.jar;
    javaaddpath log4j-1.2.9.jar;
    
    % call Gaja3D in batch mode
    gaja3d = kalypso.Gaja3D();
    
    % convert from strings to doubles
    disp(strvcat('Gaja3d command line arguments:', char(varargin))); %#ok<VCAT>
    varargin = convertArguments(varargin{:});
    
    % parse inputs, keep defaults where unspecified
    p = inputParser;
    p.KeepUnmatched = true;

    p.addParamValue('bufferTin', gaja3d.bufferTin, @isnumeric);
    p.addParamValue('bufferGrid', gaja3d.bufferGrid, @isnumeric);

    % tiles separated by comma or empty array
    p.addParamValue('tiles', gaja3d.tiles, @isnumeric);

    % grids and tins as well as boundaries (e.g. *.shp)
    % will be matched to tiles in alphabetical order
    p.addParamValue('boundaries', '', @ischar);
    p.addParamValue('demPoints', '', @ischar);
    p.addParamValue('demGrid', '', @ischar);
    p.addParamValue('breaklines', '', @ischar);

    % which operations to perform automatically
    p.addParamValue('createGrid', 'false');
    p.addParamValue('saveGrid', 'false');
    p.addParamValue('detectBreaklines', 'false');
    p.addParamValue('saveBreaklines', 'false');
    p.addParamValue('createTin', 'false');
    p.addParamValue('assignElevations', 'false');
    p.addParamValue('refineTin', 'false');
    p.addParamValue('saveTin', 'false');
    p.addParamValue('tinFormat', 'shp'); % shp or 2d

    p.parse(varargin{:});

    gaja3d.bufferTin = p.Results.bufferTin;
    gaja3d.bufferGrid = p.Results.bufferGrid;
    
    % preload boundaries
    boundariesSpec = p.Results.boundaries;
    if(~isempty(boundariesSpec))
        bFile = boundariesSpec;
        if(exist(bFile, 'file'))
            fprintf(1, 'Loading boundaries %s...\n', bFile);
            gaja3d.setBoundaries(bFile);
            fprintf(1, 'Loaded boundary with %d tiles.\n', numel(gaja3d.boundaries));
        else
            error('Boundary file %s could not be found!', bFile);
        end
    end

    pointSpec = p.Results.demPoints;
    if(~isempty(pointSpec))
        if(isempty(gaja3d.boundaries))
            error('Must specify boundaries for triangulation of elevation points.');
        end
        pFiles = pointSpec;
        fprintf(1, 'Specified points %80s.\n', pFiles);
        pointFiles = dir(pFiles);
        if(~isempty(pointFiles))
            fprintf(1, '%d files matched:\n%s\n', numel(pointFiles), char({pointFiles.name})');
        else
            error('No point files found that match %s', pFiles);
        end
    end

    gridSpec = p.Results.demGrid;
    if(~isempty(gridSpec))
        if(~isempty(pointSpec))
            error('Cannot specify both grids and points.');
        end
        gFiles = gridSpec;
        fprintf(1, 'Specified grids %s.\n', gFiles);
        gridFiles = dir(gFiles);
        if(~isempty(gridFiles))
            fprintf(1, '%d files matched:\n%s\n', numel(gridFiles), char({gridFiles.name})');
        else
            error('No grid files found that match %s.', gFiles);
        end
    end
    
    % determine tiles to process
    if(~isempty(p.Results.tiles))
        % process given tiles
        gaja3d.tiles = p.Results.tiles;
    end

    if(~isempty(gridSpec))
        if(~isempty(gaja3d.tiles))
            noGrid = gaja3d.tiles > numel(gridFiles);
            if(any(noGrid))
                error('No grid for tile(s) %s!', mat2str(gaja3d.tiles(noGrid)));
            end
        else
            % process all grids
            gaja3d.tiles = 1:numel(gridFiles);
        end
    end

    noBoundary = gaja3d.tiles > numel(gaja3d.boundaries);
    if(any(noBoundary))
        error('No boundary for tile(s) %s!', mat2str(gaja3d.tiles(noBoundary)));
    end

    if(~isempty(gaja3d.tiles))
        fprintf(1, 'Tiles: %s\n', mat2str(gaja3d.tiles));
    end

    if(~isempty(pointSpec))
        % preload points
        allPointsCell = cell(numel(pointFiles),1);
        for i=1:numel(pointFiles)
            file = pointFiles(i);
            fprintf(1, 'Loading point data from %s...\n', file.name);
            pointFileName = [fileparts(pFiles) filesep file.name];
            allPointsCell{i} = loadPointData(pointFileName);
            pointCount = size(allPointsCell{i},1);
            fprintf(1, 'Loaded %d points.\n', pointCount);
        end
        if(~isempty(allPointsCell))
            allPoints = vertcat(allPointsCell{:});
            totalPointCount = size(allPoints,1);
            fprintf(1, 'Total number of points loaded is %d.\n', totalPointCount);
            disp('Triangulating elevation surfaces for all tiles...');
            gaja3d.setElevationPoints(allPoints);
            for i=gaja3d.tiles
                tin = gaja3d.demTin(i);
                pointCount = size(tin.X,1);
                fprintf(1, 'Tile %d has %d points.\n', i, pointCount);
            end
        end
    end

    if(~isempty(gridSpec))
        theGrids = cell(numel(gridFiles),1);
        % preload grids
        for i=gaja3d.tiles;
            if(i <= numel(gridFiles))
                file = gridFiles(i);
                filename = [fileparts(gFiles) filesep file.name];
                fprintf(1, 'Loading grid %s...\n', filename);
                theGrids{i} = kalypso.RectifiedGridCoverage(filename, varargin{:});
            end
        end
        gaja3d.setElevationGrid(theGrids);
    end

    % preload breaklines
    breaklinesSpec = p.Results.breaklines;
    if(~isempty(breaklinesSpec))
        brFile = breaklinesSpec;
        if(exist(brFile, 'file'))
            fprintf(1, 'Loading breaklines %s...\n', brFile);
            gaja3d.setBreaklines(brFile);
        else
            error('Breaklines file %s could not be found!', brFile);
        end
    end

    if(strcmpi('true', p.Results.createGrid))
        disp('Creating grid...');
        gaja3d.createGrid(varargin{:});
    end
    
    if(strcmpi('true', p.Results.saveGrid))
        % save all grids
        for i=gaja3d.tiles
            grid = gaja3d.demGrid(i);
            if(numel(gaja3d.tiles) == 1 && i == 1)
                gridFile = 'DemGrid';
            else
                gridFile = sprintf('DemGrid_%04d',i);
            end
            disp(['Saving grid file ' gridFile]);
            saveAsciiGrid([gridFile '.asc'], grid);
            zip([gridFile '.zip'], {[gridFile '.asc']});
        end
    end    

    if(strcmpi('true', p.Results.detectBreaklines))
        disp('Detecting breaklines...');
        gaja3d.setSmoothFilter(varargin{:});
        gaja3d.setFeatureDetector(varargin{:});
        gaja3d.detectBreaklines(varargin{:});
    end
    
    if(strcmpi('true', p.Results.saveBreaklines))
        % save all breaklines
        for i=gaja3d.tiles
            bs = gaja3d.breaklines{i}.asGeostruct();
            if(~isempty(bs))
                if(numel(gaja3d.tiles) == 1 && i == 1)
                  shpFileName = 'Breaklines';
                else
                  shpFileName = sprintf('Breaklines_%04d', i);
                end
                fprintf(1,'Saving breaklines file %s.\n', [shpFileName '.shp']);
                shapewrite(bs, [shpFileName '.shp'], 'DbfSpec', makedbfspec(bs));
                zip([shpFileName '.zip'], {[shpFileName '.shp'], [shpFileName '.dbf'], [shpFileName '.shx']});
            end
        end
    end

    if(strcmpi('true', p.Results.createTin))
        disp('Creating tin...');
        gaja3d.createTin(varargin{:});

        % assign elevations after tin creation
        if(strcmpi('true', p.Results.assignElevations))
            disp('Assigning elevations...');
            gaja3d.assignElevations(varargin{:});
        end
    end

    if(strcmpi('true', p.Results.refineTin))
        disp('Refining tin...');
        gaja3d.refineTin(varargin{:});

        % assign elevations after tin refinement
        if(strcmpi('true', p.Results.assignElevations))
            disp('Assigning elevations...');
            gaja3d.assignElevations(varargin{:});
        end
    end
    
    if(strcmpi('true', p.Results.saveTin))
        % save model tin
        SHPEXT = 'shp';
        BCE2DEXT = '2d';
        filebase = 'ModelTin';
        points = gaja3d.modelTin.points;
        Xtri = points(:,1);
        Ytri = points(:,2);
        Ztri = points(:,3);
        elements = gaja3d.modelTin.elements;
        switch(lower(p.Results.tinFormat))
            case SHPEXT
                fprintf(1,'Saving model TIN shapefile %s.\n', [filebase '.shp']);
                saveTrianglesAsShape(filebase, elements, Xtri, Ytri);
                zip([filebase '.zip'], {[filebase '.shp'], [filebase '.dbf'], [filebase '.shx']});
            case BCE2DEXT
                filename = [filebase '.2d'];
                fprintf(1,'Saving model TIN BCE 2d file %s.\n', filename);
                [xshift, yshift] = saveTrianglesAsBce2d(filename, elements, Xtri, Ytri, Ztri);
                fprintf(1,'Shifted coordinates by X %f and Y %f.\n', xshift, yshift);
            otherwise
                error('Unknown tin format %s.', p.Results.tinFormat);
        end
    end
    
    disp('Done.');
end
