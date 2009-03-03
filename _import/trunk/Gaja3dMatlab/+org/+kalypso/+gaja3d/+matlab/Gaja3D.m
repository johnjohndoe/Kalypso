classdef Gaja3D < handle
%GAJA3D 3D Terrain Discretisation Service

    properties (SetAccess = private, SetObservable = true)
        % all file paths are be relative to this directory,
        % defaults to the current directory.
        % Directory will be created if it does not exist.
        workingDir = pwd;
        
        % buffers [meter] around tile boundaries
        bufferTin = 300; % for tin creation from points
        bufferGrid = 150; % for grid creation from tin 
        
        % array of indices determining which boundaries to process
        tiles = [];
        
        % the outer boundaries of the model area tiles
        boundaries = org.kalypso.gaja3d.matlab.Polygon.EMPTY;
        
        % original tin points and triangle indices
        demTin = org.kalypso.gaja3d.matlab.TriangulatedSurface.EMPTY;
        
        % raster elevation model (GRID)
        % single instance of RectifiedGridCoverage
        demGrid = org.kalypso.gaja3d.matlab.RectifiedGridCoverage.EMPTY;
        
        % breaklines for tin generation
        % Curve array
        breaklines = org.kalypso.gaja3d.matlab.Curve.EMPTY;
        
        % All breaklines for tin generation. These may be set manually and
        % will override the other breaklines if present.
        breaklinesMerged = org.kalypso.gaja3d.matlab.Curve.EMPTY;
        
        % generated tin points and triangle indices
        modelTin = org.kalypso.gaja3d.matlab.TriangulatedSurface.EMPTY;
        refineCount = 0;
        
        cmd_args = cell(0);
    end % public properties
        
    methods 
        % set demTin, revert demGrid
        function this = set.demTin(this, varargin)
            if(nargin == 2 && isa(varargin{1},'org.kalypso.gaja3d.matlab.TriangulatedSurface'))
                this.demTin = varargin{1};
            else
                this.demTin = org.kalypso.gaja3d.matlab.TriangulatedSurface(varargin{:});
            end
            this.demGrid = [];
        end 

        % set demGrid, revert breaklines
        function this = set.demGrid(this, varargin)
            if(nargin == 2 && isa(varargin{1},'org.kalypso.gaja3d.matlab.RectifiedGridCoverage'))
                this.demGrid = varargin{1};
            else
                this.demGrid = org.kalypso.gaja3d.matlab.RectifiedGridCoverage(varargin{:});
            end
            this.breaklines = [];
        end

        % set breaklines, revert modelTin
        function this = set.breaklines(this, varargin)
            if(nargin == 2 && iscell(varargin{1}))
                breaklines = varargin{1};
                for i=1:numel(breaklines)
                    if(isa(breaklines{i},'org.kalypso.gaja3d.matlab.Curve'))
                        this.breaklines{i} = breaklines{i};
                    else
                        args = breaklines{i};
                        if(isempty(args))
                            this.breaklines{i} = org.kalypso.gaja3d.matlab.Curve.EMPTY;
                        else
                            this.breaklines(i) = org.kalypso.gaja3d.matlab.Curve(args{:});
                        end
                    end
                end
            elseif(nargin == 2 && isempty(varargin{1}))
                noBreaklines = cell(size(this.tiles));
                this.breaklines = noBreaklines;
            end
            this.breaklinesMerged = org.kalypso.gaja3d.matlab.Curve.EMPTY;
            this.modelTin = org.kalypso.gaja3d.matlab.TriangulatedSurface.EMPTY;
        end
        
        % set breaklines, revert modelTin
        function this = set.boundaries(this, varargin)
            if(nargin == 2 && isa(varargin{1},'org.kalypso.gaja3d.matlab.Polygon'))
                this.boundaries = varargin{1};
            elseif(nargin == 3)
                this.boundaries = griddedBoundaries(varargin{:}); 
            else
                this.boundaries = org.kalypso.gaja3d.matlab.Polygon(varargin{:});
            end
            this.tiles = 1:numel(this.boundaries);
            this.demTin = [];
        end
    end
   
    methods (Access = public)
        % Private constructor
        function this = Gaja3D(varargin)
            % convert from strings to doubles if deployed
            if(isdeployed)
                disp(strvcat('Gaja3d command line arguments:', char(varargin))); %#ok<VCAT>
                varargin = convertArguments(varargin{:});
            end
            
            % parse inputs, keep defaults where unspecified
            p = inputParser;
            p.KeepUnmatched = true;

            p.addParamValue('workingDir', this.workingDir, @ischar);
            p.addParamValue('bufferTin', this.bufferTin, @isnumeric);
            p.addParamValue('bufferGrid', this.bufferGrid, @isnumeric);
            
            % tiles separated by comma or empty array
            p.addParamValue('tiles', this.tiles, @isnumeric);
            
            % grids and tins as well as boundary (e.g. *.shp)
            % will be matched to tiles in alphabetical order
            p.addParamValue('boundary', '', @ischar);
            p.addParamValue('demPoints', '', @ischar);
            p.addParamValue('demTin', '', @ischar);
            p.addParamValue('demGrid', '', @ischar);
            p.addParamValue('breaklines', '', @ischar);
            
            % which operations to perform automatically
            p.addParamValue('createGrid', 'false');
            p.addParamValue('detectBreaklines', 'false');
            p.addParamValue('createTin', 'false');
            p.addParamValue('assignElevations', 'false');
            p.addParamValue('refineTin', 'false');
            
            p.parse(varargin{:});
            
            % set working directory
            this.workingDir = p.Results.workingDir;
            this.bufferTin = p.Results.bufferTin;
            this.bufferGrid = p.Results.bufferGrid;
            
            % preload boundaries
            boundariesSpec = p.Results.boundary;
            if(~isempty(boundariesSpec))
                bFile = fullfile(this.workingDir, boundariesSpec);
                if(exist(bFile, 'file'))
                    disp(sprintf('Loading boundaries %s...', bFile));
                    this.setBoundaries(bFile);
                    disp(sprintf('Loaded boundary with %d tiles', numel(this.boundaries)));
                else
                    error('Boundary file %s could not be found!', bFile);
                end
            end
            
            pointSpec = p.Results.demPoints;
            if(~isempty(pointSpec))
                if(isempty(this.boundaries))
                    error('Must specify boundaries for triangulation of elevation points.');
                end
                pFiles = fullfile(this.workingDir, pointSpec);
                disp(sprintf('Specified points %s', pFiles));
                pointFiles = dir(pFiles);
                if(~isempty(pointFiles))
                    disp(sprintf('%d files matched:', numel(pointFiles)));
                    disp(char({pointFiles.name}));
                else
                    error('No point files found.');
                end
            end
            
            tinSpec = p.Results.demTin;
            if(~isempty(tinSpec))
                if(~isempty(pointSpec))
                    error('Cannot specify both tins and points.');
                end
                tFiles = fullfile(this.workingDir, tinSpec);
                disp(sprintf('Specified tins %s', tFiles));
                tinFiles = dir(tFiles);
                if(~isempty(tinFiles))
                    disp(sprintf('%d files matched:', numel(tinFiles)));
                    disp(char({tinFiles.name}));
                else
                    error('No tin files found.');
                end
            end
            
            gridSpec = p.Results.demGrid;
            if(~isempty(gridSpec))
                if(~isempty(pointSpec))
                    error('Cannot specify both grids and points.');
                end
                if(~isempty(tinSpec))
                    error('Cannot specify both tins and grids.');
                end
                gFiles = fullfile(this.workingDir, gridSpec);
                disp(sprintf('Specified grids %s', gFiles));
                gridFiles = dir(gFiles);
                if(~isempty(gridFiles))
                    disp(sprintf('%d files matched:', numel(gridFiles)));
                    disp(char({gridFiles.name}));
                else
                    error('No grid files found.');
                end
            end
            
            % determine tiles to process
            if(~isempty(p.Results.tiles))
                % process given tiles
                this.tiles = p.Results.tiles;
            end

            if(~isempty(tinSpec))
                if(~isempty(this.tiles))
                    noTin = this.tiles > numel(tinFiles);
                    if(any(noTin))
                        error('No tin for tile(s) %s!', mat2str(this.tiles(noTin)));
                    end
                else
                   % process all tins
                    this.tiles = 1:numel(tinFiles);
                end            
            elseif(~isempty(gridSpec))
                if(~isempty(this.tiles))
                    noGrid = this.tiles > numel(gridFiles);
                    if(any(noGrid))
                        error('No grid for tile(s) %s!', mat2str(this.tiles(noGrid)));
                    end
                else
                    % process all grids
                    this.tiles = 1:numel(gridFiles);
                end
            end
            
            noBoundary = this.tiles > numel(this.boundaries);
            if(any(noBoundary))
                error('No boundary for tile(s) %s!', mat2str(this.tiles(noBoundary)));
            end
            
            if(~isempty(this.tiles))
                disp(sprintf('Tiles: %s', mat2str(this.tiles)));
            end
            
            if(~isempty(pointSpec))
                % preload points
                allPointsCell = cell(numel(pointFiles),1);
                for i=1:numel(pointFiles)
                    file = pointFiles(i);
                    disp(sprintf('Loading point data from %s...', file.name));
                    pointFileName = [fileparts(pFiles) filesep file.name];
                    allPointsCell{i} = loadPointData(pointFileName);
                    pointCount = size(allPointsCell{i},1);
                    disp(sprintf('Loaded %d points.', pointCount));
                end
                if(~isempty(allPointsCell))
                    allPoints = vertcat(allPointsCell{:});
                    totalPointCount = size(allPoints,1);
                    disp(sprintf('Total number of points loaded is %d', totalPointCount));
                    disp('Triangulating elevation surfaces for all tiles...');
                    this.setElevationPoints(allPoints);
                    for i=this.tiles
                        tin = this.demTin(i);
                        pointCount = size(tin.points,1);
                        elementCount = size(tin.elements,1);
                        disp(sprintf('Tin %d has %d points and %d elements.', i, pointCount, elementCount));
                    end
                end
            end
            
            if(~isempty(tinSpec))
                % preload tins
                for i=this.tiles;
                    if(i <= numel(tinFiles))
                        file = tinFiles(i);
                        [pathstr, name, ext] = fileparts(file.name);
                        tinExtensions = {'.ele','.node','.shp'};
                        if(~any(strcmp(ext,tinExtensions)))
                            error('Unrecognized tin file extension %s. Known extensions are: %s', ext, char(tinExtensions)');
                        end
                        eleFileName = [fileparts(tFiles) filesep file.name];
                        disp(sprintf('Loading tin with basename %s...', eleFileName));
                        this.demTin(i) = org.kalypso.gaja3d.matlab.TriangulatedSurface(eleFileName);
                    end
                end
            end
            
            if(~isempty(gridSpec))
                % preload grids
                for i=this.tiles;
                    if(i <= numel(gridFiles))
                        file = gridFiles(i);
                        filename = [fileparts(gFiles) filesep file.name];
                        disp(sprintf('Loading grid %s...', filename));
                        this.demGrid(i) = org.kalypso.gaja3d.matlab.RectifiedGridCoverage(filename, varargin{:});
                    end
                end
            end
            
            % preload breaklines
            breaklinesSpec = p.Results.breaklines;
            if(~isempty(breaklinesSpec))
                brFile = fullfile(this.workingDir, breaklinesSpec);
                if(exist(brFile, 'file'))
                    disp(sprintf('Loading breaklines %s...', brFile));
                    this.setBreaklines(brFile);
                else
                    error('Breaklines file %s could not be found!', brFile);
                end
            end
            
            for i=1:max(this.tiles)
                if(~any(i==this.tiles))
                    this.demTin(i) = org.kalypso.gaja3d.matlab.TriangulatedSurface.EMPTY;
                    this.demGrid(i) = org.kalypso.gaja3d.matlab.RectifiedGridCoverage.EMPTY;
                end
            end
            
            % remember arguments for all calls
            this.cmd_args = varargin;
            if(strcmpi('true', p.Results.createGrid))
                disp('Creating grid...');
                this.createGrid();
            end
            
            if(strcmpi('true', p.Results.detectBreaklines))
                disp('Detecting breaklines...');
                this.setSmoothFilter();
                this.setFeatureDetector();
                this.detectBreaklines();
            end
            
            if(strcmpi('true', p.Results.createTin))
                disp('Creating tin...');
                this.createTin();
                
                % assign elevations after tin creation
                if(strcmpi('true', p.Results.assignElevations))
                    disp('Assigning elevations...');
                    this.assignElevations();
                end
            end
            
            if(strcmpi('true', p.Results.refineTin))
                disp('Refining tin...');
                this.refineTin();
                
                % assign elevations after tin refinement
                if(strcmpi('true', p.Results.assignElevations))
                    disp('Assigning elevations...');
                    this.assignElevations();
                end
            end
        end
        
        function this = setElevationPoints(this, varargin)
            if(isempty(this.boundaries))
                error('must set boundaries first');
            end
            if(nargin < 2)
                error('must specify point or tin data');
            end
            if(nargin >= 2)
                points = varargin{1};
            end
            if(nargin >= 3)
                % if elements is specified, use given tin for all
                % boundaries
                elements = varargin{2};
                tin = org.kalypso.gaja3d.matlab.TriangulatedSurface(points, elements);
                [this.demTin] = deal(tin);
                return;
            else
                workingDir = this.workingDir;
                if(ischar(points))
                    points = loadPointData(points);
                end
                
                % discard duplicate points and NaN elevations and sort
                nanEle = isnan(points(:,3));
                points = points(~nanEle,:);
                [b, m] = unique(points(:,1:2), 'rows');
                points = sortrows(points(m,:),1:2);
                
                
                % triangulate patches with buffer
                for i=this.tiles
                    if(numel(this.tiles) == 1 && i == 1)
                        tempfile = [workingDir filesep 'DemTin'];
                    else
                        tempfile = [workingDir filesep sprintf('DemTin_%04d', i)];
                    end
                    % buffer for tin generation
                    bufDist = this.bufferTin;
                    redPoints = reduceToBoundary(points(:,1:3), this.boundaries(i), bufDist);
                    if(isempty(redPoints))
                        warning('No elevation points in tile %n.', i);
                        tin = org.kalypso.gaja3d.matlab.TriangulatedSurface.EMPTY;
                    else
                        exportNodes(redPoints, tempfile);
                        if(isdeployed())
                            tricommand = [pwd filesep 'exec' filesep 'triangle -N'];
                        else
                            tricommand = [pwd filesep 'exec' filesep 'triangle.exe -N'];
                        end
                        eval(sprintf('! %s %s', tricommand, tempfile));
                        outputPrefix = [tempfile '.1'];
                        elements = loadTriangleOutput( outputPrefix );
                        tin = org.kalypso.gaja3d.matlab.TriangulatedSurface(redPoints, double(elements));
                        movefile([outputPrefix '.ele'], [tempfile '.ele']);
                        zip([tempfile '.zip'], {[tempfile '.ele'], [tempfile '.node']});
                    end
                    this.demTin(i) = tin;
                end
            end 
        end
        
        function this = setElevationGrid(this, grid)
            if(~iscell(grid))
                grid = {grid};
            end
            for i=this.tiles
                this.demGrid(i) = grid{i};
            end
        end
        
        function this = setBreaklines(this, breaklines)
            if(~iscell(breaklines))
                this.breaklinesMerged = org.kalypso.gaja3d.matlab.Curve(breaklines);
            elseif(~all(size(breaklines)==size(this.breaklines)))
                error('Number of breaklines sets does not match number of grids');
            else
                for i=this.tiles
                    this.breaklines(i) = breaklines(i);
                end
                this.breaklinesMerged = [this.breaklines{this.tiles}];
            end
        end
        
        function this = setBoundaries(this, boundaries)
            this.boundaries = boundaries;
        end
        
        % creates a grid from a tin with given resolution
        function this = createGrid(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end
            noTin = isempty(this.demTin(this.tiles));
            if(any(noTin(:)))
                error('For tile(s) %s tin needs to be set first.', mat2str(this.tiles(noTin)));
            end        
            
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('gridx', 5, @isnumeric);
            p.addParamValue('gridy', 5, @isnumeric);
            p.parse(this.cmd_args{:}, varargin{:});
            
            dx = p.Results.gridx;
            dy = p.Results.gridy;
            
            % create grid for all instances
            workingDir = this.workingDir;
            for i=this.tiles
                tin = this.demTin(i);
                points = tin.points;
                elements = tin.elements;
                X = points(:,1);
                Y = points(:,2);
                Z = points(:,3);
                boundary = this.boundaries(i);
                
                %buffer boundary for grid creation
                bufdist = this.bufferGrid;
                boundaryBuffer = boundary.buffer(bufdist);
                
                polyX = boundaryBuffer.getX();
                polyY = boundaryBuffer.getY();
                pointCount = numel(polyX);
                edges = zeros(pointCount - sum(isnan(polyX))*2 - 1, 2);
                currentEdge = 1;
                for j=1:pointCount
                    if(j == pointCount)
                    elseif(isnan(polyX(j)))
                    elseif(isnan(polyX(j+1)))
                    else
                        edges(currentEdge,:) = [j j+1];
                        currentEdge = currentEdge + 1;
                    end
                end
                
                minx = min(boundaryBuffer.getX());
                miny = min(boundaryBuffer.getY());
                
                refmat = constructRefMat(minx, miny, dx, dy);

                % mesh grid
                maxx = max(boundaryBuffer.getX());
                maxy = max(boundaryBuffer.getY());
                rangex = minx:dx:maxx;
                rangey = miny:dy:maxy;
                [gridx, gridy] = meshgrid(rangex, rangey);
                
                % interpolate gridz
                %[gridx, gridy, refmat] = createGrid(points, (change to cell) dx, dy);
                %[gridx, gridy, gridz] = gridfit(X, Y, Z, rangex, rangey);
                %[gridx, gridy, gridz] = griddata(X, Y, Z, gridx, gridy);
                gridz = interptri(elements, X, Y, Z, gridx, gridy);
                inboundary = inpoly([gridx(:) gridy(:)], [polyX polyY], edges);
                %inboundary = inpolygon(gridx, gridy, boundaryBuffer.getX(), boundaryBuffer.getY());
                gridz(~inboundary) = NaN;
                grid = org.kalypso.gaja3d.matlab.RectifiedGridCoverage(gridz, refmat);
                this.demGrid(i) = grid;
                if(numel(this.tiles) == 1 && i == 1)
                    gridFile = [workingDir filesep 'DemGrid'];
                else
                    gridFile = [workingDir filesep sprintf('DemGrid_%04d',i)];
                end
                saveAsciiGrid(grid, [gridFile '.asc']);
                zip([gridFile '.zip'], {[gridFile '.asc']});
            end
        end
        
        function this = setSmoothFilter(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end
            noGrid = isempty(this.demGrid(this.tiles));
            if(any(noGrid(:)))
                error('For tile(s) %s grid needs to be set first.', mat2str(this.tiles(noGrid)));
            end
            grids = this.demGrid;
            for i=this.tiles
                grid = grids(i);
                grid.setSmoothFilter(this.cmd_args{:}, varargin{:});
            end
        end
        
        function this = setFeatureDetector(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end            
            noGrid = isempty(this.demGrid(this.tiles));
            if(any(noGrid(:)))
                error('For tile(s) %s grid needs to be set first.', mat2str(this.tiles(noGrid)));
            end
            grids = this.demGrid;
            for i=this.tiles
                grid = grids(i);
                grid.setFeatureDetector(this.cmd_args{:}, varargin{:});
            end
        end
        
        function this = setEdgeFilter(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end            
            noGrid = isempty(this.demGrid(this.tiles));
            if(any(noGrid(:)))
                error('For tile(s) %s grid needs to be set first.', mat2str(this.tiles(noGrid)));
            end
            grids = this.demGrid;
            for i=this.tiles
                grid = grids(i);
                grid.setEdgeFilter(this.cmd_args{:}, varargin{:});
            end
        end
       
        % detects breaklines in a grid
        function this = detectBreaklines(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end
            noGrid = isempty(this.demGrid(this.tiles));
            if(any(noGrid(:)))
                error('For tile(s) %s grid needs to be set first.', mat2str(this.tiles(noGrid)));
            end
            
            % parse inputs
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('distanceTolerance', 0); % simplify lines using this tolerance
            p.parse(this.cmd_args{:}, varargin{:});
            
            distanceTolerance = p.Results.distanceTolerance;
            
            workingDir = this.workingDir;
            grids = this.demGrid;
            for i=this.tiles
                grid = grids(i);
                breakpoints = grid.breakpoints;
                if(any(breakpoints(:)))
                    try
                        edgelist = bwboundaries(breakpoints);
                    catch e
                        disp(['Error linking edges for grid number ' num2str(i)]);
                        disp(e);
                        continue;
                    end
                   edgeCount = numel(edgelist);
                   this.breaklines{i} = org.kalypso.gaja3d.matlab.Curve.empty(edgeCount,0);
                   X = cell(edgeCount,0);
                   Y = cell(edgeCount,0);
                   Z = cell(edgeCount,0);
                   for j=1:edgeCount
                       [X{j}, Y{j}] = pix2map(this.demGrid(i).refmat, edgelist{j}(:, 1), edgelist{j}(:, 2));
                       if(distanceTolerance > 0)
                           % Douglas Peucker line simplification
                           [X{j}, Y{j}] = simplifyLine(X{j}, Y{j}, distanceTolerance);
                       end
                       Z{j} = zeros(size(X{j}));
                   end
                   this.breaklines{i} = org.kalypso.gaja3d.matlab.Curve(X, Y, Z);
                   this.breaklines{i} = this.breaklines{i}.clip(this.boundaries(i));
                   bs = this.breaklines{i}.asGeostruct();
                   if(~isempty(bs))
%                       featureMethod = 'simple';
%                       if(~isempty(grid.smoothFilter))
%                           smoothMethod = grid.smoothFilter.name;
%                           smooth = sprintf('%02.1f', grid.smoothFilter.hsize);
%                       else
%                           smoothMethod = 'none';
%                           smooth = '';
%                       end
%                       shpFileName =
%                       sprintf('Breaklines_%s_%02.1f-%02.1f_%s%s_%04d.shp'
%                       , featureMethod, grid.featureDetector.lowThresh, grid.featureDetector.highThresh, smoothMethod, smooth, i);
                      if(numel(this.tiles) == 1 && i == 1)
                          shpFileName = 'Breaklines';
                      else
                          shpFileName = sprintf('Breaklines_%04d', i);
                      end
                      fullshpbase = [workingDir filesep shpFileName];
                      shapewrite(bs, [fullshpbase '.shp'], 'DbfSpec', makedbfspec(bs));
                      zip([workingDir filesep shpFileName '.zip'], {[fullshpbase '.shp'], [fullshpbase '.dbf'], [fullshpbase '.shx']});
                   end
                end
            end
            this.breaklinesMerged = [this.breaklines{this.tiles}];
        end
        
        function this = createTin(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end
            
            % parse inputs
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('minAngle', []); % maximum angle in degrees
            p.addParamValue('maxArea', []); % maximum area
            p.addParamValue('minArea', []); % maximum area
            p.parse(this.cmd_args{:}, varargin{:});

            workingDir = this.workingDir;
            tempfile = [workingDir filesep 'ModelTin'];
            this.exportForTriangle(tempfile);
            if(isdeployed())
                tricommand = [pwd filesep 'exec' filesep 'triangle'];
            else
                tricommand = [pwd filesep 'exec' filesep 'triangle.exe'];
            end
            
            % Delaunay parameters
            if(~isempty(p.Results.minAngle))
                angleString = sprintf('q%d', p.Results.minAngle);
            else
                angleString = '';
            end
            if(~isempty(p.Results.maxArea))
                maxAreaString = sprintf('a%d', p.Results.maxArea);
            else
                maxAreaString = '';
            end
            
            cmd = sprintf('! %s -p%s%s %s', tricommand, angleString, maxAreaString, tempfile);
            eval(cmd);
            outputPrefix = [tempfile '.1'];
            [ elements, Xtri, Ytri ] = loadTriangleOutput( outputPrefix );
            movefile([outputPrefix '.ele'], [tempfile '.ele']);
            movefile([outputPrefix '.node'], [tempfile '.node']);
            movefile([outputPrefix '.poly'], [tempfile '.poly']);
            zip([tempfile '.zip'], {[tempfile '.ele'], [tempfile '.node'], [tempfile '.poly']}, workingDir);
            Ztri = zeros(size(Xtri));
            points = [Xtri Ytri Ztri];
            this.modelTin = org.kalypso.gaja3d.matlab.TriangulatedSurface(points, elements);
            this.refineCount = 0;
        end
        
        function this = assignElevations(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('source', 'tin'); % tin or grid
            p.parse(this.cmd_args{:}, varargin{:});
            
            tin = this.modelTin;
            if(isempty(tin))
                error('No tin to assign elevations to.');
            end
            elements = tin.elements;
            Xtri = tin.points(:,1);
            Ytri = tin.points(:,2);
            % initialize to NaN elevations
            Ztri = zeros(size(Xtri)) * NaN;
            for i=this.tiles
                % use tin as elevation source
                if(~(strcmpi(p.Results.source, 'grid') || i > numel(this.demTin) || isempty(this.demTin(i).points)))
                    source = this.demTin(i);
                elseif(i <= numel(this.demGrid))
                    % fall back to using grid if that is the only available
                    % elevation source
                    source = this.demGrid(i);
                end
                if(isempty(source))
                    warning('No elevation source for tile %d', i);
                end
                Zi = source.interpolateZ(Xtri, Ytri, varargin{:});
                Zmatched = ~isnan(Zi);
                hasZ = ~isnan(Ztri);
                ZmatchedAndNotHasZ = Zmatched & ~hasZ;
                ZmatchedAndHasZ = Zmatched & hasZ;
                Ztri(ZmatchedAndNotHasZ) = Zi(ZmatchedAndNotHasZ);
                Ztri(ZmatchedAndHasZ) = (Zi(ZmatchedAndHasZ) + Ztri(ZmatchedAndHasZ)) / 2;
            end
            this.modelTin = org.kalypso.gaja3d.matlab.TriangulatedSurface([Xtri Ytri Ztri], elements);
            workingDir = this.workingDir;
            tempfile = [workingDir filesep 'ModelTin'];
            saveTrianglesAsEle(this.modelTin.elements, [tempfile '.ele']);
            exportNodes(this.modelTin.points, tempfile);
            zip([tempfile '.zip'], {[tempfile '.ele'], [tempfile '.node']});
        end
        
        function this = refineTin(this, varargin)
            tin = this.modelTin;
            if(isempty(tin))
                error('No tin to refine.');
            end
            workingDir = this.workingDir;
            if(this.refineCount == 0)
                tempfile = [workingDir filesep 'ModelTin'];
            else
                tempfile = [workingDir filesep sprintf('ModelTin.%d', this.refineCount)];
            end
            
            [elements, Xtri, Ytri] = tin.refine(this.cmd_args{:}, varargin{:}, 'tempfile', tempfile);
            
            % initialize to NaN elevations
            Ztri = zeros(size(Xtri)) * NaN;
            this.modelTin = org.kalypso.gaja3d.matlab.TriangulatedSurface([Xtri Ytri Ztri], elements);
            this.refineCount = this.refineCount + 1;
        end
       
        % disposes of Gaja3D workspace
        function this = delete(this)
            % clean up
            % this call will clear all the other properties too
            this.boundaries = org.kalypso.gaja3d.matlab.Polygon.EMPTY;
            this.cmd_args = cell(0);
            delete('temp.*');
        end       
    end % public methods
   
	methods (Access = private)
         function exportForTriangle(this, filename)
            segmentsMatrixFinal = zeros(0,2);
            nodeCount = 1;
            
            boundaries = this.boundaries(this.tiles);
            boundariesMerged = boundaries(1);
            
            for i=2:numel(boundaries)
                boundariesMerged = boundariesMerged.union(boundaries(i));
            end

            polyX = boundariesMerged.getX();
            polyY = boundariesMerged.getY();
            
            pointCount = numel(polyX);
            holeCount = sum(isnan(polyX));
            holeCurrent = 1;
            segmentsMatrixFinal = zeros(pointCount - holeCount*2 - 1, 2);
            holesMatrix = zeros(holeCount, 3);
            currentEdge = 1;
            interiorStart = -1;
            for i=1:pointCount
                if(isnan(polyX(i)))
                    %ignore
                elseif(i == pointCount || isnan(polyX(i+1)))
                    if(interiorStart ~= -1)
                        holeX = polyX(interiorStart:i);
                        holeY = polyY(interiorStart:i);
                        [pointX, pointY] = getPointInPolygon(holeX, holeY);
                        holesMatrix(holeCurrent,:) = [holeCurrent pointX pointY];
                        holeCurrent = holeCurrent + 1;
                    end
                    interiorStart = i + 2;
                else
                    segmentsMatrixFinal(currentEdge,:) = [i i+1];
                    currentEdge = currentEdge + 1;
                end
            end
            nodesMatrixFinal = [polyX polyY];
            
            %nodesMatrixFinal = [polyX(1:end-1) polyY(1:end-1)];
            %first = nodeCount;
            %for n = 1:size(polyX, 1)-2 % last point connected to first
            %    node1 = nodeCount;
            %    node2 = nodeCount + 1;
            %    segmentsMatrixFinal = [segmentsMatrixFinal; node1 node2];
            %    nodeCount = nodeCount + 1;
            %end
            %segmentsMatrixFinal = [segmentsMatrixFinal; nodeCount first];
            nodeCount = numel(polyX);
            
            breaklines = this.breaklinesMerged;

            % clip breaklines that are too close to outer boundary (20 m)
            % EXPERIMENTAL
            %[outerClipX, outerClipY] = bufferPolygon(boundariesMerged.getX, boundariesMerged.getY, -20);
            %outerClip = org.kalypso.gaja3d.matlab.Polygon(outerClipX, outerClipY);
            %breaklines = breaklines.clip(outerClip);
            
            for i=1:numel(breaklines)
                lineX = breaklines(i).getX();
                lineY = breaklines(i).getY();
                nodesMatrixFinal = [nodesMatrixFinal; lineX lineY];
                for n = 1:size(lineX, 1)-1 % last point is only endpoint
                    node1 = nodeCount;
                    node2 = nodeCount + 1;
                    segmentsMatrixFinal = [segmentsMatrixFinal; node1 node2];
                    nodeCount = nodeCount + 1;
                end
                nodeCount = nodeCount + 1;
            end

            [uniqueNodes, m, n] = unique(nodesMatrixFinal(segmentsMatrixFinal(:),:), 'rows');
            index = (1:size(uniqueNodes,1))';
            nodesMatrixFinal = [index uniqueNodes];    

            uniqueSegments = reshape(n, size(segmentsMatrixFinal));
            index = (1:size(uniqueSegments,1))';
            segmentsMatrixFinal = [index uniqueSegments];
            
            Nodes=size(nodesMatrixFinal,1);
            Segments=size(segmentsMatrixFinal,1);
            output12_head=[num2str(Nodes),' 2 0 0']; %2D triangulation.No attributes and boundary markers.
            output22_head=[num2str(Segments),' 1']; % boundary markers are considered
            

            
            % INITIALIZE poly-file
            % open for writing
            fid = fopen([ filename '.poly' ], 'w');

            % write nodes
            fprintf(fid,'%s\n',output12_head); % number of nodes
            fprintf(fid,'%d %20.7f%20.7f\n',nodesMatrixFinal');

            % write segments
            fprintf(fid,'%s\n',output22_head); % number of segments
            fprintf(fid,'%d %d %d\n',segmentsMatrixFinal'); 

            % write holes
            output3 = num2str(holeCount);
            fprintf(fid,'%s\n',output3); % number of holes
            fprintf(fid,'%d %20.7f%20.7f\n',holesMatrix');

            %close the handle of the file
            fclose(fid);
        end
    end % private methods
end 
