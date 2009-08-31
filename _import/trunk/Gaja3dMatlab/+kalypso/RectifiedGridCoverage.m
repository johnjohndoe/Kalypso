classdef RectifiedGridCoverage < handle
   properties (Constant)
       EMPTY = kalypso.RectifiedGridCoverage();
   end % constant properties
   
   properties
       minx = 0;
       miny = 0;
   end % public properties

   properties (SetObservable=true)
       smoothFilter;
       edgeFilter;
       featureDetector;
   end
   
   properties (SetAccess=private)
       Z = zeros(0, 0);
       dx = 1;
       dy = 1;
   end
   
   properties (Access=private)
       smoothed_cache
       slope_cache
       breakpoints_cache
   end
   
   properties (Dependent)
       refmat, smoothed, slope, breakpoints;
   end % dependent properties
   
   methods
        % listen to property changes
        % maybe update smoothed, slope and breakpoints
        function objectChanged(this, src, event) %#ok<INUSD>
            if(any(strcmp(src.Name, {'smoothFilter'})))
                this.smoothed_cache = [];
            end
            if(any(strcmp(src.Name, {'smoothFilter', 'edgeFilter'})))
                this.slope_cache = [];
            end
            if(any(strcmp(src.Name, {'smoothFilter', 'edgeFilter', 'featureDetector'})))
                this.breakpoints_cache = [];
            end
        end
        
        % getter for refmat, calculated on-the-fly
        function refmat = get.refmat(this)
            refmat = constructRefMat(this.minx, this.miny, this.dx, this.dy);
        end
        
        % getter for smoothed, calculated on-the-fly
        function smoothed = get.smoothed(this)
            if(isempty(this.smoothed_cache))
                % run smooth filter
                this.smoothed_cache = this.smoothFilter.process(this);
            end
            smoothed = this.smoothed_cache;
        end
        
        % getter for slope, calculated on-the-fly
        function slope = get.slope(this)
            if(isempty(this.slope_cache))
                % calculate slope based on smoothed image
                image = this.smoothed;
                if(isempty(image))
                    this.slope_cache = zeros(0, 0, 2);
                else
                    this.slope_cache = zeros([size(image) 2]);

                    edgesVert = imfilter(image, this.edgeFilter, 'replicate');
                    edgesHori = imfilter(image, this.edgeFilter', 'replicate');

                    %normalize slopes using grid resolution and convert
                    %from radians to degrees
                    normVert = 180 / pi / this.dy;
                    normHori = 180 / pi / this.dx;

                    this.slope_cache(:,:,1) = atan(edgesVert) * normVert;
                    this.slope_cache(:,:,2) = atan(edgesHori) * normHori;
                end
            end
            slope = this.slope_cache;
        end
        
        % getter for breakpoints, calculated on-the-fly
        function breakpoints = get.breakpoints(this)
            if(isempty(this.breakpoints_cache))
                % run feature detector
                this.breakpoints_cache = this.featureDetector.process(this);
            end
            breakpoints = this.breakpoints_cache;
        end
   end % property getters and setters

   methods (Access = public)
        % Constructor   ()
        %               (filename)
        %               (Z, refmat)
        %               (Z, dx, dy, minx, miny)
        function this = RectifiedGridCoverage(varargin)
            smoothFilterRegistry = kalypso.SmoothFilterRegistry();
            this.smoothFilter = smoothFilterRegistry('none');
            
            featureDetectorRegistry = kalypso.FeatureDetectorRegistry();
            this.featureDetector = featureDetectorRegistry('none');
            
            edgeFilterRegistry = kalypso.EdgeFilterRegistry();
            this.edgeFilter = edgeFilterRegistry('ood');
            
            % add property listener
            % it will recalculate smoothed, slope and breakpoints
            this.addlistener({'smoothFilter', 'edgeFilter', 'featureDetector'},...
                'PostSet', @(src,event)this.objectChanged(src,event));
       
            if(nargin == 0)
                return;
            elseif(ischar(varargin{1}))
                this = kalypso.RectifiedGridCoverage.fromFile(varargin{:});
                return;
            else
                l_Z = varargin{1};
            end
            
            if(nargin == 2)
                refmat = varargin{2};
                
                [rows, cols] = size(l_Z);
                % coordinates of map corners
                ul = [1 1 1] * refmat; % upper left pixel
                lr = [rows cols 1] * refmat; % lower right pixel
                [this.minx, iminx] = min([ul(1) lr(1)]);
                [this.miny, iminy] = min([ul(2) lr(2)]);
                maxx = max([ul(1) lr(1)]);
                maxy = max([ul(2) lr(2)]);

                % convert to increasing X and Y coordinates
                if(iminx == 2)
                    l_Z = fliplr(l_Z);
                end
                if(iminy == 2)
                    l_Z = flipud(l_Z);
                end

                this.dx = (maxx - this.minx) / (cols - 1);
                this.dy = (maxy - this.miny) / (rows - 1);
            end
            
            % convert to double
            this.Z = double(l_Z);
            
            if(nargin >= 3)
                this.dx = varargin{2};
                this.dy = varargin{3};
            end
            
            if(nargin >= 5)
                this.minx = varargin{4};
                this.miny = varargin{5};
            end
        end
        
        function setSmoothFilter(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('smoothFilter', 'none', @ischar);
            p.parse(varargin{:});
            smoothMethod = p.Results.smoothFilter;
            
            smoothFilterRegistry = kalypso.SmoothFilterRegistry();
            this.smoothFilter = smoothFilterRegistry(smoothMethod);
            this.smoothFilter.configure(varargin{:});
        end
        
        function setFeatureDetector(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('featureDetector', 'none', @ischar);
            p.parse(varargin{:});
            featureMethod = p.Results.featureDetector;
            
            featureDetectorRegistry = kalypso.FeatureDetectorRegistry();
            this.featureDetector = featureDetectorRegistry(featureMethod);
            this.featureDetector.configure(varargin{:});
        end
       
        % getter for slope, calculated on-the-fly
        function setEdgeFilter(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('edgeFilter', 'ood', @ischar);
            p.parse(varargin{:});
            edgeMethod = p.Results.edgeFilter;
            
            edgeFilterRegistry = kalypso.EdgeFilterRegistry();
            this.edgeFilter = edgeFilterRegistry(edgeMethod);
        end
       
        % interpolate grid z-values from tin
        function zi = interpolateZ(this, xi, yi, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('useSmoothed', false);
            p.addParamValue('interp', 'nearest', @isstr);
            p.parse(varargin{:});
            if(p.Results.useSmoothed)
                l_Z = this.smoothed;
            else
                l_Z = this.Z;
            end
            [rows, cols] = size(l_Z);
            maxx = this.minx + this.dx * (cols - 1);
            maxy = this.miny + this.dy * (rows - 1);
            rangex = this.minx:this.dx:maxx;
            rangey = this.miny:this.dy:maxy;
            [X, Y] = meshgrid(rangex, rangey);
            zi = interp2(X, Y, l_Z, xi, yi, p.Results.interp);
        end
        
        % Warning: this method changes the underlying grid data!
        function scale(this, dx, dy, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('interp', 'cubic', @isstr);
            p.parse(varargin{:});
            
            [rows, cols] = size(this.Z);
            maxx = this.minx + this.dx * (cols - 1);
            maxy = this.miny + this.dy * (rows - 1);
            rangex = this.minx:this.dx:maxx;
            rangey = this.miny:this.dy:maxy;
            [X, Y] = meshgrid(rangex, rangey);
            rangexi = this.minx:dx:maxx;
            rangeyi = this.miny:dy:maxy;
            [xi, yi] = meshgrid(rangexi, rangeyi);
            
            % to avoid recomputation of slope etc.
            this.dx = dx;
            this.dy = dy;
            this.Z = interp2(X, Y, this.Z, xi, yi, p.Results.interp);
        end
        
        % Warning: this method changes the underlying grid data!
        % deletes all data outside the given polygon
        function crop(this, polygon)
            [rows, cols] = size(this.Z);
            maxx = this.minx + this.dx * (cols - 1);
            maxy = this.miny + this.dy * (rows - 1);
            rangex = this.minx:this.dx:maxx;
            rangey = this.miny:this.dy:maxy;
            [X, Y] = meshgrid(rangex, rangey);

            polyX = polygon.X;
            polyY = polygon.Y;
            
            %compute new dimension based on polygon and old grid dimension
            maxx = min([max(polyX) maxx]);
            maxy = min([max(polyY) maxy]);
            this.minx = max([min(polyX) this.minx]);
            this.miny = max([min(polyY) this.miny]);
            rangexi = this.minx:this.dx:maxx;
            rangeyi = this.miny:this.dy:maxy;
            [xi, yi] = meshgrid(rangexi, rangeyi);
            
            %interpolate all new grid values from the old grid
            this.Z = interp2(X, Y, this.Z, xi, yi, 'cubic');
            
            %discard all data outside the polygon (make NaN)
            index = inpolygon(xi, yi, polyX, polyY);
            this.Z(~index) = NaN;
        end
        
        function delete(this)
            this.Z = zeros(0,0);
            % important: remove object references
            this.edgeFilter = [];
            this.smoothFilter = [];
            this.featureDetector = [];
        end
   end
   
    methods (Static = true, Access = public)       
        % load a grid from file (asc, tif, tiff)
        function this = fromPoints(points, varargin)
            this = kalypso.RectifiedGridCoverage();
            if(~isempty(points))
                points = sortrows(points, 1:2);

                % sort by x-coordinate
                X = sort(points(:,1));
                
                % sort by y-coordinate
                Y = sort(points(:,2));
                
                diffX = diff(X);
                rowCoords = X([true; diffX~=0]);
                rowSpacings = diff(rowCoords);
                diffY = diff(Y);
                colCoords = Y([true; diffY~=0]);
                colSpacings = diff(colCoords);
                
                % parse inputs
                p = inputParser;
                p.KeepUnmatched = true;
                p.addParamValue('gridx', colSpacings(1));
                p.addParamValue('gridy', rowSpacings(1));
                p.parse(varargin{:});
                
                this.dy = p.Results.gridy;
                this.dx = p.Results.gridx;

                % correct by half cell size
                this.minx = min(X) - (this.dx / 2);
                this.miny = min(Y) - (this.dy / 2);
                
                maxx = max(X) - (this.dx / 2);
                maxy = max(Y) - (this.dy / 2);

                diffx = maxx - this.minx;
                diffy = maxy - this.miny;
                
                rows = diffy ./ this.dy + 1;
                cols = diffx ./ this.dx + 1;
            else
                rows = 0;
                cols = 0;
                this.minx = 0;
                this.miny = 0;
                maxx = 0;
                maxy = 0;
                this.dx = 1;
                this.dy = 1;
            end

            this.Z = zeros(rows, cols) * NaN;
            rangex = this.minx:this.dx:maxx;
            rangey = this.miny:this.dy:maxy;
            [X, Y] = meshgrid(rangex, rangey);
            
            points(:,1) = points(:,1) - (this.dx / 2);
            points(:,2) = points(:,2) - (this.dy / 2);
            pcount = size(points, 1);
            idx = 1;
            for y=1:cols
                for x=1:rows
                    if(idx > pcount)
                        break;
                    end
                    currentPoint = points(idx,:);
                    if(currentPoint(1) == X(x,y) && currentPoint(2) == Y(x,y))
                        this.Z(x,y) = currentPoint(3);
                        idx = idx + 1;
                    end
                end
            end
        end
        
        function this = fromFile(file, varargin)
            if(~exist('file', 'var'))
                error('Must specify a filename.');
            elseif(~exist(file, 'file'))
                error('%s is not a valid filename or file does not exist.', file);
            end
            
            TIFEXT = '.tif';
            TIFFEXT = '.tiff';
            ASCEXT = '.asc';
            ASGEXT = '.asg';
            SHPEXT = '.shp';
            TXTEXT = '.txt';
            XYZEXT = '.xyz';
            ZIPEXT = '.zip';

            [pathstr, name, ext] = fileparts(file);

            switch(lower(ext))
                case {TIFEXT, TIFFEXT}
                    % read image file
                    l_Z = double(imread(file));
                    % load world file (referencing matrix)
                    worldfile = getworldfilename(file);
                    refmat = worldfileread(worldfile);
                    this = kalypso.RectifiedGridCoverage(l_Z, refmat);
                case {ASCEXT, ASGEXT}
                    % read arcview ascii grid file
                    [l_Z, refmat] = readAsciiGrid(file);
                    this = kalypso.RectifiedGridCoverage(l_Z, refmat);
                case {SHPEXT, TXTEXT, XYZEXT}
                    % load points from file
                    points = loadPointData(file);
                    this = kalypso.RectifiedGridCoverage.fromPoints(points, varargin{:});
                case {ZIPEXT}
                    tmp_dir = tempname;
                    filenames = unzip(file, tmp_dir);
                    allGrids = cell(size(filenames));
                    for i=1:numel(filenames)
                       filename = filenames{i};
                       grid = kalypso.RectifiedGridCoverage(filename);
                       allGrids{i} = grid;
                    end
                    this = [allGrids{:}];
                    try
                        rmdir(tmp_dir, 's');
                    catch e
                        warning(e.message);
                    end
                otherwise
                    fprintf(1, 'File extension %s not recognized.\n', ext);
            end
       end
   end
end 
