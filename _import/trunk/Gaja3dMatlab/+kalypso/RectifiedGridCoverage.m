classdef RectifiedGridCoverage < handle
   properties (Constant)
       EMPTY = kalypso.RectifiedGridCoverage();
   end % constant properties
   
   properties
       minx = 0;
       miny = 0;
   end % public properties

   properties (SetObservable = true)
       Z = zeros(0, 0);
       dx = 1;
       dy = 1;
       smoothFilter = []; % no smooth
       edgeFilter;
       featureDetector;
   end % public properties (observable)
   
   properties (SetAccess = private)
       smoothed = zeros(0, 0);
       slope = zeros(0, 0, 2);
       breakpoints = zeros(0, 0);
   end % read-only properties
   
   properties (Dependent)
       refmat;
   end % dependent properties
   
   methods
        % setter for Z
        function set.Z(this, Z)
            if(~isnumeric(Z))
                error('Z is no elevation data.');
            end
            % convert to double precision
            this.Z = double(Z);
        end
        
        % listen to property changes
        % maybe update smoothed, slope and breakpoints
        function objectChanged(this, src, event)
            if(any(strcmp(src.Name, {'Z', 'dx', 'dy', 'smoothFilter'})))
                % rerun smooth filter
                if(isempty(this.smoothFilter))
                    % just set Z as smoothed image
                    this.smoothed = this.Z;
                else
                    % run smooth filter and save result
                    this.smoothed = this.smoothFilter.process(this);
                end
            end
            
            if(any(strcmp(src.Name, {'Z', 'dx', 'dy', 'smoothFilter', 'edgeFilter'})))
                % recalculate slope based on smoothed image
                image = this.smoothed;
                if(isempty(image))
                    this.slope = zeros(0, 0, 2);
                else
                    this.slope = zeros([size(image) 2]);
                    
                    edgesVert = imfilter(image, this.edgeFilter, 'replicate');
                    edgesHori = imfilter(image, this.edgeFilter', 'replicate');

                    %normalize slopes using grid resolution and convert
                    %from radians to degrees
                    normVert = 180 / pi / this.dy;
                    normHori = 180 / pi / this.dx;

                    this.slope(:,:,1) = atan(edgesVert) * normVert;
                    this.slope(:,:,2) = atan(edgesHori) * normHori;
                end
            end
            
            if(any(strcmp(src.Name, {'Z', 'dx', 'dy', 'smoothFilter', 'edgeFilter', 'featureDetector'})))
                if(isempty(this.featureDetector))
                    this.breakpoints = zeros(size(this.Z));
                else
                    % rerun feature detector
                    this.breakpoints = this.featureDetector.process(this);
                end
            end
        end
        
        % getter for refmat, calculated on-the-fly
        function refmat = get.refmat(this)
            refmat = constructRefMat(this.minx, this.miny, this.dx, this.dy);
        end
   end % property getters and setters

   methods (Access = public)
        % Constructor   ()
        %               (filename)
        %               (Z, refmat)
        %               (Z, dx, dy, minx, miny)
        function this = RectifiedGridCoverage(varargin)
            % add property listener
            % it will recalculate smoothed, slope and breakpoints
            this.edgeFilter = kalypso.EdgeFilter.DEFAULT;
            this.featureDetector = kalypso.FeatureDetector.DEFAULT;
            this.addlistener({'Z', 'dx', 'dy', 'smoothFilter', 'edgeFilter', 'featureDetector'},...
                'PostSet', @(src,evnt)this.objectChanged(src,evnt));
            if(nargin == 0)
                return;
            elseif(ischar(varargin{1}))
                this = kalypso.RectifiedGridCoverage.fromFile(varargin{:});
                return;
            else
                Z = varargin{1};
            end
            
            if(nargin == 2)
                refmat = varargin{2};
                
                [rows, cols] = size(Z);
                % coordinates of map corners
                ul = [1 1 1] * refmat; % upper left pixel
                lr = [rows cols 1] * refmat; % lower right pixel
                [this.minx, iminx] = min([ul(1) lr(1)]);
                [this.miny, iminy] = min([ul(2) lr(2)]);
                maxx = max([ul(1) lr(1)]);
                maxy = max([ul(2) lr(2)]);

                % convert to increasing X and Y coordinates
                if(iminx == 2)
                    Z = fliplr(Z);
                end
                if(iminy == 2)
                    Z = flipud(Z);
                end

                this.dx = (maxx - this.minx) / (cols - 1);
                this.dy = (maxy - this.miny) / (rows - 1);
            end
            
            this.Z = Z;
            
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
            p.addParamValue('method', 'gauss', @ischar);
            p.parse(varargin{:});
            smoothMethod = p.Results.method;
            
            switch smoothMethod
                case 'bilateral'
                    this.smoothFilter = kalypso.BilateralFilter(varargin{:});
                case 'gauss'
                    this.smoothFilter = kalypso.GaussianFilter(varargin{:});
                case 'none'
                    this.smoothFilter = [];
                otherwise
                    error('Smooth filter method %s not recognized.', smoothMethod)
            end
        end
        
        function setFeatureDetector(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('method', 'simple', @ischar);
            p.parse(varargin{:});
            featureMethod = p.Results.method;
            
            switch featureMethod
                case 'canny'
                    this.featureDetector = kalypso.CannyDetector(varargin{:});
                case 'simple'
                    this.featureDetector = kalypso.SimpleFeatureDetector(varargin{:});
                otherwise
                    error('Feature detection method %s not recognized.', featureMethod)
            end
        end
       
        % getter for slope, calculated on-the-fly
        function setEdgeFilter(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('method', 'ood', @ischar);
            p.parse(varargin{:});
            method = p.Results.method;
            
            switch method
                case {'ood', 'frei-chen'}
                    this.edgeFilter = kalypso.EdgeFilter.FREI_CHEN;
                case 'sobel'
                    this.edgeFilter = kalypso.EdgeFilter.SOBEL;
                case 'prewitt'
                    this.edgeFilter = kalypso.EdgeFilter.PREWITT;
                otherwise
                    error('Edge filter %s not recognized.', method)                    
            end
        end
       
        % interpolate grid z-values from tin
        function zi = interpolateZ(this, xi, yi, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('useSmoothed', false);
            p.parse(varargin{:});
            if(p.Results.useSmoothed)
                Z = this.smoothed;
            else
                Z = this.Z;
            end
            [rows, cols] = size(Z);
            maxx = this.minx + this.dx * (cols - 1);
            maxy = this.miny + this.dy * (rows - 1);
            rangex = this.minx:this.dx:maxx;
            rangey = this.miny:this.dy:maxy;
            [X, Y] = meshgrid(rangex, rangey);
            zi = interp2(X, Y, Z, xi, yi, 'cubic');
        end
        
        % Warning: this method changes the underlying grid data!
        function scale(this, dx, dy)
            Z = this.Z;
            [rows, cols] = size(Z);
            maxx = this.minx + this.dx * (cols - 1);
            maxy = this.miny + this.dy * (rows - 1);
            rangex = this.minx:this.dx:maxx;
            rangey = this.miny:this.dy:maxy;
            [X, Y] = meshgrid(rangex, rangey);
            rangexi = this.minx:dx:maxx;
            rangeyi = this.miny:dy:maxy;
            [xi, yi] = meshgrid(rangexi, rangeyi);
            
            % to avoid recomputation of slope etc.
            this.Z = zeros(0,0);
            this.dx = dx;
            this.dy = dy;
            this.Z = interp2(X, Y, Z, xi, yi, 'cubic');
        end
        
        % Warning: this method changes the underlying grid data!
        % deletes all data outside the given polygon
        function crop(this, polygon)
            Z = this.Z;
            [rows, cols] = size(Z);
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
            Z = interp2(X, Y, Z, xi, yi, 'cubic');
            
            %discard all data outside the polygon (make NaN)
            index = inpolygon(xi, yi, polyX, polyY);
            Z(~index) = NaN;
            
            this.Z = Z;
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

            Z = zeros(rows, cols) * NaN;
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
                        Z(x,y) = currentPoint(3);
                        idx = idx + 1;
                    end
                end
            end
            this.Z = Z;
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
            SHPEXT = '.shp';
            TXTEXT = '.txt';
            XYZEXT = '.xyz';
            ZIPEXT = '.zip';

            [pathstr, name, ext] = fileparts(file);

            switch(lower(ext))
                case {TIFEXT, TIFFEXT}
                    % read image file
                    Z = double(imread(file));
                    % load world file (referencing matrix)
                    worldfile = getworldfilename(file);
                    refmat = worldfileread(worldfile);
                    this = kalypso.RectifiedGridCoverage(Z, refmat);
                case {ASCEXT}
                    % read arcview ascii grid file
                    [Z, refmat] = readAsciiGrid(file);
                    this = kalypso.RectifiedGridCoverage(Z, refmat);
                case {SHPEXT, TXTEXT, XYZEXT}
                    % load points from file
                    points = loadPointData(file);
                    this = kalypso.RectifiedGridCoverage.fromPoints(points, varargin{:});
                case {ZIPEXT}
                    tmp_dir = tempname;
                    filenames = unzip(file, tmp_dir);
                    allGrids = cell(size(filenames));
                    for i=1:numel(filenames)
                       grid = kalypso.RectifiedGridCoverage(filenames{i});
                       allGrids{i} = grid;
                    end
                    this = [allGrids{:}];
                    try
                        rmdir(tmp_dir, 's');
                    catch e
                        warning(e.message);
                    end
                otherwise
                    % try your luck with gdal
                    [Z, attrib] = gdalread(file);
                    refmat = flipud(reshape(attrib.GeoTransform, 3, 2));
                    this = kalypso.RectifiedGridCoverage(Z, refmat);
            end
       end
   end
end 
