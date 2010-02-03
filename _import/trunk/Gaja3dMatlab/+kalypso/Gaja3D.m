classdef Gaja3D < handle
%GAJA3D 3D Terrain Discretisation Service

    properties (SetObservable = true)
        % buffers [meter] around tile boundaries
        bufferTin = 300; % for tin creation from points
        bufferGrid = 150; % for grid creation from tin
        
        % array of indices determining which boundaries to process
        tiles = [];
    end
    
    properties (SetAccess = private, SetObservable = true)
        % the outer boundaries of the model area tiles
        boundaries = kalypso.Polygon.empty();
        
        % original scattered tin points and linear triangulation interpolation
        demTin = kalypso.TriangulatedSurface2.empty();
        
        % raster elevation model (GRID)
        % single instance of RectifiedGridCoverage
        demGrid = kalypso.RectifiedGridCoverage.empty();
        
        % breaklines for tin generation
        % Curve array
        breaklines = kalypso.Curve.empty();
        
        % All breaklines for tin generation. These may be set manually and
        % will override the other breaklines if present.
        breaklinesMerged = kalypso.Curve.empty();
        
        % generated tin points and triangle indices
        modelTin = kalypso.TriangulatedSurface.empty();
        refineCount = 0;
    end % public properties
        
    methods
         % listen to property changes
        function objectChanged(this, src, event) %#ok<INUSD>
            if(any(strcmp(src.Name, {'boundaries'})))
                this.tiles = 1:numel(this.boundaries);
                this.demTin = kalypso.TriangulatedSurface2.empty(0, numel(this.tiles));
                this.demGrid = kalypso.RectifiedGridCoverage.empty(0, numel(this.tiles));
                this.breaklines = cell(0);
                for i=1:max(this.tiles)
                    this.breaklines{i} = kalypso.Curve.empty();
                end                                
            elseif(any(strcmp(src.Name, {'demTin'})))
                this.demGrid = kalypso.RectifiedGridCoverage.empty(0, numel(this.tiles));
                this.breaklines = cell(0);
                for i=1:max(this.tiles)
                    this.breaklines{i} = kalypso.Curve.empty();
                end                
            elseif(any(strcmp(src.Name, {'demGrid'})))
                this.breaklines = cell(0);
                for i=1:max(this.tiles)
                    this.breaklines{i} = kalypso.Curve.empty();
                end
            elseif(any(strcmp(src.Name, {'breaklines'})))
                this.breaklinesMerged = kalypso.Curve.empty();
                this.modelTin = kalypso.TriangulatedSurface();
            end
        end
    end
   
    methods (Access = public)
        % Private constructor
        function this = Gaja3D(varargin)
            this.addlistener({'boundaries','demTin','demGrid','breaklines'},...
                'PostSet', @(src,event)this.objectChanged(src,event));
            
            % parse inputs
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('bufferTin', this.bufferTin);
            p.addParamValue('bufferGrid', this.bufferGrid);
            p.parse(varargin{:});
            
            this.bufferTin = p.Results.bufferTin;
            this.bufferGrid = p.Results.bufferGrid;
        end
        
        function setElevationPoints(this, varargin)
            if(isempty(this.boundaries))
                error('Must set boundaries first!');
            end
            if(nargin < 2)
                error('Call with point data argument only!');
            end
            if(nargin >= 2)
                points = varargin{1};
                
                if(~iscell(points))
                    warning('Using the same points for all boundaries');
                    pointcell = cell(size(this.boundaries));
                    for i=this.tiles
                        pointcell{i} = points;
                    end
                    this.setElevationPoints(pointcell);
                    return;
                end
                
                if(~all(size(points)==size(this.boundaries)))
                    error('Number of point inputs does not match number of boundaries');
                end
                
                % triangulate patches with buffer
                for i=this.tiles
                    if(ischar(points{i}))
                        tilePoints = loadPointData(points{i},0);
                    else
                        tilePoints = points{i};
                    end
                    
                    % discard duplicate points and NaN elevations and sort
                    nanEle = isnan(tilePoints(:,3));
                    tilePoints = tilePoints(~nanEle,:);
                    [b, m] = unique(tilePoints(:,1:2), 'rows');
                    tilePoints = sortrows(tilePoints(m,:),1:2);

                    % buffer boundary for tin
                    boundary = this.boundaries(i);
                    bufDist = this.bufferTin;
                    boundaryBuffer = boundary.buffer(bufDist);
                    
                    % reduce to boundary
                    index = inBoundary(tilePoints(:,1), tilePoints(:,2), boundaryBuffer);
                    redPoints = tilePoints(index,:);
                    
                    % create tin
                    if(isempty(redPoints))
                        error('No elevation points in boundary %i.', i);
                    else
                        this.demTin(i) = kalypso.TriangulatedSurface2(redPoints);
                    end
                end
            end 
        end
        
        function setElevationGrid(this, grid)
            if(~iscell(grid))
                grid = {grid};
            end
            for i=this.tiles
                if(isa(grid{i},'kalypso.RectifiedGridCoverage'))
                    this.demGrid(i) = grid{i};
                else
                    this.demGrid(i) = kalypso.RectifiedGridCoverage(grid{i});
                end
            end
        end
        
        function setBreaklines(this, breaklines)
            if(isempty(breaklines))
                this.breaklinesMerged = kalypso.Curve.empty();
            elseif(~iscell(breaklines))
                this.breaklinesMerged = kalypso.Curve(breaklines);
            elseif(~all(size(breaklines)==size(this.breaklines)))
                error('Number of breaklines sets does not match number of grids');
            else
                for i=this.tiles
                    for j=1:numel(breaklines)
                        if(isa(breaklines{i}, 'kalypso.Curve'))
                            this.breaklines(i) = breaklines(i);
                        elseif(isempty(breaklines{i}))
                            this.breaklines{i} = kalypso.Curve.empty();
                        else
                            this.breaklines{i} = kalypso.Curve(breaklines{i});
                        end
                    end
                end
                this.breaklinesMerged = [this.breaklines{this.tiles}];
            end
        end
        
        function setBoundaries(this, boundaries)
            if(isa(boundaries, 'kalypso.Polygon'))
                this.boundaries = boundaries;
            else
                this.boundaries = kalypso.Polygon(boundaries);
            end
        end
        
        % creates a grid from a tin with given resolution
        function createGrid(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end
            
            % parse inputs
            p = inputParser;
            p.KeepUnmatched = true;
            testf = @(x)(isnumeric(x) && isscalar(x) && x>0);
            p.addParamValue('gridx', 1, testf);
            p.addParamValue('gridy', 1, testf);
            p.parse(varargin{:});
            dx = p.Results.gridx;
            dy = p.Results.gridy;
            
            % create grid for all tins
            for i=this.tiles
                tin = this.demTin(i);
                if(isempty(tin))
                    error('For tile %d elevation points need to be set first.', i);
                end
                
                %buffer boundary for grid creation
                bufdist = this.bufferGrid;
                boundary = this.boundaries(i);
                boundaryBuffer = boundary.buffer(bufdist);

                % create grid mesh
                polyX = boundaryBuffer.getX();
                polyY = boundaryBuffer.getY();
                [gridx, gridy, refmat] = createGrid(polyX, polyY, dx, dy);
                
                % interpolate gridz
                gridz = tin(gridx, gridy);
                
                % reduce to boundary
                index = inBoundary(gridx(:), gridy(:), boundaryBuffer);
                gridz(~index) = NaN;
                
                % create coverage
                grid = kalypso.RectifiedGridCoverage(gridz, refmat);
                
                this.demGrid(i) = grid;
            end
        end
        
        function setSmoothFilter(this, varargin)
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
                grid.setSmoothFilter(varargin{:});
            end
        end
        
        function setFeatureDetector(this, varargin)
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
                grid.setFeatureDetector(varargin{:});
            end
        end
        
        function setEdgeFilter(this, varargin)
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
                grid.setEdgeFilter(varargin{:});
            end
        end
       
        % detects breaklines in a grid
        function detectBreaklines(this, varargin)
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
            p.parse(varargin{:});
            
            distanceTolerance = p.Results.distanceTolerance;
            
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
                   this.breaklines{i} = kalypso.Curve.empty(edgeCount,0);
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
                   this.breaklines{i} = kalypso.Curve(X, Y, Z);
                   this.breaklines{i} = this.breaklines{i}.clip(this.boundaries(i));
                end
            end
            this.breaklinesMerged = [this.breaklines{this.tiles}];
        end
        
        function createTin(this, varargin)
            if(isempty(this.tiles))
                error('No tiles to process!');
            end
            
            % parse inputs
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('minAngle', []); % maximum angle in degrees
            p.addParamValue('maxArea', []); % maximum area
            p.addParamValue('fixBoundary', 0); % if insertion of points on boundary is prohibited
            p.parse(varargin{:});

            tempfile = 'ModelTin';
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
            if(p.Results.fixBoundary)
                restrictSteinerPointsString = 'Y';
            else
                restrictSteinerPointsString = '';
            end
            
            cmd = sprintf('! %s -p%s%s%s %s', tricommand, restrictSteinerPointsString, angleString, maxAreaString, tempfile);
            eval(cmd);
            outputPrefix = [tempfile '.1'];

            [ elements, Xtri, Ytri ] = loadTriangleOutput( outputPrefix );
            
            movefile([outputPrefix '.ele'], [tempfile '.ele']);
            movefile([outputPrefix '.node'], [tempfile '.node']);
            movefile([outputPrefix '.poly'], [tempfile '.poly']);
            
            % initialize to 0 elevation
            Ztri = ones(size(Xtri)) * NaN;
            points = [Xtri Ytri Ztri];
            
            % discard points that are not in any triangle
            keepPoints = sort(unique(elements(:)));
            discardPoints = setdiff(1:size(points,1), keepPoints);
            toMove = int32(zeros(size(elements)));
            for point=discardPoints
                toMove = toMove + int32(elements > point);
            end
            elements = elements - toMove;
            points = points(keepPoints,:);
            
            this.modelTin = kalypso.TriangulatedSurface(points, elements);
            this.refineCount = 0;
        end
        
        function assignElevations(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('source', 'grid'); % tin or grid
            p.addParamValue('useContour', 'false'); % use contour values
            p.parse(varargin{:});
            
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
                if(strcmpi(p.Results.source, 'grid'))
                    % fall back to using grid if that is the only available
                    % elevation source
                    l_demGrid = this.demGrid(i);
                    Zi = l_demGrid.interpolateZ(Xtri, Ytri, varargin{:});
                elseif(strcmpi(p.Results.source, 'tin'))
                	l_demTin = this.demTin(i);
                    Zi = l_demTin(Xtri, Ytri);
                end
                Zmatched = ~isnan(Zi);
                hasZ = ~isnan(Ztri);
                ZmatchedAndNotHasZ = Zmatched & ~hasZ;
                ZmatchedAndHasZ = Zmatched & hasZ;
                Ztri(ZmatchedAndNotHasZ) = Zi(ZmatchedAndNotHasZ);
                Ztri(ZmatchedAndHasZ) = (Zi(ZmatchedAndHasZ) + Ztri(ZmatchedAndHasZ)) / 2;
            end
            
            if(strcmpi(p.Results.useContour, 'true'))
                l_breaklines = this.breaklinesMerged;
                geomFactory = com.vividsolutions.jts.geom.GeometryFactory();
                quadtree = com.vividsolutions.jts.index.quadtree.Quadtree();
                for j=1:numel(l_breaklines)
                    l_line = l_breaklines(j);
                    coordCount = l_line.jtsGeometry.getNumPoints;
                    % insert all segments into quadtree
                    for k=0:coordCount-2;
                        coords = javaArray('com.vividsolutions.jts.geom.Coordinate',2);
                        coords(1) = l_line.jtsGeometry.getCoordinateN(k);
                        coords(2) = l_line.jtsGeometry.getCoordinateN(k+1);
                        item = javaArray('java.lang.Object',2);
                        item(1) = geomFactory.createLineString(coords);
                        item(2) = java.lang.Double(l_line.contour);
                        envelope = item(1).getEnvelopeInternal();
                        quadtree.insert(envelope,item);
                    end
                end

                % for each point of the tin
                for i=1:size(Xtri,1)
                    coord = com.vividsolutions.jts.geom.Coordinate(Xtri(i),Ytri(i));
                    envelope = com.vividsolutions.jts.geom.Envelope(coord, coord);
                    list = quadtree.query(envelope);
                    if(~list.isEmpty)
                        % loop over breaklines for this point
                        bcount = list.size();
                        point = geomFactory.createPoint(coord);
                        for j=0:bcount-1
                            item = list.get(j);
                            if(item(1).isWithinDistance(point,0.01))
                                Ztri(i) = item(2);
                                break;
                            end
                        end
                    end
                end
            end
            this.modelTin = kalypso.TriangulatedSurface([Xtri Ytri Ztri], elements);
            
            if(this.refineCount == 0)
                tempfile = 'ModelTin';
            else
                tempfile = sprintf('ModelTin.%d', this.refineCount);
            end
            
            exportNodes([Xtri Ytri Ztri], tempfile);
        end
        
        function refineTin(this, varargin)
            tin = this.modelTin;
            if(isempty(tin))
                error('No tin to refine.');
            end
            
            if(this.refineCount == 0)
                tempfile = 'ModelTin';
            else
                tempfile = sprintf('ModelTin.%d', this.refineCount);
            end
            
            [elements, Xtri, Ytri] = tin.refine(varargin{:}, 'tempfile', tempfile);
            
            % initialize to NaN elevations
            Ztri = zeros(size(Xtri)) * NaN;
            this.modelTin = kalypso.TriangulatedSurface([Xtri Ytri Ztri], elements);
            this.refineCount = this.refineCount + 1;
        end
       
        % disposes of Gaja3D workspace
        function delete(this)
            % this call will clear all properties
            this.boundaries = kalypso.Polygon.empty();
        end       
    end % public methods
   
	methods (Access = private)
         function exportForTriangle(this, filename)
            % merge one boundary from all tiles
            l_boundaries = this.boundaries(this.tiles);
            boundariesMerged = l_boundaries(1);
            for i=2:numel(l_boundaries)
                boundariesMerged = boundariesMerged.union(l_boundaries(i));
            end

            % get merged breaklines of all tiles
            l_breaklines = this.breaklinesMerged;
            
            % save boundary
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
            
            % save breaklines
            nodeCount = numel(polyX);
            nodeCount = nodeCount + 1;
            
            for i=1:numel(l_breaklines)
                lineX = l_breaklines(i).getX();
                lineY = l_breaklines(i).getY();
                nodesMatrixFinal = [nodesMatrixFinal; lineX lineY];
                for n = 1:size(lineX, 1)-1 % last point is only endpoint
                    node1 = nodeCount;
                    node2 = nodeCount + 1;
                    segmentsMatrixFinal = [segmentsMatrixFinal; node1 node2];
                    nodeCount = nodeCount + 1;
                end
                nodeCount = nodeCount + 1;
            end

            %roundedNodes = round(nodesMatrixFinal(segmentsMatrixFinal(:),:) * 1000);
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
            fprintf(fid,'%d %20.2f%20.2f\n',nodesMatrixFinal');

            % write segments
            fprintf(fid,'%s\n',output22_head); % number of segments
            fprintf(fid,'%d %d %d\n',segmentsMatrixFinal'); 

            % write holes
            output3 = num2str(holeCount);
            fprintf(fid,'%s\n',output3); % number of holes
            fprintf(fid,'%d %20.2f%20.2f\n',holesMatrix');

            %close the handle of the file
            fclose(fid);
        end
    end % private methods
end 
