classdef TriangulatedSurface < handle
    properties (Constant)
        EMPTY = kalypso.TriangulatedSurface();
    end % public static
    
    properties (Constant, Hidden, GetAccess = private)
        EMPTY_POINT_CLOUD = zeros(0, 3);
        EMPTY_TIN = zeros(0, 3);
    end % private static
   
	properties (SetAccess = private)
        % irregular elevation model (point cloud)
        % nx3 double precision array with x,y,z coordinates
        points = kalypso.TriangulatedSurface.EMPTY_POINT_CLOUD
        % triangulated elevation model (TIN)
        % nx3 indices into points for triangle coordinates
        elements = kalypso.TriangulatedSurface.EMPTY_TIN
        % for refinement specify maximum area for each triangle
        % nx1 double precision array
        maxArea = -1;
    end % private
    
    methods
        % Constructor
        function this = TriangulatedSurface(varargin)
            if(nargin == 0)
                return;
            elseif(nargin == 1)
                if(ischar(varargin{1}))
                    [pathstr, name, ext] = fileparts(varargin{1});
                    switch(ext)
                        case '.shp'
                            shpFileId = fopen(varargin{1});
                            if (shpFileId == -1)
                                error('Could not open file %s.', filename);
                            end;
                            try
                                fseek(shpFileId,32,'bof'); %shape type code
                                headerTypeCode = fread(shpFileId,1,'uint32','ieee-le');
                                switch(headerTypeCode)
                                    case 11 % PointZ
                                        this.points = varargin{1};
                                        this.initializeTin();
                                    case 15 % PolygonZ
                                        [this.elements, this.points] = loadTriangleShape( varargin{1} );
                                    otherwise
                                        error('Shape geometry type neither PointZ nor PolygonZ.');
                                end
                            catch err
                                fclose(shpFileId);
                                rethrow(err);
                            end
                            fclose(shpFileId);
                        otherwise
                            % treat as triangle output
                            [l_elements, Xtri, Ytri, Ztri] = loadTriangleOutput(fullfile(pathstr, name));
                            this.points = [Xtri Ytri Ztri];
                            this.elements = l_elements;
                    end
                else
                    this.points = varargin{1};
                    this.initializeTin();
                end
            elseif(nargin == 2)
                this.points = varargin{1};
                this.elements = varargin{2};
            else
                error('Constructor must be called with 0, 1 or 2 arguments.');
            end
        end

        % set points
        function set.points(this, value)
            valueSize = size(value);
            if(isnumeric(value) && (numel(valueSize) == 2) && (valueSize(2) == 3))
                % value is a point cloud
                this.points = value;
            elseif(ischar(value) && exist(value, 'file'))
                % value is a file
                this.points = loadPointData(value);
            else
                error('Value must be a nx3 double point data or valid point file.');
            end
        end
        
        % set elements
        function set.elements(this, value)
            valueSize = size(value);
            pointCount = size(this.points,1);
            if(isnumeric(value) && (numel(valueSize) == 2) && (valueSize(2) == 3))
                % value is a tin
                if(max(value(:)) ~= pointCount)
                   error('Triangulation does not fit the data.');
                end
                tin = value;
            elseif(ischar(value) && exist(value, 'file'))
                tin = loadTriangleOutput(value);
            end
            this.elements = tin;
        end

        % interpolate values zi for points (xi, yi)
        function zi = interpolateZ(this, xi, yi, varargin)
            X = this.points(:,1);
            Y = this.points(:,2);
            Z = this.points(:,3);
            zi = interptri(this.elements, X, Y, Z, xi, yi);
        end
        
        % refine this tin based on given parameters
        function [eleMatrix, Xtri, Ytri] = refine(this, varargin)
            % parse inputs
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('minAngle', []); % area shape file name
            p.addParamValue('maxArea', []); % max area default value
            p.addParamValue('minArea', []); % min area default value            
            p.addParamValue('maxHeight', Inf); % if to refine steep elements            
            p.addParamValue('refineFile', []); % area shape file name
            p.addParamValue('maxAreaField', 'MAX_AREA'); % max area field name
            p.addParamValue('maxHeightField', 'MAX_HEIGHT'); % if to refine steep elements
            p.addParamValue('keepBreaklines', true); % if to keep breaklines
            p.addParamValue('tempfile', 'temp.1'); % number of refinement
            p.parse(varargin{:});

            l_elements = this.elements;
            l_points = this.points;
            allX = zeros(size(l_elements));
            allY = zeros(size(l_elements));
            allZ = zeros(size(l_elements));
            allX(:) = l_points(l_elements,1);
            allY(:) = l_points(l_elements,2);
            allZ(:) = l_points(l_elements,3);
            areas = polyarea(allX, allY, 2);
            
            % iterate over elements
            tricount = size(l_elements, 1);
            maxAreaDefault = p.Results.maxArea;
            maxHeightDefault = p.Results.maxHeight;
            tempfile = p.Results.tempfile;
                        
%             minArea = p.Results.minArea;
%             if(~isempty(minArea))
%                 % remove triangles that are too small
%                 tooSmall = areas < minArea;
%                 %fractionBad = sum(tooSmall) / tricount;
%                 keepElements = elements(~tooSmall,:);
%                 [keepPoints, m, n] = unique(points(keepElements(:),:),'rows');
%                 if(size(keepPoints,1) < size(points,1))
%                     %keepElements = reshape(n, size(keepElements));
%                     %tin = kalypso.TriangulatedSurface(keepPoints, keepElements);
%                     %exportPoly([tempfile '.poly'], this.boundaries.asGeostruct(), this.breaklinesMerged.asGeostruct());
%                     nodeFilename = [tempfile '.node'];
%                     eleFilename = [tempfile '.ele'];
%                     delete(nodeFilename);
%                     delete(eleFilename);
%                     exportNodes(keepPoints, tempfile);
%                     saveTrianglesAsEle(zeros(0,3), eleFilename);
%                     cmd = sprintf('! %s -p%s%s %s', tricommand, 'q', maxAreaString, tempfile);
%                     eval(cmd);
%                     outputPrefix = [tempfile '.1'];
%                     [ elements, Xtri, Ytri ] = loadTriangleOutput( outputPrefix );
%                     movefile([outputPrefix '.ele'], [tempfile '.ele']);
%                     movefile([outputPrefix '.node'], [tempfile '.node']);
%                     movefile([outputPrefix '.poly'], [tempfile '.poly']);
%                     zip([tempfile '.zip'], {[tempfile '.ele'], [tempfile '.node'], [tempfile '.poly']}, workingDir);
%                     Ztri = zeros(size(Xtri));
%                     points = [Xtri Ytri Ztri];
%                 end
%             end 
            
            areaSwitch = '';
            maxHeight = ones(tricount, 1) * maxHeightDefault;
            if(~isempty(p.Results.refineFile))
                refinePolygons = kalypso.Polygon(p.Results.refineFile);
                [path, name] = fileparts(p.Results.refineFile);
                [dbfData, dbfFields] = loadDbf(fullfile(path, [name '.dbf']));
                maxAreaField = p.Results.maxAreaField;
                maxHeightField = p.Results.maxHeightField;
                
                % assign default value
                this.maxArea = ones(tricount, 1) * Inf;

                maxElementHeights = ones(numel(refinePolygons), 1) * maxHeightDefault;
                
                for i=1:numel(dbfFields)
                    field = dbfFields{i};
                    if(strcmpi(field, maxAreaField))
                        maxAreas = str2num(strvcat(dbfData{:,i}));
                    elseif(strcmpi(field, maxHeightField))
                        maxElementHeights(:) = str2num(strvcat(dbfData{:,i}));
                    end
                end
                for i=1:tricount
                    pidx = l_elements(i,[1 2 3 1]);
                    px = l_points(pidx,1);
                    py = l_points(pidx,2);
                    t = kalypso.Polygon(px, py);
                    for j=1:numel(refinePolygons)
                        refPoly = refinePolygons(j);
                        try
                            intersectsPoly = t.intersects(refPoly);
                        catch e
                            disp(e);
                        end
                        if(intersectsPoly)
                            knownMaxArea = this.maxArea(i);
                            knownMaxHeight = maxHeight(i);
                            if(isfinite(knownMaxArea))
                                this.maxArea(i) = (knownMaxArea + maxAreas(j)) / 2;
                            else
                                this.maxArea(i) = maxAreas(j);
                            end
                            if(isfinite(knownMaxHeight))
                                maxHeight(i) = (knownMaxHeight + maxElementHeights(j)) / 2;
                            else
                                maxHeight(i) = maxElementHeights(j);
                            end
                        end
                    end
                end
                
                if(isfinite(maxAreaDefault))
                    % assign default value
                    this.maxArea(isinf(this.maxArea)) = maxAreaDefault;
                else
                    this.maxArea(isinf(this.maxArea)) = 0;
                end

                areaSwitch = 'a';
            end

            if(any(isfinite(maxHeight)))
                elementHeights = max(allZ,[],2) - min(allZ,[],2);
                lengthFractions = maxHeight ./ elementHeights;
                areaFactor = lengthFractions .* lengthFractions;
                badTriangleFraction = sum(areaFactor < 1) / tricount;
                fprintf(1, '%2.1f %% of %d triangles are bad.\n', badTriangleFraction*100, tricount);
                l_maxArea = areas .* areaFactor;
                l_maxArea(lengthFractions >= 1) = -1;
                l_maxArea(isnan(l_maxArea)) = -1;
                this.maxArea = min(l_maxArea, this.maxArea);
                areaSwitch = 'a';
            end
            
            if(strcmp(areaSwitch, 'a'))
                tempAreaFile = [tempfile '.area'];
                exportArea(tempAreaFile, this.maxArea);
            end
            
            tricommand = [pwd filesep 'exec' filesep 'triangle.exe'];
            if(p.Results.keepBreaklines)
                polySwitch = 'p';
            else
                polySwitch = '';
            end
            if(~isempty(p.Results.minAngle))
                angleSwitch = sprintf('q%d', p.Results.minAngle);
            else
                angleSwitch = '';
            end
            
            if(~isempty(maxAreaDefault))
                areaDefaultSwitch = sprintf('a%d',maxAreaDefault);
            else
                areaDefaultSwitch = '';
            end
            
            cmd = sprintf('! %s -r%s%s%s%s %s', tricommand, polySwitch, angleSwitch, areaSwitch, areaDefaultSwitch, tempfile);
            eval(cmd);
            [path, name, ext] = fileparts(tempfile);
            nameext = [name ext];
            idx = strfind(nameext, '.');
            if(isempty(idx))
                resultfile = [tempfile '.1'];
            else
                count = str2double(nameext(idx+1:end));
                resultfile = [path filesep nameext(1:idx) num2str(count + 1)];
            end
            [ eleMatrix, Xtri, Ytri ] = loadTriangleOutput( resultfile );
        end
        
        function s = asGeostruct(this)
            eleMatrix = this.elements;
            Xtri = this.points(:,1);
            Ytri = this.points(:,2);
            
            totalTriCount = size(eleMatrix, 1);
            X = cell(totalTriCount, 1);
            Y = cell(totalTriCount, 1);
            B = cell(totalTriCount, 1);
            ID = cell(totalTriCount, 1);

            for t=1:totalTriCount
                % make last coordinates equal to first
                X{t} = Xtri(eleMatrix(t,[1 2 3 1]))'; % x coordinates of triangle t
                Y{t} = Ytri(eleMatrix(t,[1 2 3 1]))'; % y coordinates of triangle t
                % calculate bounding box
                B{t} = [min(X{t}), min(Y{t}); max(X{t}), max(Y{t})];
                % set consecutive id
                ID{t} = t;
            end

            % create geostruct with polygon geometry from cell arrays
            s = struct('Geometry', 'Polygon', 'BoundingBox', B, 'X', X, 'Y', Y, 'ID', ID);
        end
        
        function plot2d(this)
            trimesh(this.elements, this.points(:,1), this.points(:,2), 'Color', 'black');
        end
        
        function plot3d(this)
            trisurf(this.elements, this.points(:,1), this.points(:,2), this.points(:,3), 'FaceColor', 'interp', 'EdgeColor', 'black');
        end
        
    end % public methods
    
    methods (Access = private)
        % creates a triangulation of the point data
        function initializeTin(this)
            %sort points so the values are in increasing coordinates (first X then Y)
            %and remove duplicate entries
            [b, m] = unique(this.points(:,1:2), 'rows');
            this.points = sortrows(this.points(m,:),1:2);
            X = this.points(:,1);
            Y = this.points(:,2);
            % call delaunay triangulation (qhull)
            this.elements = delaunay(X, Y);
        end
    end % private methods
end 
