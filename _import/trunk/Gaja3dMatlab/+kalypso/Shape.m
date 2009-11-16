classdef Shape
    
    properties (GetAccess = protected, Constant = true)
        geomFactory = com.vividsolutions.jts.geom.GeometryFactory();
    end
    
    properties (GetAccess = protected, SetAccess = protected)
        % Java Topology Suite 1.8 geometry object
        jtsGeometry
    end
    
    properties (GetAccess = private, SetAccess = private, Transient = true)
        X, Y, Z
    end
    
    methods
        function this = set.jtsGeometry(this, jtsGeometry)
            this.jtsGeometry = jtsGeometry;
            switch class(this.jtsGeometry)
                case {'com.vividsolutions.jts.geom.Polygon'}
                    exteriorRingCoords = this.jtsGeometry.getExteriorRing.getCoordinates();
                    intRingCount = this.jtsGeometry.getNumInteriorRing();
                    c = cell(intRingCount + 1, 1);
                    c{1} = exteriorRingCoords;
                    count = numel(exteriorRingCoords);
                    for n=0:(intRingCount-1)
                        interiorRingCoords = this.jtsGeometry.getInteriorRingN(n).getCoordinates();
                        count = count + numel(interiorRingCoords) + 1;
                        c{2+n} = interiorRingCoords;
                    end
                    this.X = zeros(count, 1);
                    this.Y = zeros(count, 1);
                    this.Z = zeros(count, 1);
                    current = 1;
                    for i=1:numel(c)
                        coords = c{i};
                        numCoords = numel(coords);
                        if(i == 1)
                            %ccw
                            r=1:numCoords; 
                        else
                            %ccw
                            r=numCoords:-1:1;            
                            this.X(current) = NaN;
                            this.Y(current) = NaN;
                            this.Z(current) = NaN;
                            current = current + 1;
                        end
                        for j=r
                            coord = coords(j);
                            this.X(current) = coord.x();
                            this.Y(current) = coord.y();
                            this.Z(current) = coord.z();
                            current = current + 1;
                        end
                    end
                otherwise
                    coords = this.jtsGeometry.getCoordinates();
                    count = numel(coords);
                    this.X = zeros(count, 1);
                    this.Y = zeros(count, 1);
                    this.Z = zeros(count, 1);
                    for i=1:count
                        c = coords(i);
                        this.X(i) = c.x();
                        this.Y(i) = c.y();
                        this.Z(i) = c.z();
                    end            
            end 
        end
        
        function X = getX(this)            
            X = this.getC('X');
        end
        
        function Y = getY(this)
            Y = this.getC('Y');
        end
        
        function Z = getZ(this)
            Z = this.getC('Z');
        end
        
        function geom = asDeegreeGeometry(this)
            % convert to Deegree geometry
            count = numel(this);
            if(count == 0)
                geom = [];
            else
                geom = javaArray('org.deegree.model.spatialschema.Geometry', count);
                for n=1:count
                    jtsGeom = this(n).jtsGeometry;
                    if(~jtsGeom.isEmpty())
                        geom(n) = org.deegree.model.spatialschema.JTSAdapter.wrap(jtsGeom);
                    end
                end
            end
        end
        
        function envelope = getDeegreeEnvelope(this)
            import org.deegree.model.spatialschema.*;
            geom = this.asDeegreeGeometry();
            count = numel(geom);
            if(count == 0)
                envelope = [];
            end
            envelope = geom(1).getEnvelope();
            for n=2:count
                g = geom(n);
                gEnv = g.getEnvelope();
                envelope = envelope.merge(gEnv);
            end
        end
        
        function shp = asGeostruct(this)
           s = size(this);
           X = cell(s);
           Y = cell(s);
           BoundingBox = cell(s);
           G = cell(s);
           ID = cell(s);
           for n=1:numel(this)
               X{n} = this(n).getX()';
               Y{n} = this(n).getY()';
               BoundingBox = [min(X{n}) min(Y{n}); max(X{n}) max(Y{n})];
               G = this(n).getGeometry();
               ID{n} = n;
           end
           shp = struct('Geometry', G, 'BoundingBox', BoundingBox, 'X', X, 'Y', Y, 'ID', ID);
        end      
        
        function toFile(this, filename)
            gs = this.asGeostruct();
            shapewrite(gs, filename, 'DbfSpec', makedbfspec(gs));
        end
        
        function show(this)
            gs = this.asGeostruct();
            mapshow(gs);
        end
    end
    
    methods (Access = private)        
        function C = getC(this, fieldname)
            n = numel(this);
            if(n == 1)
                C = this.(fieldname);
            else
                C = cell(size(this));
                for i=1:n
                    % linear indexing
                    C{i} = this(i).getC(fieldname);
                end
            end
        end
    end    
    
    methods (Static = true)
        function coords = toCoords(X, Y, Z)
            n = numel(X);
            coords = javaArray('com.vividsolutions.jts.geom.Coordinate', n);
            for i=1:n
                coords(i) = com.vividsolutions.jts.geom.Coordinate(X(i), Y(i), Z(i));
            end
        end
        
        function this = fromFile(filename, constructorCallback)
           import org.deegree.io.shpapi.*;
           [pathstr, name, ext] = fileparts(filename);
           switch(lower(ext))
               case '.zip'
                   tmp_dir = tempname;
                   filenames = unzip(filename, tmp_dir);
                   allShapes = cell(size(filenames));
                   fileOrder = sortcellchar(filenames);
                   for i=fileOrder'
                       shp = kalypso.Shape.fromFile(filenames{i}, constructorCallback);
                       if(~all(size(shp) == [1,1]) || ~shp.jtsGeometry.isEmpty())
                           % add only non-empty shapes
                           allShapes{i} = shp;
                       end
                   end
                   this = [allShapes{:}];
                   try
                       rmdir(tmp_dir, 's');
                   catch e
                       warning(e.message);
                   end
               case '.shp'
                   shpfile = ShapeFile([pathstr filesep name]);
                   shpCount = shpfile.getRecordNum;
                   shapeCount = 0;
                   for i=1:shpCount;
                       deegreeGeom = shpfile.getGeometryByRecNo(i);
                       if(~isempty(deegreeGeom))
                           jtsGeom = org.deegree.model.spatialschema.JTSAdapter.export(deegreeGeom);
                           geomCount = jtsGeom.getNumGeometries();
                           for n=1:geomCount
                               shapeCount = shapeCount + 1;
                               geomN = jtsGeom.getGeometryN(n-1);
                               if(isempty(constructorCallback))
                                   geomClass = class(geomN);
                                   switch(geomClass)
                                       case {'com.vividsolutions.jts.geom.Polygon'}
                                           this(shapeCount) = kalypso.Polygon(geomN);
                                       case {'com.vividsolutions.jts.geom.LineString'}
                                           this(shapeCount) = kalypso.Curve(geomN);
                                       otherwise
                                           error('Unknown Shape geometry %s', geomClass);
                                   end
                               else
                                   this(shapeCount) = constructorCallback(geomN);
                               end
                           end
                       end
                   end
                   shpfile.close();
               otherwise
                   warning('File extension %s not recognized.', ext);
                   this = constructorCallback();
           end
        end
    end
end 
