classdef Polygon < kalypso.Shape
   
   properties (Constant)
       EMPTY = kalypso.Polygon.empty();
   end
   
   methods (Static = true)
       function geom = getGeometry()
           geom = 'Polygon';
       end
   end
   
   methods   
       
       % Constructor
       function this = Polygon(varargin)
           import kalypso.*;
           
           if(nargin == 0)
               this.jtsGeometry = Shape.geomFactory.createPolygon([], []);
           elseif(nargin == 1 || nargin == 3)
               switch class(varargin{1})
                   case 'com.vividsolutions.jts.geom.Polygon'
                       polygon = varargin{1};
                       this.jtsGeometry = polygon;
                   case 'char'
                       filename = varargin{1};
                       this = Shape.fromFile(filename, @kalypso.Polygon);
                   otherwise
                       error('Call one-argument constructor only with LinearRing geometries.');
               end
           elseif(nargin > 1)
               X = varargin{1};
               Y = varargin{2};
               s = size(X);
               if(nargin == 3)
                   Z = varargin{3};
               else
                   % zero elevations
                   Z = zeros(s);
               end
               
               % create from X,Y,Z
               coords = Shape.toCoords(X, Y, Z);
               if(isnan(X(end)))
                   coords = coords(1:(end-1));
               end
               if(X(1) ~= X(end))
                   coords(end+1) = coords(1);
               end
               exteriorRing = Shape.geomFactory.createLinearRing(coords);
               
               % no interior rings supported yet
               %interiorRings = javaArray('com.vividsolutions.jts.geom.LinearRing', 1);
               %interiorRings(1) = Shape.geomFactory.createLinearRing([]);
               
               polygon = Shape.geomFactory.createPolygon(exteriorRing, []);
               this.jtsGeometry = polygon;
           end
       end

       function C = getCentroid(obj)
           X = mean(obj.exteriorRing.X);
           Y = mean(obj.exteriorRing.Y);
           Z = mean(obj.exteriorRing.Z);
           C = [X Y Z];
       end
       
       function this = buffer(this, distance)
%            import kalypso.*;
%            geom = this.jtsGeometry;
%            extRing = geom.getExteriorRing;
%            outer = Shape.geomFactory.createPolygon(extRing, []).buffer(distance);
%            n = geom.getNumInteriorRing();
%            holesCell = cell(n,1);
%            holeCount = 0;
%            for i=0:(n-1)
%                innerRing = geom.getInteriorRingN(i);
%                inner = Shape.geomFactory.createPolygon(innerRing, []).buffer(-distance);
%                if(~inner.isEmpty())
%                    holesCell{i+1} = inner;
%                    holeCount = holeCount + 1;
%                end
%            end
%            if(holeCount > 0)
%                holes = javaArray('com.vividsolutions.jts.geom.LinearRing', holeCount);
%                current = 1;
%                for j=1:numel(holesCell)
%                    if(~isempty(holesCell{j}))
%                        holes(current) = holesCell{j}.getExteriorRing();
%                        current = current + 1;
%                    end
%                end
%            else
%                holes = [];
%            end
%            this.jtsGeometry = Shape.geomFactory.createPolygon(outer.getExteriorRing(), holes);
             this.jtsGeometry = this.jtsGeometry.buffer(distance);
       end
       
       function this = clip(this, other)
           this.jtsGeometry = other.jtsGeometry.intersection(this.jtsGeometry);
       end
       
       function result = intersects(this, other)
           result = this.jtsGeometry.intersects(other.jtsGeometry);
       end
       
       function result = union(this, other)
           allGeom = [other.jtsGeometry this.jtsGeometry];
           newGeom = allGeom(1);
           for i=2:numel(allGeom)
               newGeom = newGeom.union(allGeom(i));
           end
           switch(class(newGeom))
               case 'com.vividsolutions.jts.geom.Polygon'
                   result = kalypso.Polygon(newGeom);
               case 'com.vividsolutions.jts.geom.MultiPolygon'
                   polyCount = newGeom.getNumGeometries();
                   result = kalypso.Polygon.empty(polyCount,0);
                   for n=1:polyCount
                       result(n) = newGeom.getGeometryN(n-1);
                   end
               otherwise
                   error('No polygon result for union');
           end
       end
   end
end
