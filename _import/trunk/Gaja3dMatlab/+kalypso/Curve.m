classdef Curve < kalypso.Shape

   properties (Constant)
       % creates an empty MultiLineString
       EMPTY = kalypso.Curve.empty();
   end
   
   properties (GetAccess = public, SetAccess = public)
       contour 
   end
   
   methods (Static = true)
       function geom = getGeometry()
           geom = 'Line';
       end
   end
   
   methods
       % Shape constructor
       % function this = Curve(X, Y, Z)
       function this = Curve(varargin)
           import kalypso.*;
           
           if(nargin == 0)
               lineString{1} = Shape.geomFactory.createLineString([]);
           elseif(nargin == 1)
               switch class(varargin{1})
                   case 'com.vividsolutions.jts.geom.LineString'
                       lineString{1} = varargin{1};
                   case 'char'
                       filename = varargin{1};
                       this = Shape.fromFile(filename, @kalypso.Curve);
                       return;
                   otherwise
                       error('Call one-argument constructor only with LineString geometries.');
               end
           elseif(nargin > 1)
               X = varargin{1};
               Y = varargin{2};
               s = size(X);
               if(nargin == 3)
                   Z = varargin{3};
               elseif(iscell(X))
                   Z = cellfun(@(x)zeros(size(x)), X, 'UniformOutput', 0);
               else
                   % zero elevations
                   Z = zeros(s);
               end
           end
           
           if(~exist('lineString', 'var'))
               if(iscell(X))
                   X_cell = X;
                   Y_cell = Y;
                   Z_cell = Z;
               else
                   X_cell{1} = X;
                   Y_cell{1} = Y;
                   Z_cell{1} = Z;
               end
                              
               lineString = cell(size(X_cell));
               n = numel(X_cell);
               for i=1:n
                   % create from X,Y,Z
                   coords = Shape.toCoords(X_cell{i}, Y_cell{i}, Z_cell{i});
                   lineString{i} = Shape.geomFactory.createLineString(coords);
               end
           end
           
           for i=1:numel(lineString)
               this(i).jtsGeometry = lineString{i};
           end
       end
       
       function result = discardSmall(this, length)
           count = numel(this);
           j = 1;
           for i=1:count
               if(this(i).jtsGeometry.getLength() >= length)
                   result(j) = this(i);
                   j = j + 1;
               end
           end
       end
       
       function result = merge(this)
           count = numel(this);
           import com.vividsolutions.jts.operation.linemerge.*;
           lineMerger = LineMerger();
           for i=1:count
               lineMerger.add(this(i).jtsGeometry);
           end
           mergedLines = lineMerger.getMergedLineStrings();
           newcount = mergedLines.size();
           for i=0:(newcount-1)
               result(i+1) = kalypso.Curve(mergedLines.get(i));
           end
       end
       
       function allCurves = clip(this, polygon)
           X = this.getX();
           Y = this.getY();
           Z = this.getZ();
           count = numel(this);
           allCurves = kalypso.Curve.empty();
           for i=1:count
               if(count == 1)
                   [linesXNaN, linesYNaN] = clipLine(X, Y, polygon.getX(), polygon.getY());
               else
                   [linesXNaN, linesYNaN] = clipLine(X{i}, Y{i}, polygon.getX(), polygon.getY());
               end
               if(numel(linesXNaN) > 0)
                   [linesX, linesY] = polysplit(linesXNaN, linesYNaN); 
                   %[linesX, linesZ] = polysplit(linesXNaN, linesZNaN);
                   newCurves = kalypso.Curve(linesX, linesY);%, linesZ);
                   c = numel(newCurves);
                   for j=1:c
                       newCurves(j).contour = this(i).contour;
                   end
                   allCurves(end+1:end+c) = newCurves;
               end
           end
       end
       
       function this = simplify(this, tolerance)
           count = numel(this);
           %multigeom = this(1).jtsGeometry;
           for i=1:count
               %multigeom = multigeom.union(this(i).jtsGeometry);
               this(i).jtsGeometry = com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier.simplify(this(i).jtsGeometry, tolerance);
           end
           %result = com.vividsolutions.jts.simplify.TopologyPreservingSimplifier.simplify(multigeom, tolerance);
           %geomCount = result.getNumGeometries();
           %if(geomCount ~= count)
           %    this(geomCount+1:end) = [];
           %    %error('Number of curves changed');
           %end
           %for i=1:geomCount
           %    this(i).jtsGeometry = result.getGeometryN(i - 1);
           %end
       end
       
       function [this, polygon] = snap(this, polygon, tolerance)
           count = numel(this);
           %multigeom = this(1).jtsGeometry;
           %for i=2:count
           %    multigeom = multigeom.union(this(i).jtsGeometry);
           %end
           %result = com.vividsolutions.jts.operation.overlay.snap.GeometrySnapper.snap(multigeom, polygon.jtsGeometry, tolerance);
           
           polyGeom = polygon.jtsGeometry;
           for i=1:count
               result = com.vividsolutions.jts.operation.overlay.snap.GeometrySnapper.snap(this(i).jtsGeometry, polyGeom, tolerance);
               thisGeom = result(1);
               geomCount = thisGeom.getNumGeometries();
               if(geomCount ~= 1)
                   error('Number of curves has changed');
               end
               this(i).jtsGeometry = thisGeom.getGeometryN(0);
               
               polyGeom = result(2);
           end
           polygon.jtsGeometry = polyGeom;
           
           %thisGeom = result(1);
           %geomCount = thisGeom.getNumGeometries();
           %for i=1:geomCount
           %    this(i).jtsGeometry = thisGeom.getGeometryN(i - 1);
           %end
           
           %polyGeom = result(2);
           %polygon.jtsGeometry = polyGeom;
       end
       
       function shp = asDeegreeShape(this)
            import org.deegree.io.shpapi.shape_new.*;            
            geom = this.asDeegreeGeometry();
            count = numel(geom);
            if(count == 0)
                shp = [];
            else
                for n=1:count
                   g = geom(n);
                   shp(n) = ShapePolyline(g);
                end
            end
       end        
   end
end 
