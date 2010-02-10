classdef Contour < kalypso.Curve
   properties (GetAccess = public, SetAccess = public)
       CONTOUR 
   end
   
   methods
       % Shape constructor
       % function this = Contour(X, Y, Z)
       function this = Contour(varargin)
           import kalypso.*;
           
           if(nargin == 0)
               lineString = Shape.geomFactory.createLineString([]);
               this.jtsGeometry = lineString;
           elseif(nargin == 1)
               switch class(varargin{1})
                   case 'kalypso.Curve'
                       curves = varargin{1};
                       numcurves = numel(curves);
                       for i=1:numcurves
                           lineString = curves.jtsGeometry;
                           this(i).jtsGeometry = lineString;
                       end
                   case 'com.vividsolutions.jts.geom.LineString'
                       lineString = varargin{1};
                       this.jtsGeometry = lineString;
                   case 'char'
                       filename = varargin{1};
                       this = Shape.fromFile(filename, @kalypso.Contour);
                   otherwise
                       error('Call one-argument constructor only with LineString geometries.');
               end
           elseif(nargin == 3)
               lineString = varargin{1};
               this.jtsGeometry = lineString;
               propNames = varargin{2};
               propValues = varargin{3};
               idx = strmatch('CONTOUR', propNames);
               this.CONTOUR = propValues{idx};
           end
       end
   
       function allCurves = clip(this, polygon)
               X = this.getX();
               Y = this.getY();
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
                       newCurves = kalypso.Curve(linesX, linesY);%, linesZ);
                       c = numel(newCurves);
                       for j=1:c
                           newCurves(j).CONTOUR = this(i).CONTOUR;
                       end
                       allCurves(end+1:end+c) = newCurves;
                   end
               end
         end
    end
end 
