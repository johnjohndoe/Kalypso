function zi=interptri(tri,x,y,z,xi,yi)
% INTERPTRI Interpolate Triangulation.
% Zi = INTERPTRI(TRI,X,Y,Z,Xi,Yi) linearly interpolates the scattered data
% in vectors X,Y,Z as described by the triangulation TRI as returned by
% DELAUNAY at the individual pairs of points in Xi and Yi. That is, Zi(k)
% is the linear interpolation at the point (Xi(k),Yi(k)). Xi and Yi are not
% MESHGRIDed as they are in GRIDDATA when Xi is a row vector and and Yi is
% a column vector. Use MESHGRID when desired. For example,
%                  [Xii,yi] = MESHGRID(Xi,Yi); % meshgrid yourself
%                         Zi = INTERPTRI(TRI,x,y,z,Xii,yi);
% is the same as          Zi = GRIDDATA(x,y,z,Xi(:).',Yi(:));
% 
% INTERPTRI is faster than GRIDDATA when the same data is interpolated more
% than once because the triangulation TRI is precomputed and passed into
% INTERPTRI, whereas it is computed within GRIDDATA each time it is called.
%
% See also GRIDDATA, MESHGRID, DELAUNAY, TRIPLOT, TRIMESH, TRISURF, TSEARCH

% D.C. Hanselman, University of Maine, Orono, ME 04469
% MasteringMatlab@yahoo.com
% Mastering MATLAB 7
% 2006-05-03

if nargin~=6
   error('Six Input Arguments Required.')
end
%x=x(:); % make input data into vectors
%y=y(:);
%z=z(:).';
xlen=numel(x);
if ~isequal(xlen, numel(y), numel(z))
   error('x, y, and z must have the same number of elements')
end

if size(tri, 2)~=3 || any(tri(:) < 0) || any(tri(:) > xlen)
   error('tri must be a valid triangulation of the data in x, y, z.')
end

xilen = numel(xi);
if (xilen ~= numel(yi))
   error('xi and yi must have the same number of elements')
end

% initialize zi
zisiz = size(xi);
zi = zeros(zisiz) * NaN;

% offset x,y
% minx = min(min(xi));
% miny = min(min(yi));
% xi = xi - minx;
% yi = yi - miny;
% x = x - minx;
% y = y - miny;

ti = tsearch(x(:), y(:), cast(tri,'double'), xi(:), yi(:)); % find triangle associated with each data point
%tinan = isnan(ti);  % True for xi, yi outside the convex hull
%ti(tinan)=tsearch(x(:), y(:), tri, xi(tinan), yi(tinan)); % try again
tinan = isnan(ti);
ti(tinan) = 1;      % nan points to triangle one for now
tri = tri(ti,:);    % keep only those triangles where xi and yi exist
x1=x(tri(:,1));   % x data at vertices
x2=x(tri(:,2));
x3=x(tri(:,3));
y1=y(tri(:,1));   % y data at vertices
y2=y(tri(:,2));
y3=y(tri(:,3));
A2 = (x2-x1).*(y3-y1) - (x3-x1).*(y2-y1); % shape functions
N = zeros(xilen, 3);
N(:,3) = ((x1-xi(:)).*(y2-yi(:)) - (x2-xi(:)).*(y1-yi(:)))./A2;
N(:,2) = ((x3-xi(:)).*(y1-yi(:)) - (x1-xi(:)).*(y3-yi(:)))./A2;
N(:,1) = ((x2-xi(:)).*(y3-yi(:)) - (x3-xi(:)).*(y2-yi(:)))./A2;
N(tinan,:) = 0;           % give zero weight to nan data
zi(:) = sum(z(tri).*N, 2);  % interpolate

if(any(isnan(zi(:))))
    warning('Some nodes have NaN elevations!');
end

%zi=reshape(zi,zisiz);   % reshape output to match xi and yi input