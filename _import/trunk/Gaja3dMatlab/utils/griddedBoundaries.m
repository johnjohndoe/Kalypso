function boundaries = griddedBoundaries(varargin)
    if(nargin >=1)
        outerBoundary = org.kalypso.gaja3d.matlab.Polygon(varargin{1});
    end
    
    if(nargin>=2)
        cellwidth = varargin{2};
        cellheight = varargin{2};
    else
        cellwidth = 1000;
        cellheight = 1000;
    end
    
    if(nargin >= 3)
        cellheight = varargin{3};
    end

    maxx = max(outerBoundary.getX());
    maxy = max(outerBoundary.getY());
    minx = min(outerBoundary.getX());
    miny = min(outerBoundary.getY());

    w = maxx-minx;
    h = maxy-miny;
    dx = 5;
    dy = 5;
    px = w / dx;
    py = h / dy;
    nx = ceil(px / cellwidth);
    ny = ceil(py / cellheight);
    pw = minx:px / nx * dx:maxx;
    ph = miny:py / ny * dy:maxy;

    bc = 0;
    for i=1:nx
        for j=1:ny
            X = [pw(i) pw(i+1) pw(i+1) pw(i) pw(i)];
            Y = [ph(j) ph(j) ph(j+1) ph(j+1) ph(j)];
            boundary = org.kalypso.gaja3d.matlab.Polygon(X, Y);
            boundary = boundary.clip(outerBoundary);
            if(~isempty(boundary.getX()))
                bc = bc + 1;
                boundaries(bc) = boundary;
            end
        end
    end
end