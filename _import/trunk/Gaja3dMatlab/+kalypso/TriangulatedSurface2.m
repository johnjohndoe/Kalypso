classdef TriangulatedSurface2 < handle
    
	properties (SetAccess = private)
        triangulation, maxXY, minXY, minZ, maxZ
    end % private
    
    methods
        % Constructor
        function this = TriangulatedSurface2(points)
            if(nargin == 0)
                return;
            elseif(nargin == 1)
                this.setPoints(points);
            else
                % this will throw an error at the time
                error('Call constructor only with 0 or 1 arguments.');
            end
        end
        
        % initialize interpolation and min/max
        function setPoints(this, points)
            pointSize = size(points);
            if(pointSize(1) >= 3 && pointSize(2) >= 3)
                XY = points(:,1:2);
                Z = points(:,3);
                this.triangulation = TriScatteredInterp(XY, Z, 'linear');
                this.maxXY = max(XY);
                this.minXY = min(XY);
                this.minZ = min(Z);
                this.maxZ = max(Z);
            else
                error('Points must be a Nx3 array with N >= 3');
            end
        end
        
        % fall back to triangulation indexing for ()
        function B = subsref(this, S)
            switch(S(1).type)
                case '.'
                    fieldName = S(1).subs;
                    fields = fieldnames(this);
                    switch(fieldName)
                        case fields
                            inner = this.(fieldName);
                        otherwise
                            inner = subsref(this.triangulation, S(1));
                    end
                    if(numel(S) > 1)
                        B = subsref(inner, S(2:end));
                    else
                        B = inner;
                    end
                case '()'
                    subs = S(1).subs;
                    if(all(size(subs{1}) == 1))
                        inner = this(subs{1});
                        if(numel(S) > 1)
                            B = subsref(inner, S(2:end));
                        else
                            B = inner;
                        end
                    else
                        B = subsref(this.triangulation, S);
                    end
            end
        end
        
        % delete handle
        function delete(this)
            this.triangulation = [];
        end
    end % public methods
end 

