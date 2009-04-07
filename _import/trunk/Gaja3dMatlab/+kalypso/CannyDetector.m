classdef CannyDetector < org.kalypso.gaja3d.matlab.FeatureDetector
   methods
       function this = CannyDetector(varargin)
           this = this@org.kalypso.gaja3d.matlab.FeatureDetector(varargin{:});
       end
       function breakpoints = process(this, grid)
           [ax, ay, mag, highThresh, lowThresh] = this.getInputs(grid);
           
           % initialize breakpoints
           [m,n] = size(mag);
           breakpoints = false(m,n);
           
           if(isempty(breakpoints))
               % do not try to calculate breakpoints
               return;
           end
           
           % The next step is to do the non-maximum supression.  
           % We will accrue indices which specify ON pixels in strong edgemap
           % The array breakpoints will become the weak edge map.
           idxStrongCell = cell(1,4);
           for dir = 1:4
              idxLocalMax = cannyFindLocalMaxima(dir,ax,ay,mag);
              idxWeak = idxLocalMax(mag(idxLocalMax) > lowThresh);
              breakpoints(idxWeak)=1;
              idxStrongCell{dir} = idxWeak(mag(idxWeak) > highThresh);
           end
           idxStrong = vertcat(idxStrongCell{:});
           if ~isempty(idxStrong) % result is all zeros if idxStrong is empty
              rstrong = rem(idxStrong-1, m)+1;
              cstrong = floor((idxStrong-1)/m)+1;
              breakpoints = bwselect(breakpoints, cstrong, rstrong, 8);
              % Thin double (or triple) pixel wide contours
              breakpoints = bwmorph(breakpoints, 'thin', Inf);
           end
       
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
        %   Local Function : cannyFindLocalMaxima
        %
        function idxLocalMax = cannyFindLocalMaxima(direction,ix,iy,mag)
        %
        % This sub-function helps with the non-maximum supression in the Canny
        % edge detector.  The input parameters are:
        % 
        %   direction - the index of which direction the gradient is pointing, 
        %               read from the diagram below. direction is 1, 2, 3, or 4.
        %   ix        - input image filtered by derivative of gaussian along x 
        %   iy        - input image filtered by derivative of gaussian along y
        %   mag       - the gradient magnitude image
        %
        %    there are 4 cases:
        %
        %                         The X marks the pixel in question, and each
        %         3     2         of the quadrants for the gradient vector
        %       O----0----0       fall into two cases, divided by the 45 
        %     4 |         | 1     degree line.  In one case the gradient
        %       |         |       vector is more horizontal, and in the other
        %       O    X    O       it is more vertical.  There are eight 
        %       |         |       divisions, but for the non-maximum supression  
        %    (1)|         |(4)    we are only worried about 4 of them since we 
        %       O----O----O       use symmetric points about the center pixel.
        %        (2)   (3)        

        % Find the indices of all points whose gradient (specified by the 
        % vector (ix,iy)) is going in the direction we're looking at.  

        switch direction
         case 1
          idx = find((iy<=0 & ix>-iy)  | (iy>=0 & ix<-iy));
         case 2
          idx = find((ix>0 & -iy>=ix)  | (ix<0 & -iy<=ix));
         case 3
          idx = find((ix<=0 & ix>iy) | (ix>=0 & ix<iy));
         case 4
          idx = find((iy<0 & ix<=iy) | (iy>0 & ix>=iy));
        end

        % Exclude the exterior pixels
        if ~isempty(idx)
          v = mod(idx,m);
          extIdx = v==1 | v==0 | idx<=m | (idx>(n-1)*m);
          idx(extIdx) = [];
        end

        ixv = ix(idx);  
        iyv = iy(idx);   
        gradmag = mag(idx);

        % Do the linear interpolations for the interior pixels
        switch direction
         case 1
          d = abs(iyv./ixv);
          gradmag1 = mag(idx+m).*(1-d) + mag(idx+m-1).*d; 
          gradmag2 = mag(idx-m).*(1-d) + mag(idx-m+1).*d; 
         case 2
          d = abs(ixv./iyv);
          gradmag1 = mag(idx-1).*(1-d) + mag(idx+m-1).*d; 
          gradmag2 = mag(idx+1).*(1-d) + mag(idx-m+1).*d; 
         case 3
          d = abs(ixv./iyv);
          gradmag1 = mag(idx-1).*(1-d) + mag(idx-m-1).*d; 
          gradmag2 = mag(idx+1).*(1-d) + mag(idx+m+1).*d; 
         case 4
          d = abs(iyv./ixv);
          gradmag1 = mag(idx-m).*(1-d) + mag(idx-m-1).*d; 
          gradmag2 = mag(idx+m).*(1-d) + mag(idx+m+1).*d; 
        end
        idxLocalMax = idx(gradmag>=gradmag1 & gradmag>=gradmag2); 
        end
       end
   end
end 
