classdef EdgeFilter
   properties (Constant = true)
       FREI_CHEN = [1 sqrt(2) 1; 0 0 0; -1 -sqrt(2) -1] / (4 + 2 * sqrt(2)); % frei chen
       SOBEL = fspecial('sobel') / 8;
       PREWITT = fspecial('prewitt') / 6;
       DEFAULT = org.kalypso.gaja3d.matlab.EdgeFilter.FREI_CHEN;
   end
end 
