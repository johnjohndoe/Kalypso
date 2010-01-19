function [points, elements] = trisplit( points, elements )
    % remember point and element count
    pcount = size(points,1);
    ecount = size(elements,1);
    
    newpoints = cell(ecount,1);
    newelements = cell(ecount,1);
    for i=1:ecount
        pointids = elements(i,1:3);
        triXYZ = points(pointids,:);

        % find out points with z<0
        isbelow = triXYZ(:,3) < 0;
        sumbelow = sum(isbelow);

        % if all or none of the points have z<0, return original triangle
        if(sumbelow == 0 || sumbelow == 3)
            continue;
        end

        % get differences dx, dy, dz on triangle edges 1-2, 1-3 and 2-3
        dxyz = triXYZ([1 1 2],:) - triXYZ([2 3 3],:);

        % get slope dz/dx, dz/dy, dz/dz (=1)
        mxyz = dxyz(:,[3 3 3]) ./ dxyz(:,[1 2 3]);

        % get b in line equations z=mx+b, z=my+b, z=mz+b (b=0)
        bxyz = triXYZ([1 1 2],[3 3 3]) - mxyz .* triXYZ([1 1 2],[1 2 3]);

        % get intersection points with z=0 for x, y, z (0)
        itsctxyz = - bxyz ./ mxyz;

        % invert for two below (=one up)
        if(sumbelow == 2)
            isbelow = ~isbelow;
        end

        % reverse isbelow
        isbelow = isbelow([3 2 1]);

        %calculate remaining split as mean of neighboring points
        pairs = [1 1 2; [2 3 3]]';
        itsctxyz(isbelow,:) = mean(triXYZ(pairs(isbelow,:),:));
        
        % append split points
        newpoints{i} = itsctxyz;
        
        % append new triangles
        newelements{i} = [pointids(1) pcount+1 pcount+2; pcount+2 pcount+1 pcount+3; pcount+2 pcount+3 pointids(3); pcount+3 pcount+1 pointids(2) ];
        
        pcount = pcount + 3;
    end
   
    % remove old triangles and append new ones
    toberemoved = ~cellfun(@isempty, newelements);
    elements(toberemoved,:) = [];
    elements = [elements; vertcat(newelements{:})];
    
    % append new points
    points = [points; vertcat(newpoints{:})];
end

