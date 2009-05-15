toDelete = l_elements(l_maxArea~=-1,:);
l_elements(l_maxArea~=-1,:)=[];
danglingPoints = unique(toDelete(:));
innerPoints = int32([]);
outerPoints = int32([]);
toMove = int32(zeros(size(l_elements)));
for i=1:numel(danglingPoints)
    if(~any(any(l_elements==danglingPoints(i))))
        innerPoints(end+1)=danglingPoints(i);
        toMove = toMove + int32(l_elements > danglingPoints(i));
    else
        outerPoints(end+1)=danglingPoints(i);
    end
end
outerPointsXYZ = l_points(outerPoints,:);
l_elements = l_elements - toMove;
l_points(innerPoints,:)=[];
C = [l_elements(:,[1 2]); l_elements(:,[2 3]); l_elements(:,[3 1])];
tri = DelaunayTri(l_points(:,1),l_points(:,2),double(C));
l_elements = tri.Triangulation;
tricount = size(l_elements,1);
allX = zeros(size(l_elements));
allY = zeros(size(l_elements));
allZ = zeros(size(l_elements));
allX(:) = l_points(l_elements,1);
allY(:) = l_points(l_elements,2);
allZ(:) = l_points(l_elements,3);
areas = polyarea(allX, allY, 2);
elementHeights = max(allZ,[],2) - min(allZ,[],2);
maxHeight = ones(tricount, 1) * maxHeightDefault;
lengthFractions = maxHeight ./ elementHeights;
areaFactor = lengthFractions .* lengthFractions;
l_maxArea = areas .* areaFactor;
l_maxArea(lengthFractions >= 1) = -1;
l_maxArea(isnan(l_maxArea)) = -1;
this.maxArea = ones(tricount, 1) * -1;
this.maxArea = min(l_maxArea, this.maxArea);
exportNodes(l_points, tempfile);
saveTrianglesAsEle(l_elements, [tempfile '.ele']);