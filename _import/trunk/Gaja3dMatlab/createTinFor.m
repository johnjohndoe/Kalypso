function [points, elements] = createTinFor(gaja3d, breaklines, maxArea, minAngle)
    % set breaklines
    gaja3d.setBreaklines(breaklines);

    % create mesh
    gaja3d.createTin('maxArea', maxArea, 'minAngle', minAngle);
    gaja3d.assignElevations();
    
    % display
    points = gaja3d.modelTin.points;
    elements = gaja3d.modelTin.elements;
    
    figure(1);
    trisurf(elements, points(:,1), points(:,2), points(:,3), 'FaceColor', 'interp', 'EdgeColor', 'black');
end