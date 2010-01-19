h = figure(1);
units=get(h,'units');
set(h,'units','normalized','outerposition',[0 0 1 1]);
set(h,'units',units);
[p,a,e] = loadBce2d(sprintf('P:/transfer/nico/temp/demo2d/A%04d.2d',1),'VA');
plotBce2d(p,a,e);
hold on;
isWet = p(:,7) > p(:,3);
tri = kalypso.TriangulatedSurface(p(isWet,[1 2 7]));
points = tri.points;
elements = tri.elements;
tsurf = trisurf(elements, points(:,1), points(:,2), points(:,3),'FaceColor','blue','EdgeColor','none','FaceAlpha',0.5);
axis([3928500 3930500 773500 775500 366.5 367.5]);
view(0,85);
daspect([3000 3000 1]);
camlight headlight
lighting phong
for i=1:18
    [p,a,e] = loadBce2d(sprintf('P:/transfer/nico/temp/demo2d/A%04d.2d',i),'VA');
    isWet = p(:,7) > p(:,3);
    tri = kalypso.TriangulatedSurface(p(isWet,[1 2 7]));
    points = tri.points;
    elements = tri.elements;
    set(tsurf,'vertices',points);
    set(tsurf,'faces',elements);
    set(tsurf,'FaceVertexCData',points(:,3));
    drawnow 
    print('-djpeg', sprintf('P:/transfer/nico/temp/demo2d/A%04d.jpg',i));
end