package de.tuhh.wb.javagis.view.netview;


// Box with GK-points
public class GisBox
{
    public GisPoint p1,p2;

    public GisBox(double x, double y, double w, double h)
    {    
	p1=new GisPoint(x,y);
	p2=new GisPoint(x+w,y+h);
    }
    
    public GisBox(GisPoint p1,GisPoint p2)
    {
	double x,y,w,h;
	if(p1.x<p2.x)
	    {
		x=p1.x;
		w=p2.x-p1.x;
	    }
	else
	    {
		x=p2.x;
		w=p1.x-p2.x;
	    }
	if(p1.y<p2.y)
	    {
		y=p1.y;
		h=p2.y-p1.y;
	    }
	else
	    {
		y=p2.y;
		h=p1.y-p2.y;
	    }
	this.p1=new GisPoint(x,y);
	this.p2=new GisPoint(x+w,y+h);	
    }

    public GisBox getNorthHalf()
    {
	double w=p2.x-p1.x;
	double h=p2.y-p1.y;
	return new GisBox(p1.x, p1.y, w, h/2);
    }
    public GisBox getSouthHalf()
    {
	double w=p2.x-p1.x;
	double h=p2.y-p1.y;
	return new GisBox(p1.x, (p1.y+p2.y)/2, w, h/2);
    }
    public GisBox getWestHalf()
    {
	double w=p2.x-p1.x;
	double h=p2.y-p1.y;
	return new GisBox(p1.x, p1.y, w/2, h);
    }
    public GisBox getEastHalf()
    {
	double w=p2.x-p1.x;
	double h=p2.y-p1.y;
	return new GisBox((p1.x+p2.x)/2, p1.y, w/2, h);
    }
 
    public boolean intersects(GisBox b)
    {
	if(b.p1.x>p2.x)
	    return false;
	if(b.p2.x<p1.x)
	    return false;
	if(b.p1.y>p2.y)
	    return false;
	if(b.p2.y<p1.y)
	    return false;
	return true;
    }

    public double getX()
    {
	return p1.x;
    }

    public double getY()
    {
	return p1.y;
    }
    public double getWidth()
    {
	return p2.x-p1.x;
    }
    public double getHeight()
    {
	return p2.y-p1.y;
    }
}
