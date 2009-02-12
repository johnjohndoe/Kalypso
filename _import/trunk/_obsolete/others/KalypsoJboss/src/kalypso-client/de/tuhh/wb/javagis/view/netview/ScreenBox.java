package de.tuhh.wb.javagis.view.netview;

// Box with GK-points
public class ScreenBox
{
    public ScreenPoint p1,p2;

    public ScreenPoint getP1()
    {
	return p1;
    }
    
    public ScreenPoint getP2()
    {
	return p2;
    }

    public ScreenBox(double x, double y, double w, double h)
    {    
	p1=new ScreenPoint(x,y);
	p2=new ScreenPoint(x+w,y+h);
    }

    public ScreenBox(ScreenPoint p1,ScreenPoint p2)
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
	this.p1=new ScreenPoint(x,y);
	this.p2=new ScreenPoint(x+w,y+h);	
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
