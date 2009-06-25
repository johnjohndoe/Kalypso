package de.tuhh.wb.javagis.view.netview;

import java.awt.event.MouseEvent;
public class Transformation
{
    /**
     * geometry of the viewport
     */

    public static final int _UseGK = 1;

       
    double sx1,sy1,mx1,my1;
    double mDx,mDy,sDx,sDy;
    
    public Transformation(ScreenBox screenBox,GisBox mapBox,int type)
    {
	sx1 = screenBox.getX();
	sy1 = screenBox.getY();
	sDx = screenBox.getWidth();
	sDy = screenBox.getHeight();

	mx1 = mapBox.getX();
	my1 = mapBox.getY()+mapBox.getHeight();
	mDx = mapBox.getWidth();
	mDy = mapBox.getHeight();	
    }

    public GisPoint convert(MouseEvent e)
    {
	double sx=e.getPoint().x;
	double sy=e.getPoint().y;
 	double mx=mx1+(sx/sDx)*mDx;
	double my=my1-(sy/sDy)*mDy;
	GisPoint newGisPoint=new GisPoint(mx,my);
	return newGisPoint;
    }

    public GisPoint convert(ScreenPoint s)
    {
 	double mx=mx1+(s.x/sDx)*mDx;
	double my=my1-(s.y/sDy)*mDy;
	GisPoint newGisPoint=new GisPoint(mx,my);
	return newGisPoint;
    }

    public ScreenPoint convert(GisPoint mp)
    {
	double mx=mp.x;
	double my=mp.y;
	double sx=((mx-mx1)/mDx)*sDx;
	double sy=((my1-my)/mDy)*sDy;
	//	System.out.println("screen: "+sx+" "+sy);
	return new ScreenPoint(sx,sy);
    }

    public ScreenBox convert(GisBox box)
    {
	return new ScreenBox(convert(box.getP1()),convert(box.getP2()));
    }
    
    public GisBox convert(ScreenBox box)
    {
	return new GisBox(convert(box.getP1()),convert(box.getP2()));
    }

}
