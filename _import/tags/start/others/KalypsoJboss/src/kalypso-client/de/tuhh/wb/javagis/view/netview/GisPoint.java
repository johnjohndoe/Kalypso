package de.tuhh.wb.javagis.view.netview;

import de.tuhh.wb.javagis.model.BasePointTransfer;
import java.awt.geom.Point2D;
public class GisPoint extends Point2D.Double
{
    public GisPoint(BasePointTransfer bp)
    {
	super(bp.getX(),bp.getY());
    }
    public GisPoint()
    {
	super();
    }
    public GisPoint(double x, double y)
    {	
	super(x,y);
    }    
    public BasePointTransfer toBasePointTransfer()
    {
	return new BasePointTransfer(getX(),getY());
    }
}
