package org.deegree_impl.model.sort;
    
import java.awt.Graphics;
import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.displayelements.LineStringDisplayElement;
import org.deegree.graphics.displayelements.PointDisplayElement;
import org.deegree.graphics.displayelements.PolygonDisplayElement;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.graphics.displayelements.DisplayElementFactory;

public class DisplayContext
{

    private boolean isSelected=false;
    private Feature myFE=null;
    private DisplayElement myDE[]=null;

    public DisplayContext(Feature feature)
    {
	myFE=feature;
    }

    public DisplayContext(Feature feature,UserStyle styles[])
    {
	myFE=feature;
	updateDisplayElements(styles);
    }

    public boolean isSelected()
    {
	return isSelected;
    }

    public boolean select()
    {
	if(isSelected)
	    return false;
	else
	    {
		isSelected=true;
		return true;
	    }
    }
    
    public boolean unselect()
    {
	if(!isSelected)
	    return false;
	else
	{
	    isSelected=false;
	    return true;
	}
    }

    public void toggle()
    {
	isSelected=!isSelected;
    }
    

    public void paint(Graphics g,GeoTransform projection)
    {
	if(myDE!=null)
	    for(int i=0;i<myDE.length;i++)
		myDE[i].paint(g,projection);
    }

    public void paint(Graphics g,GeoTransform projection,double scale)
    {
	if(myDE!=null)
	    for(int i=0;i<myDE.length;i++)
		if (myDE[i].doesScaleConstraintApply(scale))
		    myDE[i].paint(g,projection);
    }

    public void paintSelected(Graphics g,GeoTransform projection,double scale)
    {
	if(myDE!=null)
	    for(int i=0;i<myDE.length;i++)
		if (isSelected() 
		    && ( myDE[i] instanceof PointDisplayElement
			 || myDE[i] instanceof LineStringDisplayElement
			 || myDE[i] instanceof PolygonDisplayElement)
		    && myDE[i].doesScaleConstraintApply(scale))
		    myDE[i].paint(g,projection);
    }

    public void updateDisplayElements(UserStyle styles[])
    {
	try
	    {
		myDE = DisplayElementFactory.createDisplayElement(myFE,styles);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		myDE=null;
	    }
    }

    public Feature getFeature()
    {
	return myFE;
    }

    public GM_Object getGeometry()
    {
	return myFE.getDefaultGeometryProperty();
    }
}
