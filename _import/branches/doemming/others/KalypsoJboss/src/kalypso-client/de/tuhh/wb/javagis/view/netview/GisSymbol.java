package de.tuhh.wb.javagis.view.netview;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.BasicStroke;


import java.awt.Color;
import java.util.Vector;

public class GisSymbol
{
    Vector symbolPoints;
    double width;
    double height;

    public GisSymbol()
    {
	this.symbolPoints=new Vector();
	this.width=0d;
	this.height=0d;	
    }
    public void add(double x,double y)
    {
	symbolPoints.add(new ScreenPoint(x,y));	
    }

    public void add(ScreenPoint sp)
    {
	symbolPoints.add(sp);
    }
    
    public int size()
    {
	return symbolPoints.size();
    }

    public ScreenPoint pointAt(int i)
    {
	return (ScreenPoint)symbolPoints.elementAt(i);
    }
}
