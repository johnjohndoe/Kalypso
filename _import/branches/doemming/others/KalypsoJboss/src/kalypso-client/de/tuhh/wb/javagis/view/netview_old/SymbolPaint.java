package de.tuhh.wb.javagis.view.netview;

import java.awt.Image;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Component;
import java.util.Vector;

public class SymbolPaint
{
	private Image nullStrand;
	private Image damStrand;
	private Image reservoirStrand;
	private Image channelStrand;
	private Image riverBasin;
	private Image node;
	private GisMap gisMap;
	
	public SymbolPaint(GisMap gisMap)
	{
		this.gisMap = gisMap;
		nullStrand = this.gisMap.getToolkit().getImage("NullStrandneu.gif");
		damStrand = this.gisMap.getToolkit().getImage("DamStrandneu.gif");
		reservoirStrand = this.gisMap.getToolkit().getImage("ReservoirStrandneu.gif");
		channelStrand = this.gisMap.getToolkit().getImage("Channelneu.gif");
		riverBasin = this.gisMap.getToolkit().getImage("RiverBasinneu.gif");
		node = this.gisMap.getToolkit().getImage("Nodeneu.gif");
	}
   
    public void paint(Graphics g,String className,String text,int x,int y)
    {
	if (className.equals("nullStrand"))
	    {
		drawNullStrand(g,x,y,text);
	    }
	if (className.equals("channel"))
	    {
		drawChannelStrand(g,x,y,text);
	    }
	if (className.equals("rhb"))
	    {
		drawReservoirStrand(g,x,y,text);
	    }
	if (className.equals("rht"))
	    {
		drawDamStrand(g,x,y,text);
	    }
	if (className.equals("node"))
	    {
		drawNode(g,x,y,text);
	    }
	/**for (int k = 0; k<myGisRelationClasses.length; k++)
	   {
	   String relation = myGisRelationClasses[k];
	   if (relation.equals("StrandToNode"))
	   {
	   drawStrandToNode(g,x1,y1,x2,y2);
	   }
	   if (relation.equals("NodeToStrand"))
					{
					drawStrandToNode(g,x2,y2,x1,y1);
					}
					}*/
	
	if (className.equals("rb"))
	    {
		drawRiverBasin(g,x,y,text);
	    }
    }
    
   
    public void drawChannelStrand(Graphics g, int x, int y, String name)
	{
		g.setColor(Color.green);
		//g.drawRect(x,y,50,20);
		g.drawImage(channelStrand,x,y,gisMap);
		g.drawString((name),x,(y-5));
	}
	public void drawReservoirStrand(Graphics g, int x, int y, String name)
	{
		g.setColor(Color.yellow);
		//g.drawRect(x,y,50,20);
		g.drawImage(reservoirStrand,x,y,gisMap);
		g.drawString(name,x,(y-5));
	}
	public void drawNullStrand(Graphics g, int x, int y, String name)
	{
		g.setColor(Color.orange);
		//g.drawRect(x,y,50,20);
		g.drawImage(nullStrand,x,y,gisMap);
		g.drawString(name,x,(y-5));
	}
	public void drawDamStrand(Graphics g, int x, int y, String name)
	{
		g.setColor(Color.gray);
		//g.drawRect(x,y,50,20);
		g.drawImage(damStrand,x,y,gisMap);
		g.drawString(name,x,(y-5));
	}
	public void drawRiverBasin(Graphics g, int x, int y, String name)
	{
		g.setColor(Color.blue);
		//g.drawLine(x,y,(x+25),(y+50));
		//g.drawLine((x+25),(y+50),(x+50),y);
		//g.drawLine((x+50),y,x,y);
		g.drawImage(riverBasin,x,y,gisMap);
		g.drawString(name,x,(y-5));
	}
	public void drawNode(Graphics g, int x, int y, String name)
	{
		g.setColor(Color.black);
		//g.drawOval(x,y,10,10);
		g.drawImage(node,x,y,gisMap);
		g.drawString(name,x,(y-5));
	}
    /*
	public static void drawStrandToNode(Graphics g, int startRelationPointX, int startRelationPointY, int endRelationPointX, int endRelationPointY)
	{
		g.setColor(Color.red);
		g.drawLine((startRelationPointX+50),(startRelationPointY+10),(endRelationPointX),(endRelationPointY+5));
		g.drawLine((endRelationPointX),(endRelationPointY+5),(endRelationPointX-10),(endRelationPointY));
		g.drawLine((endRelationPointX),(endRelationPointY+5),(endRelationPointX-10),(endRelationPointY+10));
	}
    public void drawNodeToStrand(Graphics g, int startRelationPointX, int startRelationPointY, int endRelationPointX, int endRelationPointY)
	{
		g.setColor(Color.red);
		g.drawLine((startRelationPointX+10),(startRelationPointY+5),(endRelationPointX),(endRelationPointY+10));
		g.drawLine((endRelationPointX),(endRelationPointY+10),(endRelationPointX-10),(endRelationPointY+5));
		g.drawLine((endRelationPointX),(endRelationPointY+10),(endRelationPointX-10),(endRelationPointY+15));
	}
    */
}
