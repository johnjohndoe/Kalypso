package org.kalypso.ogc.sensor.diagview;

/**
 * Template for diagram
 * 
 * @author schlienger
 *
 */
public interface IDiagramTemplate
{
  public String getTitle();
  
  public String getLegendName();
  public boolean isShowLegend();
  
  public IDiagramAxis[] getAxisList();
  
  public ICurve[] getCurveList();
}
