package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.template.ITemplateAdapter;

/**
 * Template for diagram
 * 
 * @author schlienger
 */
public interface IDiagramTemplate extends ITemplateAdapter
{
  public String getTitle();
  
  public String getLegendName();
  public boolean isShowLegend();
  
  public IDiagramAxis[] getAxisList();
  
  public ICurve[] getCurveList();
}
