package org.kalypso.ogc.sensor.diagview;

import java.util.Collection;

import org.kalypso.ogc.sensor.template.ITemplateEventProvider;

/**
 * Template for diagram
 * 
 * @author schlienger
 */
public interface IDiagramTemplate extends ITemplateEventProvider
{
  public String getTitle();
  public String getLegendName();
  public boolean isShowLegend();
  
  /**
   * @return list of <code>IDiagramAxis</code>
   */
  public Collection getDiagramAxes();
  
  /**
   * Returns the axis that has the given id.
   * 
   * @param diagAxisId
   * @return diagram axis if found or null if not found
   */
  public IDiagramAxis getDiagramAxis( final String diagAxisId );
  
  /**
   * @return list of <code>IDiagramTemplateTheme</code>
   */
  public Collection getThemes();
  
  /**
   * @return list of <code>IDiagramCurve</code>
   */
  public Collection getCurves();
  
  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}
