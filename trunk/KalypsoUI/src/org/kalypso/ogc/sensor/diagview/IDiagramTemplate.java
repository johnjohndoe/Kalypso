package org.kalypso.ogc.sensor.diagview;

import java.util.List;
import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.template.ITemplateEventProvider;

/**
 * Template for diagram
 * 
 * @author schlienger
 */
public interface IDiagramTemplate extends ITemplateEventProvider
{
  public String getTitle();
//  public void setTitle( String title );
  
  public String getLegendName();
//  public void setLegendName( String name );
  public boolean isShowLegend();
//  public void setShowLegend( boolean show );
  
  /**
   * @return list of <code>IDiagramAxis</code>
   */
  public List getDiagramAxes();
//  public void addAxis( IDiagramAxis axis );
//  public void removeAxis( IDiagramAxis axis );
  
  /**
   * Finds the given axis within the template's axes.
   * 
   * @param id
   * @return diagram axis if found
   * @throws NoSuchElementException
   */
  public IDiagramAxis findAxis( final String id ) throws NoSuchElementException;
  
  /**
   * @return list of <code>IDiagramCurve</code>
   */
  public List getCurves();
//  public void addCurve( IDiagramCurve curve );
//  public void removeCurve( IDiagramCurve curve );
//  public void removeAllCurves();
  
  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}
