package org.kalypso.ogc.sensor.diagview;

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
  public void setTitle( String title );
  
  public String getLegendName();
  public void setLegendName( String name );
  public boolean isShowLegend();
  public void setShowLegend( boolean show );
  
  public IDiagramAxis[] getAxisList();
  public void addAxis( IDiagramAxis axis );
  public void removeAxis( IDiagramAxis axis );
  public IDiagramAxis findAxis( String id ) throws NoSuchElementException;
  
  public IDiagramCurve[] getCurveList();
  public void addCurve( IDiagramCurve curve );
  public void removeCurve( IDiagramCurve curve );
  public void removeAllCurves();  
}
