package org.kalypso.ogc.sensor.diagview;

import java.awt.Paint;

import org.kalypso.eclipse.ui.IViewable;


/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramCurve extends IViewable
{
  /**
   * @return name of the curve as displayed in the diagram
   */
  public String getName();
  
  /**
   * @return list of mappings between diagram axes and observation axes
   */
  public IAxisMapping[] getMappings();
  
  /**
   * @return theme to which this curve belongs to
   */
  public IDiagramTemplateTheme getTheme();

  public void setName( String string );
  
  /**
   * @return paint
   */
  public Paint getPaint();

  public void setShown( boolean shown); 
}
