package org.kalypso.ogc.sensor.diagview;




/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramCurve
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

  /**
   * @param string
   */
  public void setName( String string );
}
