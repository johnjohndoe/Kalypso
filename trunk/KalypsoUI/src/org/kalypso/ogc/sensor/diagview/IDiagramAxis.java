package org.kalypso.ogc.sensor.diagview;

/**
 * An axis in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramAxis
{
  public final static String DIRECTION_HORIZONTAL = "horizontal";
  public final static String DIRECTION_VERTICAL = "vertical";
  
  public final static String POSITION_LEFT = "left";
  public final static String POSITION_RIGHT = "right";
  public final static String POSITION_BOTTOM = "bottom";
  public final static String POSITION_TOP = "top";
  
  
  public String getLabel();
  
  public String getUnit();
  
  public String getDirection();
  
  public String getPosition();
  
  public boolean isInverted();

  public String getDataType();
  
  public String getIdentifier();
  
  /**
   * The lower margin is expressed in percent of the whole axis range.
   * 
   * @return the lower margin in percent (for instance 0.07 for 7%) or null if not set
   */
  public Double getLowerMargin();

  /**
   * The upper margin is expressed in percent of the whole axis range.
   * 
   * @return the upper margin in percent (for instance 0.07 for 7%) or null if not set
   */
  public Double getUpperMaring();
  
  /**
   * @return complete Label of this axis (concatenates the label and the unit)
   */
  public String toFullString();
}
