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
}
