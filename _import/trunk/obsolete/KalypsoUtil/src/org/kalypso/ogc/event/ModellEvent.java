package org.kalypso.ogc.event;

/**
 * @author bce
 */
public class ModellEvent
{
  public final static int FEATURE_CHANGE=1;
  public final static int STYLE_CHANGE=2;
  public final static int WIDGET_CHANGE=3;
  
  private final int myType;
 public ModellEvent(int type)
 {
  	myType=type;
  }
  public int getType()
  {
  	return myType;
  }
}
