package org.kalypso.ogc.event;

/**
 * @author bce
 */
public class ModellEvent
{
  public final static int FEATURE_CHANGE = 1;

  public final static int STYLE_CHANGE = 2;

  public final static int WIDGET_CHANGE = 3;

  /** Das sendende Objekt hat sich völlig geändert */
  public static final int FULL_CHANGE = 4;

  private final int myType;

  private final ModellEventProvider m_eventSource;

  public ModellEvent( final ModellEventProvider eventSource, final int type )
  {
    m_eventSource = eventSource;
    myType = type;
  }

  public int getType()
  {
    return myType;
  }
  
  public ModellEventProvider getEventSource()
  {
    return m_eventSource;
  }
}