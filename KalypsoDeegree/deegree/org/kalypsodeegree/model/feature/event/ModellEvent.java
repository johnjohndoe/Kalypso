package org.kalypsodeegree.model.feature.event;

/**
 * TODO typen überarbeiten und vereinheitlichen, überall verwenden
 * 
 * @author bce
 */
public class ModellEvent
{
  /** TODO comment this stuff! What can have happened, if this event has been sent? */
  public final static long FEATURE_CHANGE = 1 << 1;

  public final static long STYLE_CHANGE = 1 << 2;

  public final static long WIDGET_CHANGE = 1 << 3;

  /** Das sendende Objekt hat sich völlig geändert */
  public static final long FULL_CHANGE = 1 << 4;

  /** TODO: this is not a modell-event type (it has nothing todo with the workspace) */
  public static final long THEME_ADDED = 1 << 5;

  /** TODO: this is not a modell-event type (it has nothing todo with the workspace) */
  public static final long LEGEND_UPDATED = 1 << 6;

  private final ModellEventProvider m_eventSource;

  private final long m_bitmaskType;

  public ModellEvent( final ModellEventProvider eventSource, final long bitmaskType )
  {
    m_eventSource = eventSource;
    m_bitmaskType = bitmaskType;
  }

  public boolean isType( long bitmask )
  {
    return ( m_bitmaskType & bitmask ) == bitmask;
  }

  public ModellEventProvider getEventSource()
  {
    return m_eventSource;
  }
}