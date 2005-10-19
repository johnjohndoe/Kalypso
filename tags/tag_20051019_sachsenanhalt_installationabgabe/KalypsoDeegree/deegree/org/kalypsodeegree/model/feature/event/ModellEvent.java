package org.kalypsodeegree.model.feature.event;

/**
 * TODO typen �berarbeiten und vereinheitlichen, �berall verwenden
 * 
 * @author bce
 */
public class ModellEvent
{
  /** TODO comment this stuff! What can have happened, if this event has been sent? (Value 2) */
  public final static long FEATURE_CHANGE = 1 << 1;

  /** (Value 4) */
  public final static long STYLE_CHANGE = 1 << 2;

  /** (Value 8) */
  public final static long WIDGET_CHANGE = 1 << 3;

  /** Das sendende Objekt hat sich v�llig ge�ndert (Value 16) */
  public static final long FULL_CHANGE = 1 << 4;

  /** TODO: this is not a modell-event type (it has nothing todo with the workspace) (Value 32) */
  public static final long THEME_ADDED = 1 << 5;

  /** TODO: this is not a modell-event type (it has nothing todo with the workspace) (Value 64) */
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