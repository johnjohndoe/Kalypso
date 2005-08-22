package org.kalypsodeegree.model.feature.event;

/**
 * TODO typen �berarbeiten und vereinheitlichen, �berall verwenden
 * 
 * @author bce
 */
public class ModellEvent
{

  public final static long FEATURE_CHANGE = 1 << 1;

  public final static long STYLE_CHANGE = 1 << 2;

  public final static long WIDGET_CHANGE = 1 << 3;

  /** Das sendende Objekt hat sich v�llig ge�ndert */
  public static final long FULL_CHANGE = 1 << 4;

  public static final long THEME_ADDED = 1 << 5;

  public static final long LEGEND_UPDATED = 1 << 6;

  public static final long SELECTION_CHANGED = 1 << 7;

  public static final long EDITING_CHANGED = 1 << 8;
  
  public static final long TREE_SELECTION_CHANGED = 1 << 9;

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