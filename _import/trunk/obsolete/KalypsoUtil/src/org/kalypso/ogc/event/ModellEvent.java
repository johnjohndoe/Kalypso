package org.kalypso.ogc.event;

/**
 * @author bce
 */
public interface ModellEvent
{
  public final static int FEATURE_CHANGE=1;
  public int getType();
}
