package org.kalypso.ogc.event;

/**
 * @author bce
 */
public interface ModellEventProvider
{
  public void addModellListener(final ModellEventListener listener);

  public void removeModellListener(final ModellEventListener listener);

  public void fireModellEvent(final ModellEvent event);
}
