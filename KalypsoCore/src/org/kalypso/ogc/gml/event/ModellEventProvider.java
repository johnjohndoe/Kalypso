package org.kalypso.ogc.gml.event;

/**
 * @author bce
 */
public interface ModellEventProvider
{
  public void addModellListener( final ModellEventListener listener );

  public void removeModellListener( final ModellEventListener listener );

  public void fireModellEvent( final ModellEvent event );
}