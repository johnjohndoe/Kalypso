package org.kalypso.ogc;

import org.kalypso.ogc.event.ModellEventListener;

/**
 * @author vDoemming
 */
public interface IMapModellView extends ModellEventListener
{
  public IMapModell getMapModell();
  public void setMapModell( final IMapModell modell );
}
