package org.kalypso.ogc.gml.mapmodel;

import org.kalypso.ogc.gml.event.ModellEventListener;

/**
 * @author vDoemming
 */
public interface IMapModellView extends ModellEventListener
{
  public IMapModell getMapModell();
  public void setMapModell( final IMapModell modell );
}
