package org.kalypso.ogc;

import org.kalypso.ogc.event.ModellEventListener;

/**
 * @author bce
 */
public interface IMapModellView extends ModellEventListener
{
  public MapModell getMapModell();
  public void setMapModell(MapModell modell);
}
