package org.kalypso.ogc.gml.mapmodel;

import org.deegree.model.feature.event.ModellEventListener;

/**
 * @author vDoemming
 */
public interface IMapModellView extends ModellEventListener
{
  public IMapModell getMapModell();
  public void setMapModell( final IMapModell modell );
}
