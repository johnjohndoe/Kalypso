package org.kalypso.google.earth.export.interfaces;

import org.kalypsodeegree.model.feature.Feature;

public interface IGoogleEarthAdapter
{
  /**
   * Point geometries can define place markers, this means, which image will be displayed for an point TODO add name,
   * description (additional informations to these point markers)
   */
  public IPlacemarker getPlacemarker( Feature feature );

  /**
   * additional place markers, perhaps for measures and their details (images, links, aso)...
   */
  public IPlacemarker[] getAdditionalPlacemarkers( );

  // TODO handle special geometries...
}
