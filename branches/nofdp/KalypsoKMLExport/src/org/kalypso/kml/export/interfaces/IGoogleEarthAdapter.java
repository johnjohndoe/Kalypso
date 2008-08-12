package org.kalypso.kml.export.interfaces;

import org.kalypsodeegree.model.feature.Feature;

public interface IGoogleEarthAdapter
{
  public static String ID = "org.kalypso.google.earth.export.googleEarthAdapter"; //$NON-NLS-1$

  /**
   * Point geometries can define place markers, this means, which image will be displayed for an point TODO add name,
   * description (additional informations to these point markers)
   */
  public IPlacemarkIcon getPlacemarkIcon( Feature feature );

  /**
   * additional place markers, perhaps for measures and their details (images, links, aso)...
   */
  public IPlacemark[] getAdditionalPlacemarkers( );

  /**
   * needed for additional place marks - all rendered features will be registered.
   */
  public void registerExportedFeature( Feature feature );

  public void cleanUp( );
}
