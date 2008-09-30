/**
 *
 */
package org.kalypso.kml.export.convert;

import java.util.List;
import java.util.Locale;

import org.kalypso.kml.export.utils.GoogleEarthUtils;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Point;

import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PointType;

/**
 * @author Dirk Kuch
 */
public class ConverterPoint
{

  /**
   * @param factory
   * @param gmo
   * @param style
   * @return
   * @throws Exception
   */
  public static PointType convert( final ObjectFactory factory, final GM_Point gmo ) throws Exception
  {
    final PointType pointType = factory.createPointType();

    final GeoTransformer transformer = new GeoTransformer( GoogleEarthUtils.GOOGLE_EARTH_CS );
    final GM_Point kmlPoint = (GM_Point) transformer.transform( gmo );

    final List<String> coordinates = pointType.getCoordinates();
    coordinates.add( String.format( Locale.ENGLISH, "%f,%f", kmlPoint.getX(), kmlPoint.getY() ) ); //$NON-NLS-1$

    return pointType;
  }

}
