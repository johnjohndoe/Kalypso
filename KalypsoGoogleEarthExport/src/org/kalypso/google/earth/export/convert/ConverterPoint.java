/**
 *
 */
package org.kalypso.google.earth.export.convert;

import java.util.List;

import org.kalypso.google.earth.export.utils.GoogleEarthUtils;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;

import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.PointType;

/**
 * @author kuch
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
    coordinates.add( String.format( "%f,%f", kmlPoint.getX(), kmlPoint.getY() ) );

    return pointType;
  }

}
