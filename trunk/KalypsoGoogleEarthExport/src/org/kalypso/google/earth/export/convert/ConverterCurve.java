/**
 *
 */
package org.kalypso.google.earth.export.convert;

import java.util.List;

import org.kalypso.google.earth.export.utils.GoogleEarthUtils;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.google.earth.kml._2.LineStringType;
import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.StyleType;

/**
 * @author kuch
 */
public class ConverterCurve
{

  /**
   * @param factory
   * @param string
   * @param gmo
   * @param style
   * @throws Exception
   */
  public static LineStringType convert( final ObjectFactory factory, final GM_Curve curve, final StyleType style ) throws Exception
  {
    final LineStringType lineStringType = factory.createLineStringType();
    final List<String> coordinates = lineStringType.getCoordinates();

    final GeoTransformer transformer = new GeoTransformer( GoogleEarthUtils.GOOGLE_EARTH_CS );

    final GM_LineString lineString = curve.getAsLineString();
    final GM_Position[] positions = lineString.getPositions();

    for( final GM_Position position : positions )
    {
      final GM_Point point = GeometryFactory.createGM_Point( position, curve.getCoordinateSystem() );
      final GM_Point kmlPoint = (GM_Point) transformer.transform( point );

      coordinates.add( String.format( "%f,%f", kmlPoint.getX(), kmlPoint.getY() ) );
    }

    return lineStringType;
  }
}
