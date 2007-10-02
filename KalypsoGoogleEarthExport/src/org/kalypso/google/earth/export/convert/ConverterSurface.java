/**
 *
 */
package org.kalypso.google.earth.export.convert;

import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.google.earth.export.utils.GoogleEarthUtils;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.google.earth.kml.BoundaryType;
import com.google.earth.kml.LinearRingType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PolygonType;

/**
 * @author kuch
 */
public class ConverterSurface
{

  /**
   * @param factory
   * @param gmo
   * @param style
   * @throws Exception
   */
  public static PolygonType convert( final ObjectFactory factory, final GM_Surface< ? > gmo ) throws Exception
  {
    /* handling of multigeometries not implemented at the moment */
    if( gmo.size() > 1 )
      throw new NotImplementedException();

    final GeoTransformer transformer = new GeoTransformer( GoogleEarthUtils.GOOGLE_EARTH_CS );

    for( int i = 0; i < gmo.size(); i++ )
    {
      final PolygonType polygoneType = factory.createPolygonType();

      final Object object = gmo.get( i );
      if( !(object instanceof GM_Polygon) )
        continue;

      final GM_Polygon polygon = (GM_Polygon) object;

      /* set outer boundary */
      final BoundaryType outerBoundary = factory.createBoundaryType();
      final LinearRingType outerLinearRing = factory.createLinearRingType();
      final GM_Position[] exteriorRing = polygon.getExteriorRing();

      final List<String> outerCoord = outerLinearRing.getCoordinates();

      for( final GM_Position position : exteriorRing )
      {
        final GM_Point point = GeometryFactory.createGM_Point( position, gmo.getCoordinateSystem() );
        final GM_Point kmlPoint = (GM_Point) transformer.transform( point );

        outerCoord.add( String.format( Locale.ENGLISH, "%f,%f", kmlPoint.getX(), kmlPoint.getY() ) );
      }

      outerBoundary.setLinearRing( outerLinearRing );
      polygoneType.setOuterBoundaryIs( outerBoundary );

      // get inner boundaries
      final List<BoundaryType> innerBoundaries = polygoneType.getInnerBoundaryIs();

      final GM_Position[][] interiorRings = polygon.getInteriorRings();
      for( final GM_Position[] innerRing : interiorRings )
      {
        final BoundaryType innerBoundary = factory.createBoundaryType();
        final LinearRingType innerLinearRing = factory.createLinearRingType();

        final List<String> innerCoords = innerLinearRing.getCoordinates();

        for( final GM_Position position : innerRing )
        {
          final GM_Point point = GeometryFactory.createGM_Point( position, gmo.getCoordinateSystem() );
          final GM_Point kmlPoint = (GM_Point) transformer.transform( point );

          innerCoords.add( String.format( Locale.ENGLISH, "%f,%f", kmlPoint.getX(), kmlPoint.getY() ) );
        }

        innerBoundary.setLinearRing( innerLinearRing );
        innerBoundaries.add( innerBoundary );
      }

      return polygoneType;
    }

    throw new NotImplementedException();
  }
}
