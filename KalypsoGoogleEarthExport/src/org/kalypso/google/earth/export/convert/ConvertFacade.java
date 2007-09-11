/**
 *
 */
package org.kalypso.google.earth.export.convert;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.google.earth.export.geometry.GeoUtils;
import org.kalypso.google.earth.export.geometry.GeoUtils.GEOMETRY_TYPE;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;

import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.PlacemarkType;
import com.google.earth.kml._2.StyleType;

/**
 * @author kuch
 */
public class ConvertFacade
{
  private static ObjectFactory m_factory = new ObjectFactory();

  /**
   * @param factory
   * @param features
   * @throws Exception
   */
  public static PlacemarkType[] convert( final ObjectFactory factory, final Feature[] features, final IFeatureGeometryFilter filter, final StyleType style ) throws Exception
  {
    final List<PlacemarkType> placemarks = new ArrayList<PlacemarkType>();

    for( final Feature feature : features )
    {
      final PlacemarkType[] converted = ConvertFacade.convert( factory, feature, filter, style );
      for( final PlacemarkType placemark : converted )
        placemarks.add( placemark );
    }

    return placemarks.toArray( new PlacemarkType[] {} );
  }

  /**
   * @param feature
   * @return
   * @throws Exception
   */
  public static PlacemarkType[] convert( final ObjectFactory factory, final Feature feature, final IFeatureGeometryFilter filter, final StyleType style ) throws Exception
  {
    final List<PlacemarkType> placemarks = new ArrayList<PlacemarkType>();

    /* geometry type of feature? */
    final GM_Object[] geometries = filter.getGeometries( feature );

    for( final GM_Object gmo : geometries )
    {
      final PlacemarkType placemark = factory.createPlacemarkType();
      placemark.setName( feature.getId() );

      final GEOMETRY_TYPE gt = GeoUtils.getGeoType( gmo );
      switch( gt )
      {
        case eMultiCurve:
          placemark.setGeometry( factory.createMultiGeometry( ConverterMultiCurve.convert( factory, (GM_MultiCurve) gmo, style ) ) );
          break;

        case eCurve:
          placemark.setGeometry( factory.createLineString( ConverterCurve.convert( factory, (GM_Curve) gmo, style ) ) );
          break;

        case eSurface:
          placemark.setGeometry( factory.createPolygon( ConverterSurface.convert( factory, (GM_Surface< ? >) gmo, style ) ) );
          break;

        case ePoint:
          placemark.setGeometry( factory.createPoint( ConverterPoint.convert( factory, (GM_Point) gmo, style ) ) );
          break;

        default:
          throw new NotImplementedException();
      }

      placemarks.add( placemark );
    }

    return placemarks.toArray( new PlacemarkType[] {} );
  }
}
