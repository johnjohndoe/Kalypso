/**
 *
 */
package org.kalypso.google.earth.export.convert;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.google.earth.export.geometry.GeoUtils;
import org.kalypso.google.earth.export.geometry.GeoUtils.GEOMETRY_TYPE;
import org.kalypso.google.earth.export.interfaces.IGoogleEarthAdapter;
import org.kalypso.google.earth.export.interfaces.IPlacemarker;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;

import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PlacemarkType;
import com.google.earth.kml.StyleType;

/**
 * @author kuch
 */
public class ConvertFacade
{
  public static PlacemarkType[] convert( final IGoogleEarthAdapter[] providers, final ObjectFactory factory, final GM_Object[] geometries, final StyleType style, final Feature feature ) throws Exception
  {
    final List<PlacemarkType> placemarks = new ArrayList<PlacemarkType>();

    for( final GM_Object gmo : geometries )
    {
      final PlacemarkType placemark = factory.createPlacemarkType();
      placemark.setName( feature.getId() );

      final GEOMETRY_TYPE gt = GeoUtils.getGeoType( gmo );
      if( GEOMETRY_TYPE.eMultiCurve.equals( gt ) )
      {
        placemark.setGeometry( factory.createMultiGeometry( ConverterMultiCurve.convert( factory, (GM_MultiCurve) gmo ) ) );
      }
      else if( GEOMETRY_TYPE.eCurve.equals( gt ) )
      {

        placemark.setGeometry( factory.createLineString( ConverterCurve.convert( factory, (GM_Curve) gmo ) ) );
      }
      else if( GEOMETRY_TYPE.eMultiSurface.equals( gt ) )
      {

        placemark.setGeometry( factory.createMultiGeometry( ConverterMultiSurface.convert( factory, (GM_MultiSurface) gmo ) ) );
      }
      else if( GEOMETRY_TYPE.eSurface.equals( gt ) )
      {
        placemark.setGeometry( factory.createPolygon( ConverterSurface.convert( factory, (GM_Surface< ? >) gmo ) ) );
      }
      else if( GEOMETRY_TYPE.ePoint.equals( gt ) )
      {
        IPlacemarker myMarker = null;
        for( final IGoogleEarthAdapter adapter : providers )
        {
          myMarker = adapter.getPlacemarker( feature );
          if( myMarker != null )
            break;
        }

        placemark.setGeometry( factory.createPoint( ConverterPoint.convert( factory, (GM_Point) gmo ) ) );
      }
      else
        throw new NotImplementedException();

      placemark.setStyleUrl( "#" + style.getId() ); //$NON-NLS-1$
      placemarks.add( placemark );
    }

    return placemarks.toArray( new PlacemarkType[] {} );
  }

}
