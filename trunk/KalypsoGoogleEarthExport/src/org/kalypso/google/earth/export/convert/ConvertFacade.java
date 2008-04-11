/**
 *
 */
package org.kalypso.google.earth.export.convert;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.google.earth.export.geometry.GeoUtils;
import org.kalypso.google.earth.export.geometry.GeoUtils.GEOMETRY_TYPE;
import org.kalypso.google.earth.export.interfaces.IGoogleEarthAdapter;
import org.kalypso.google.earth.export.interfaces.IGroundOverlay;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;

import com.google.earth.kml.FeatureType;
import com.google.earth.kml.GroundOverlayType;
import com.google.earth.kml.LatLonBoxType;
import com.google.earth.kml.LinkType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PlacemarkType;
import com.google.earth.kml.StyleType;

/**
 * @author kuch
 */
public class ConvertFacade
{
  public static FeatureType[] convert( final IGoogleEarthAdapter[] providers, final ObjectFactory factory, final GM_Object[] geometries, final StyleType style, final Feature feature ) throws Exception
  {
    final List<FeatureType> featureTypes = new ArrayList<FeatureType>();

    for( final GM_Object gmo : geometries )
    {

      final GEOMETRY_TYPE gt = GeoUtils.getGeoType( gmo );
      if( GEOMETRY_TYPE.eMultiCurve.equals( gt ) )
      {
        final PlacemarkType placemark = factory.createPlacemarkType();
        placemark.setName( feature.getId() );

        placemark.setGeometry( factory.createMultiGeometry( ConverterMultiCurve.convert( factory, (GM_MultiCurve) gmo ) ) );
        if( style != null )
          placemark.setStyleUrl( "#" + style.getId() ); //$NON-NLS-1$
        featureTypes.add( placemark );
      }
      else if( GEOMETRY_TYPE.eCurve.equals( gt ) )
      {
        final PlacemarkType placemark = factory.createPlacemarkType();
        placemark.setName( feature.getId() );

        placemark.setGeometry( factory.createLineString( ConverterCurve.convert( factory, (GM_Curve) gmo ) ) );
        if( style != null )
          placemark.setStyleUrl( "#" + style.getId() ); //$NON-NLS-1$
        featureTypes.add( placemark );
      }
      else if( GEOMETRY_TYPE.eMultiSurface.equals( gt ) )
      {
        final PlacemarkType placemark = factory.createPlacemarkType();
        placemark.setName( feature.getId() );

        placemark.setGeometry( factory.createMultiGeometry( ConverterMultiSurface.convert( factory, (GM_MultiSurface) gmo ) ) );
        if( style != null )
          placemark.setStyleUrl( "#" + style.getId() ); //$NON-NLS-1$
        featureTypes.add( placemark );
      }
      else if( GEOMETRY_TYPE.eSurface.equals( gt ) )
      {
        final PlacemarkType placemark = factory.createPlacemarkType();
        placemark.setName( feature.getId() );

        placemark.setGeometry( factory.createPolygon( ConverterSurface.convert( factory, (GM_Surface< ? >) gmo ) ) );
        if( style != null )
          placemark.setStyleUrl( "#" + style.getId() ); //$NON-NLS-1$
        featureTypes.add( placemark );
      }
      else if( GEOMETRY_TYPE.ePoint.equals( gt ) )
      {
        IGroundOverlay myOverlay = null;

        for( final IGoogleEarthAdapter adapter : providers )
        {
          myOverlay = adapter.getGroundOverlay( feature );
          if( myOverlay != null )
            break;
        }

        if( myOverlay != null )
        {
          final GroundOverlayType overlay = factory.createGroundOverlayType();
          overlay.setName( myOverlay.getName() );

          /* icon */
          final URL urlIcon = myOverlay.getIcon();

          // TODO copy icon
          final LinkType lnkIcon = factory.createLinkType();
// lnkIcon.setHref( myOverlay.getIconHref() );
          lnkIcon.setViewBoundScale( myOverlay.getIconViewBoundScale() );
          overlay.setIcon( lnkIcon );

          /* latlonbox */
          final LatLonBoxType latLon = factory.createLatLonBoxType();
          latLon.setNorth( myOverlay.getNorth() );
          latLon.setSouth( myOverlay.getSouth() );
          latLon.setWest( myOverlay.getWest() );
          latLon.setEast( myOverlay.getEast() );
          overlay.setLatLonBox( latLon );

          featureTypes.add( overlay );
        }
        else
        {
          final PlacemarkType placemark = factory.createPlacemarkType();
          placemark.setName( feature.getId() );

          placemark.setGeometry( factory.createPoint( ConverterPoint.convert( factory, (GM_Point) gmo ) ) );
          if( style != null )
            placemark.setStyleUrl( "#" + style.getId() ); //$NON-NLS-1$

          featureTypes.add( placemark );
        }

      }
      else
        throw new NotImplementedException();

    }

    return featureTypes.toArray( new FeatureType[] {} );
  }
}
