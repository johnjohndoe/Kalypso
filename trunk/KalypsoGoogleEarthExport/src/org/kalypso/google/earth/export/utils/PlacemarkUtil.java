package org.kalypso.google.earth.export.utils;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.google.earth.export.interfaces.IGoogleEarthAdapter;
import org.kalypso.google.earth.export.interfaces.IPlacemark;

import com.google.earth.kml.FeatureType;
import com.google.earth.kml.FolderType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PlacemarkType;
import com.google.earth.kml.PointType;

public class PlacemarkUtil
{

  public static void addAdditional( final FolderType base, final IGoogleEarthAdapter[] provider, final ObjectFactory googleEarthFactory )
  {
    /* add additional place marks and clean up providers */
    final List<IPlacemark> placemarks = new ArrayList<IPlacemark>();
    for( final IGoogleEarthAdapter adapter : provider )
    {
      final IPlacemark[] placemarkers = adapter.getAdditionalPlacemarkers();
      for( final IPlacemark placemark : placemarkers )
      {
        placemarks.add( placemark );
      }

      adapter.cleanUp();
    }

    // add additional layer
    final FolderType folderType = googleEarthFactory.createFolderType();
    folderType.setName( "Interesting Places" );

    final List<JAXBElement< ? extends FeatureType>> myFeatures = folderType.getFeature();

    for( final IPlacemark placemark : placemarks )
    {
      try
      {
        final PlacemarkType placemarkType = googleEarthFactory.createPlacemarkType();
        placemarkType.setName( placemark.getName() );
        placemarkType.setDescription( placemark.getDescription() );

        final PointType point = googleEarthFactory.createPointType();
        final List<String> coordinates = point.getCoordinates();
        coordinates.add( placemark.getX( GoogleEarthUtils.GOOGLE_EARTH_CS ) + "," + placemark.getY( GoogleEarthUtils.GOOGLE_EARTH_CS ) );
        placemarkType.setGeometry( googleEarthFactory.createPoint( point ) );

        myFeatures.add( googleEarthFactory.createPlacemark( placemarkType ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

    }

    // add to base
    final List<JAXBElement< ? extends FeatureType>> features = base.getFeature();
    features.add( 0, googleEarthFactory.createFolder( folderType ) );
  }
}
