package org.kalypso.kml.export.utils;

import java.util.List;

import javax.xml.bind.JAXBElement;

import com.google.earth.kml.FeatureType;
import com.google.earth.kml.FolderType;
import com.google.earth.kml.GroundOverlayType;
import com.google.earth.kml.PlacemarkType;

public class FolderUtil
{

  public static void removeEmptyFolders( final FolderType folderType )
  {
    final List<JAXBElement< ? extends FeatureType>> features = folderType.getFeature();
    final Object[] myFeatures = features.toArray();

    for( final Object obj : myFeatures )
    {
      if( !(obj instanceof JAXBElement) )
      {
        continue;
      }
      final JAXBElement< ? extends FeatureType> element = (JAXBElement< ? extends FeatureType>) obj;

      final FeatureType featureType = element.getValue();
      if( featureType instanceof FolderType )
      {
        final FolderType myFolderType = (FolderType) featureType;
        if( isEmptyFolder( myFolderType ) )
          features.remove( obj );
      }
    }

  }

  private static boolean isEmptyFolder( final FolderType base )
  {
    final List<JAXBElement< ? extends FeatureType>> features = base.getFeature();

    boolean isEmpty = true;

    final Object[] myFeatures = features.toArray();
    for( final Object obj : myFeatures )
    {
      if( !(obj instanceof JAXBElement) )
      {
        continue;
      }
      final JAXBElement< ? extends FeatureType> element = (JAXBElement< ? extends FeatureType>) obj;

      final FeatureType featureType = element.getValue();
      if( featureType instanceof FolderType )
      {
        final FolderType myFolderType = (FolderType) featureType;
        final boolean empty = isEmptyFolder( myFolderType );
        if( empty == false )
          isEmpty = false;
        else
        {
          features.remove( element );
        }
      }
      else if( featureType instanceof PlacemarkType )
        return false;
      else if( featureType instanceof GroundOverlayType )
        return false;
    }

    return isEmpty;
  }

}