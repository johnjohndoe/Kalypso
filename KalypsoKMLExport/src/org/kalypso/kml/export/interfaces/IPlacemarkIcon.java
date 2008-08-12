package org.kalypso.kml.export.interfaces;

import java.net.URL;

public interface IPlacemarkIcon
{

  String getName( );

  URL getIcon( );

  Float getIconViewBoundScale( );

  Double getNorth( );

  Double getSouth( );

  Double getWest( );

  Double getEast( );

}
