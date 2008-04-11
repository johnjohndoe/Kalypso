package org.kalypso.google.earth.export.interfaces;

import java.net.URL;

public interface IGroundOverlay
{

  String getName( );

  URL getIcon( );

  Float getIconViewBoundScale( );

  Double getNorth( );

  Double getSouth( );

  Double getWest( );

  Double getEast( );

}
