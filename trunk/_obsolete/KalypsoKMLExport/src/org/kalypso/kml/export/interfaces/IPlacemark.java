package org.kalypso.kml.export.interfaces;

public interface IPlacemark
{
  String getName( );

  String getDescription( );

  String getX( String targetCRS ) throws Exception;

  String getY( String targetCRS ) throws Exception;

}
