package org.kalypso.google.earth.export.interfaces;

public interface IPlacemark
{
  String getName( );

  String getDescription( );

  String getX( String targetCRS );

  String getY( String targetCRS );

}
