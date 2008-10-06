package org.kalypsodeegree_impl.model.feature;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.GM_Object_Impl;

/**
 * @author vdoemming
 */
public class GMLUtilities
{
  public static void setCrs( Feature fe, String srcCS )
  {
    final IPropertyType ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getQName() );
      if( prop != null && prop instanceof Feature )
        setCrs( (Feature) prop, srcCS );
      else if( prop != null && prop instanceof GM_Object )
      {
        ((GM_Object_Impl) prop).setCoordinateSystem( srcCS );
      }
    }
  }

  /**
   * ueperprueft die koordinatensysteme der geometrien, fall null, dann wird das defaultCoordinatessystem angenommen
   * (gesetzt).
   */
  public static void checkCrs( Feature fe, String defaultCS )
  {
    final IPropertyType ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getQName() );
      if( prop != null && prop instanceof Feature )
        checkCrs( (Feature) prop, defaultCS );
      else if( prop != null && prop instanceof GM_Object )
      {
        GM_Object_Impl gmlProp = (GM_Object_Impl) prop;
        if( gmlProp.getCoordinateSystem() == null )
          gmlProp.setCoordinateSystem( defaultCS );
      }
    }
  }

}