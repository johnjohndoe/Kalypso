package org.kalypso.ogc.gml;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class GMLHelper
{
  public static void setCrs( Feature fe, CS_CoordinateSystem srcCS)
  {
    final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof Feature )
        setCrs( (Feature)prop, srcCS);
      else if( prop != null && prop instanceof GM_Object )
      {
        ( (GM_Object_Impl)prop ).setCoordinateSystem( srcCS );
      }
    }
  }
  
  /**
   * ueperprueft die koordinatensysteme der geometrien,
   * fall null, dann wird das defaultCoordinatessystem angenommen (gesetzt).
   */
  public static void checkCrs( Feature fe, CS_CoordinateSystem defaultCS)
  {
    final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof Feature )
        checkCrs( (Feature)prop, defaultCS);
      else if( prop != null && prop instanceof GM_Object )
      {
        GM_Object_Impl gmlProp= (GM_Object_Impl)prop ;
        if(gmlProp.getCoordinateSystem()==null)
          gmlProp.setCoordinateSystem(defaultCS );
      }
    }
  }
  
  /** Returns the {@link FeatureType} with the given name within a {@link GMLWorkspace}*/
  public static FeatureType getFeatureType( final GMLWorkspace workspace, final String name )
  {
    final FeatureType[] featureTypes = workspace.getFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      final FeatureType type = featureTypes[i];
      if( type.getName().equals( name ) )
        return type;
    }
    
    return null;
  }

}
