package org.deegree_impl.model.feature;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class GMLHelper
{
  public static void setCrs( Feature fe, CS_CoordinateSystem srcCS )
  {
    final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof Feature )
        setCrs( (Feature)prop, srcCS );
      else if( prop != null && prop instanceof GM_Object )
      {
        ( (GM_Object_Impl)prop ).setCoordinateSystem( srcCS );
      }
    }
  }

  /**
   * ueperprueft die koordinatensysteme der geometrien, fall null, dann wird das
   * defaultCoordinatessystem angenommen (gesetzt).
   */
  public static void checkCrs( Feature fe, CS_CoordinateSystem defaultCS )
  {
    final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof Feature )
        checkCrs( (Feature)prop, defaultCS );
      else if( prop != null && prop instanceof GM_Object )
      {
        GM_Object_Impl gmlProp = (GM_Object_Impl)prop;
        if( gmlProp.getCoordinateSystem() == null )
          gmlProp.setCoordinateSystem( defaultCS );
      }
    }
  }

  public static FeatureType[] getResolveSubstitutionGroup( final FeatureType ft, final FeatureType[] fts )
  {
    final List result = new ArrayList();
    final String substitutionGroup = ft.getNamespace() + ":" + ft.getName();
    if( substitutionGroup == null )
      return new FeatureType[0];
    
    for( int i = 0; i < fts.length; i++ )
    {
      final String substiGroup = fts[i].getSubstitutionGroup();
      if( substitutionGroup.equals( substiGroup ) )
        result.add( fts[i] );
    }
    return (FeatureType[])result.toArray( new FeatureType[result.size()] );
  }

}