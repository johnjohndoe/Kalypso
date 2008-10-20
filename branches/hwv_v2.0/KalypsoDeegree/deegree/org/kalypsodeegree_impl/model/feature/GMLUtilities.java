/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
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