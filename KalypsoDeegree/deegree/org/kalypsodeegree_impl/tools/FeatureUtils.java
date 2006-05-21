/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypsodeegree_impl.tools;

import java.text.ParseException;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Helper code for features.
 * 
 * @author belger
 */
public class FeatureUtils
{
  /**
   * Creates a data object suitable for a feature property out of string.
   * 
   * @return null, if the data-type is unknown
   * @throws NumberFormatException
   */
  public static final Object createFeaturePropertyFromStrings( final IValuePropertyType type, final String format, final String[] input )
  {
    final IMarshallingTypeHandler typeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerFor( type );

    if( GeometryUtilities.getPointClass() == type.getValueClass() )
    {
      final String rwString = input[0].trim();
      final String hwString = input[1].trim();

      final String crsString;
      if( input.length > 2 )
        crsString = input[2];
      else
        crsString = format;

      final CS_CoordinateSystem crs = ConvenienceCSFactory.getInstance().getOGCCSByName( crsString );
      if( rwString == null || rwString.length() == 0 || hwString == null || hwString.length() == 0 )
        return GeometryFactory.createGM_Point( 0, 0, crs );

      final double rw = Double.parseDouble( rwString );
      final double hw = Double.parseDouble( hwString );

      return GeometryFactory.createGM_Point( rw, hw, crs );
    }

    if( typeHandler != null )
    {
      try
      {
        return typeHandler.parseType( input[0] );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
      }
    }

    return null;
  }

  public static void addChild( final Feature parentFE, final IRelationType rt, final Feature childFE )
  {
    if( rt.isList() )
    {
      final FeatureList list = (FeatureList) parentFE.getProperty( rt );
      list.add( childFE );
    }
    else
      parentFE.setProperty( rt, childFE );
  }

  public static void addChild( Feature parentFE, IRelationType rt, String featureID )
  {
    if( rt.isList() )
    {
      final FeatureList list = (FeatureList) parentFE.getProperty( rt );
      list.add( featureID );
    }
    else
      parentFE.setProperty( rt, featureID );
  }
}
