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
   * Creates a data object sutiable for a feature property out of string.
   * 
   * @return null, if the data-type is unknown
   * 
   * @throws NumberFormatException
   */
  public static final Object createFeaturePropertyFromStrings( final String type, final String format, final String[] input )
  {
    if( String.class.getName().equals( type ) )
      return input[0];

    if( Integer.class.getName().equals( type ) )
      return new Integer( input[0] );

    if( Long.class.getName().equals( type ) )
      return new Long( input[0] );

    if( Float.class.getName().equals( type ) )
      return new Float( input[0] );

    if( Double.class.getName().equals( type ) )
      return new Double( input[0].replace( ',', '.' ) );

    if( GeometryUtilities.getPointClass().getName().equals( type ) )
    {
      final String rwString = input[0].trim();
      final String hwString = input[1].trim();
      if( rwString == null || rwString.length() == 0 || hwString == null || hwString.length() == 0 )
        return null;

      final double rw = Double.parseDouble( rwString );
      final double hw = Double.parseDouble( hwString );

      final CS_CoordinateSystem crs = ConvenienceCSFactory.getInstance().getOGCCSByName( format );

      return GeometryFactory.createGM_Point( rw, hw, crs );
    }

    return null;
  }
}
