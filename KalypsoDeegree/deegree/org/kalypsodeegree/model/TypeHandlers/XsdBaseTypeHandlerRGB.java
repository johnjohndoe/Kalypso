/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree.model.TypeHandlers;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.XsdBaseTypeHandler;

/**
 * @author kuch
 */
public class XsdBaseTypeHandlerRGB extends XsdBaseTypeHandler<RGB>
{
  public XsdBaseTypeHandlerRGB( )
  {
    super( new QName( NS.COMMON, "color" ), RGB.class );
  }

  /**
   * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
   */
  @Override
  public RGB convertToJavaValue( final String xmlString )
  {
    if( xmlString.length() == 7 )
    {
      final RGB rgb = new RGB( 0, 0, 0 );

      final String red = xmlString.substring( 1, 3 );
      rgb.red = Integer.decode( "0x" + red );

      final String green = xmlString.substring( 3, 5 );
      rgb.green = Integer.decode( "0x" + green );

      final String blue = xmlString.substring( 5, 7 );
      rgb.blue = Integer.decode( "0x" + blue );

      return rgb;
    }
    else
    {
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
   */
  @Override
  public String convertToXMLString( final RGB value )
  {
    if( value == null )
    {
      return new String( "" );
    }

    String red = Integer.toHexString( value.red );
    if( red.length() < 2 )
    {
      red = "0" + red;
    }

    String green = Integer.toHexString( value.green );
    if( green.length() < 2 )
    {
      green = "0" + green;
    }

    String blue = Integer.toHexString( value.blue );
    if( blue.length() < 2 )
    {
      blue = "0" + blue;
    }

    return new String( "#" + red + green + blue );
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final RGB o1, final RGB o2 )
  {
    return ("" + o1).compareTo( "" + o2 );
  }
}
