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

package org.kalypso.utilbinding;

import java.awt.Color;

import javax.xml.bind.JAXBException;

import org.kalypso.util.awt.ObjectFactory;

/**
 * Utilities to help with the awt-binding-classes.
 *
 * @author belger
 */
public final class AwtBindingUtilities
{
  private static final ObjectFactory OF = new ObjectFactory();

  private AwtBindingUtilities()
  {
    // will not get instantiated
  }

  public static Color xmlColor2javaColor( final org.kalypso.util.awt.Color xmlColor )
  {
    return new Color( xmlColor.getRed(), xmlColor.getGreen(), xmlColor.getBlue(), xmlColor.getAlpha() );
  }

  public static org.kalypso.util.awt.Color javaColor2xmlColor( final Color javaColor )
  {
    try
    {
      final org.kalypso.util.awt.Color color = OF.createColor();
      color.setRed( (short)javaColor.getRed() );
      color.setGreen( (short)javaColor.getGreen() );
      color.setBlue( (short)javaColor.getBlue() );
      color.setAlpha( (short)javaColor.getAlpha() );
      return color;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      
      // should never happen
    }
      return null;
  }
  
}
