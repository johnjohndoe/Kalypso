/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.util.xml.xlink;

import org.w3._1999.xlinkext.SimpleLinkType;

/**
 * XLink wrapper over Jaxb XlinkBase.
 * 
 * @author schlienger
 */
public class JAXBXLink implements IXlink
{
  private final SimpleLinkType m_xlink;

  public JAXBXLink( final SimpleLinkType xlink )
  {
    if( xlink == null )
      throw new IllegalArgumentException( "xlink is null" );

    m_xlink = xlink;
  }

  /**
   * @see org.kalypso.util.xml.xlink.IXlink#getType()
   */
  public String getType()
  {
    return m_xlink.getType();
  }

  /**
   * @see org.kalypso.util.xml.xlink.IXlink#getActuate()
   */
  public String getActuate()
  {
    return m_xlink.getActuate();
  }

  /**
   * @see org.kalypso.util.xml.xlink.IXlink#getHRef()
   */
  public String getHRef()
  {
    return m_xlink.getHref();
  }
}