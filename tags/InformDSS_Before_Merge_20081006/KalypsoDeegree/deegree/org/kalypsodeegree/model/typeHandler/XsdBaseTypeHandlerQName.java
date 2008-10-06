/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypsodeegree.model.typeHandler;

import javax.xml.namespace.QName;


/**
 * @author kuch
 */
public class XsdBaseTypeHandlerQName extends XsdBaseTypeHandler<QName>
{
  public XsdBaseTypeHandlerQName( )
  {
    super( "QName", QName.class );
  }

  /**
   * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
   */
  @Override
  public QName convertToJavaValue( final String xmlString )
  {
    return QName.valueOf( xmlString );
  }

  /**
   * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(T)
   */
  @Override
  public String convertToXMLString( final QName value )
  {
    return value.toString();
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final QName o1, final QName o2 )
  {
    return ("" + o1).compareTo( "" + o2 );
  }
}
