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
package org.kalypsodeegree_impl.extension;

import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class XSDBooleanTypeHandler extends AbstractXSDSimpleTypeHandler
{

  public XSDBooleanTypeHandler( )
  {
    super( Boolean.class, new QName[] { new QName( XMLHelper.XMLSCHEMA_NS, "boolean" ) } );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( Object object, Node node, URL context )
  {
    if( object == null )
      return;
    final Boolean value = (Boolean) object;
    final CDATASection section = node.getOwnerDocument().createCDATASection( value.toString() );
    node.appendChild( section );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver )
  {
    final String result = node.getTextContent();
    if( result == null || result.length() < 1 )
      return null;
    if( "1".equals( result ) )
      return Boolean.TRUE;
    if( "0".equals( result ) )
      return Boolean.FALSE;
    return new Boolean( result );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone )
  {
    return (Boolean) objectToClone;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( String text )
  {
    if( text == null )
      return null;
    if( "1".equals( text ) )
      return Boolean.TRUE;
    if( "0".equals( text ) )
      return Boolean.FALSE;
    return new Boolean( text );
  }

}
