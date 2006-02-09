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
import java.text.ParseException;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class XSDIntegerTypeHandler extends AbstractXSDSimpleTypeHandler
{

  public XSDIntegerTypeHandler( )
  {
    super( Integer.class, new QName[] { new QName( XMLHelper.XMLSCHEMA_NS, "integer" ) } );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( Object object, Node node, URL context ) throws TypeRegistryException
  {
    if( object == null )
      return;
    final Integer value = (Integer) object;
    final CDATASection section = node.getOwnerDocument().createCDATASection( value.toString() );
    node.appendChild( section );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver ) throws TypeRegistryException
  {
    final String result = node.getTextContent();
    if( result == null || result.length()<1)
      return null;
    return new Integer( result );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone ) throws CloneNotSupportedException
  {
    return (Integer) objectToClone;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( String text ) throws ParseException
  {
    if( text == null )
      return null;
    return new Integer( text );
  }

}
