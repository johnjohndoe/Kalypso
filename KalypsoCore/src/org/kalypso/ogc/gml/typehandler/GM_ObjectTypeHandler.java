/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.typehandler;

import java.io.StringReader;
import java.net.URL;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.kalypsodeegree_impl.gml.GMLFactory;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.model.geometry.GMLAdapter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class GM_ObjectTypeHandler implements IMarshallingTypeHandler
{
  private final Class m_clazz;

  private final String m_typeName;

  private static final String PSEUDEGMLHEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      + "<GMLHelper xmlns=\"http://www.opengis.net/gml\" xmlns:gml=\"http://www.opengis.net/gml\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">";

  private static final String PSEUDOGMLFOOTER = "</GMLHelper>";

  private final String m_shortName;

  public GM_ObjectTypeHandler( final String schemaTypeName, final Class clazz )
  {
    m_typeName = schemaTypeName;
    m_clazz = clazz;
    m_shortName = m_clazz.getName().replaceAll( ".+\\.", "" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( Object value, Node node, URL context ) throws TypeRegistryException
  {
    if( value == null )
      return;
    final StringBuffer xml = new StringBuffer();
    try
    {
      xml.append( PSEUDEGMLHEADER );
      xml.append( GMLAdapter.export( (GM_Object)value ) );
      xml.append( PSEUDOGMLFOOTER );

      final StringReader reader = new StringReader( xml.toString() );
      
//      final ReaderInputStream stream = new ReaderInputStream( reader );
      final InputSource source = new InputSource(reader);
      final Document dom = XMLHelper.getAsDOM( source, true );
      final NodeList childs = dom.getElementsByTagNameNS( XMLHelper.GMLSCHEMA_NS, "GMLHelper" );
      final Node gmlHelperNode = childs.item( 0 );
      final NodeList childNodes = gmlHelperNode.getChildNodes();
      final Node newNode = childNodes.item( 0 );
      final Document ownerDocument = node.getOwnerDocument();
      final Node importedNode = ownerDocument.importNode( newNode, true );
      node.appendChild( importedNode );
    }
    catch( Exception e )
    {
      throw new TypeRegistryException( e );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver ) throws TypeRegistryException
  {
    if( node == null )
      return null;
    final ElementList childElements = XMLTools.getChildElements( node );
    if( childElements.getLength() < 1 )
      return null;
    final Element element = childElements.item( 0 );

    if( element != null )
      try
      {
        GMLGeometry gml = GMLFactory.createGMLGeometry( element );
        GM_Object object = GMLAdapter.wrap( gml );
        //        if( !m_clazz.isInstance( object ) )
        //        {
        //          System.out.println( "hier ist der falsche type angegeben biem typehandler" );
        //          System.out.println( "registriert: " + m_clazz.getName() );
        //          System.out.println( "wert ist " + object.getClass().getName() );
        //        }
        return object;
      }
      catch( GM_Exception e )
      {
        throw new TypeRegistryException( e );
      }
    return null;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname()
  {
    return m_shortName;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( final Object objectToClone )
  {
    final GM_Object geom = (GM_Object)objectToClone;
    GMLGeometry gml = null;

    try
    {
      gml = GMLFactory.createGMLGeometry( null, geom );
      return GMLAdapter.wrap( gml );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return null;

  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return m_clazz.getName();
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return XMLHelper.GMLSCHEMA_NS + ":" + m_typeName;
  }
  
  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }
}
