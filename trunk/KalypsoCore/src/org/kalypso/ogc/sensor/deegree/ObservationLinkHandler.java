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
package org.kalypso.ogc.sensor.deegree;

import java.net.URL;
import java.text.ParseException;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypso.zml.obslink.TimeseriesLinkFeatureProperty;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author belger
 */
public class ObservationLinkHandler implements IMarshallingTypeHandler
{
  public static final String CLASS_NAME = TimeseriesLinkType.class.getName();

  public static final String NAMESPACE = "obslink.zml.kalypso.org";

  public static final String TYPE_NAME = NAMESPACE + ":"
      + ClassUtilities.getOnlyClassName( TimeseriesLinkFeatureProperty.class );

  private final ObjectFactory m_factory = new ObjectFactory();

  private final Marshaller m_marshaller = m_factory.createMarshaller();

  private final Unmarshaller m_unmarshaller = m_factory.createUnmarshaller();

  public ObservationLinkHandler() throws JAXBException
  {
    // nur da, um die exception zu werfen
    m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public String getClassName()
  {
    return CLASS_NAME;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return TYPE_NAME;
  }

  private String getElementName()
  {
    return ClassUtilities.getOnlyClassName( TimeseriesLink.class );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( final Object object, final Node node, URL context ) throws TypeRegistryException
  {
    try
    {
      m_marshaller.marshal( object, node );
    }
    catch( JAXBException e )
    {
      throw new TypeRegistryException( e );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( final Node node, URL context, IUrlResolver urlResolver ) throws TypeRegistryException
  {
    try
    {
      final Element element = (Element)node;
      final NodeList childNodes = ( element ).getElementsByTagNameNS( NAMESPACE, getElementName() );

      for( int i = 0; i < childNodes.getLength(); i++ )
      {
        final Node child = childNodes.item( i );

        // child namespace may be null
        if( NAMESPACE.equals( child.getNamespaceURI() ) && getElementName().equals( child.getLocalName() ) )
          return m_unmarshaller.unmarshal( child );
      }

      return null;
    }
    catch( final JAXBException e )
    {
      throw new TypeRegistryException( e );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Zeitreihen Verkn�pfung";
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone )
  {
    TimeseriesLinkType link = (TimeseriesLinkType)objectToClone;
    TimeseriesLinkType clone = null;
    try
    {
      clone = m_factory.createTimeseriesLinkType();
      clone.setActuate( link.getActuate() );
      clone.setArcrole( link.getArcrole() );
      clone.setHref( link.getHref() );
      clone.setLinktype( link.getLinktype() );
      clone.setRole( link.getRole() );
      clone.setShow( link.getShow() );
      clone.setTimeaxis( link.getTimeaxis() );
      clone.setTitle( link.getTitle() );
      clone.setType( link.getType() );
      clone.setValueaxis( link.getValueaxis() );
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    return clone;
  }
  
  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text ) throws ParseException
  {
    final org.kalypso.zml.obslink.ObjectFactory factory = new org.kalypso.zml.obslink.ObjectFactory();
    try
    {
      final TimeseriesLink link = factory.createTimeseriesLink();
      link.setHref( text );
      return link;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      
      throw new ParseException( e.getLocalizedMessage(), 1 );
    }
  }

}