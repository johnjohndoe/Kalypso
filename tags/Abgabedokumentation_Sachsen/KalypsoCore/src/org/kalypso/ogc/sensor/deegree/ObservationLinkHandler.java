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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypsodeegree_impl.extension.ITypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author belger
 */
public class ObservationLinkHandler implements ITypeHandler
{
  private final ObjectFactory m_factory = new ObjectFactory();

  private final Marshaller m_marshaller = m_factory.createMarshaller();

  private final Unmarshaller m_unmarshaller = m_factory.createUnmarshaller();

  public ObservationLinkHandler() throws JAXBException
  {
  // nur da, um die exception zu werfen
    m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return TimeseriesLinkType.class.getName();
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return getNamespaceUri() + ":" + ClassUtilities.getOnlyClassName( TimeseriesLinkType.class );
  }
  
  private String getElementName()
  {
    return ClassUtilities.getOnlyClassName( TimeseriesLink.class );
  }
  
  private String getNamespaceUri()
  {
    return "obslink.zml.kalypso.org";
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#marshall(java.lang.Object, org.w3c.dom.Node, java.net.URL)
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
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL)
   */
  public Object unmarshall( final Node node,URL context ) throws TypeRegistryException
  {
    try
    {
    	final NodeList childNodes=((Element)node).getElementsByTagNameNS(getNamespaceUri(),getElementName());
      for( int i = 0; i < childNodes.getLength(); i++ )
      {
        final Node child = childNodes.item( i );

        // child namespace may be null
        if( getNamespaceUri( ).equals( child.getNamespaceURI() ) && getElementName().equals( child.getLocalName() ) )
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
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Zeitreihen Verknüpfung";
  }

}