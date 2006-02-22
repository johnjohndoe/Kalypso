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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkFeatureProperty;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author belger
 */
public class ObservationLinkHandler implements IMarshallingTypeHandler
{
  public static final Class CLASS_NAME = TimeseriesLinkType.class;

  public static final String NAMESPACE = "obslink.zml.kalypso.org";

  public static final QName[] TYPE_NAME = new QName[] { new QName( NAMESPACE, ClassUtilities.getOnlyClassName( TimeseriesLinkFeatureProperty.class ) ) };

  private final static ObjectFactory m_factory = new ObjectFactory();

  private final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public Class getValueClass( )
  {
    return CLASS_NAME;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public QName[] getTypeName( )
  {
    return TYPE_NAME;
  }

  private String getElementName( )
  {
    return ClassUtilities.getOnlyClassName( TimeseriesLinkType.class );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( final Object object, final Node node, URL context ) throws TypeRegistryException
  {
    try
    {
      final Marshaller marshaller = JC.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      marshaller.marshal( object, node );
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
      final Element element = (Element) node;
//      String elementTXT = XMLHelper.toString(element);
      final NodeList childNodes = (element).getElementsByTagNameNS( NAMESPACE, "TimeseriesLink");
//      String childNodesTXT = XMLHelper.toString(childNodes);
      for( int i = 0; i < childNodes.getLength(); i++ )
      {
        final Node child = childNodes.item( i );
//        String childTXT = XMLHelper.toString(child);

        // child namespace may be null
        if( NAMESPACE.equals( child.getNamespaceURI() ) && "TimeseriesLink".equals( child.getLocalName() ) )
        {
          final JAXBElement valueElement = (JAXBElement) JC.createUnmarshaller().unmarshal( child );
          return valueElement.getValue();
        }
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
  public String getShortname( )
  {
    return "Zeitreihen Verknüpfung";
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone )
  {
    final TimeseriesLinkType link = (TimeseriesLinkType) objectToClone;
    final TimeseriesLinkType clone = m_factory.createTimeseriesLinkType();
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
    return clone;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    final org.kalypso.zml.obslink.ObjectFactory factory = new org.kalypso.zml.obslink.ObjectFactory();
    final TimeseriesLinkType link = factory.createTimeseriesLinkType();
    link.setHref( text );
    return link;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }

}