package org.kalypso.ogc.sensor.deegree;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistryException;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.ObservationLink;
import org.w3._1999.xlink.XlinkBase;
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
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return ObservationLink.class.getName();
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return getNamespaceUri() + ":" + getElementName();
  }
  
  private String getElementName()
  {
    final String className = getClassName();
    final int dotIndex = className.lastIndexOf( '.' );
    return className.substring( dotIndex + 1 );
  }

  private String getNamespaceUri()
  {
    return "obslink.zml.kalypso.org";
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#marshall(java.lang.Object,
   *      org.w3c.dom.Node)
   */
  public void marshall( final Object object, final Node node ) throws TypeRegistryException
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
   * @see org.deegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node)
   */
  public Object unmarshall( final Node node ) throws TypeRegistryException
  {
    try
    {
      final NodeList childNodes = node.getChildNodes();
      for( int i = 0; i < childNodes.getLength(); i++ )
      {
        final Node child = childNodes.item( i );

        // child namespace may be null
        if( getNamespaceUri( ).equals( child.getNamespaceURI() ) && getElementName().equals( child.getLocalName() ) )
          return m_unmarshaller.unmarshal( child );    
      }
      
      throw new TypeRegistryException( "Could not parse Node" );
    }
    catch( final JAXBException e )
    {
      throw new TypeRegistryException( e );
    }
  }

  public static void main( final String[] args ) throws JAXBException
  {
    new ObservationLinkHandler().test();
  }

  private void test() throws JAXBException
  {
    final ObservationLink link = m_factory.createObservationLink();

    final XlinkBase xlinkBase = new org.w3._1999.xlink.ObjectFactory().createXlinkBase();
    xlinkBase.setActuate( "onDemand" );
    xlinkBase.setHref( "path=blubb" );
    xlinkBase.setType( "simple" );

    link.setObservation( xlinkBase );

    m_marshaller.marshal( link, System.out );
  }

}