package org.kalypso.ogc.sensor.deegree;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistryException;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.ObservationLink;
import org.w3c.dom.Node;

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
    return "ObservationLink";
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#marshall(java.lang.Object, org.w3c.dom.Node)
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
      return m_unmarshaller.unmarshal( node );
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
    link.setActuate( "onDemand" );
    link.setHref( "path=blubb" );
    link.setType( "simple" );
    
    m_marshaller.marshal( link, System.out );
  }
  

}
