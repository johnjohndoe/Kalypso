package org.kalypso.ogc.sensor.deegree;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author belger
 */
public class ObservationLinkHandlerTest extends TestCase
{
  public void testMarshal() throws JAXBException
  {
    final ObjectFactory factory = new ObjectFactory();

    final Marshaller m_marshaller = factory.createMarshaller();
    m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

    final TimeseriesLinkType link = factory.createTimeseriesLinkType();

    link.setActuate( "onDemand" );
    link.setHref( "path=blubb" );
    link.setType( "simple" );

    m_marshaller.marshal( link, System.out );
  }
}
