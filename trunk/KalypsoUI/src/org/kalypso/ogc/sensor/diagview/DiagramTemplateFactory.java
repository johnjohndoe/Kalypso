package org.kalypso.ogc.sensor.diagview;

import java.io.OutputStream;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.ObsdiagviewType.CurveType;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author schlienger
 */
public class DiagramTemplateFactory
{
  private final static ObjectFactory m_objectFactory = new ObjectFactory();
  
  private DiagramTemplateFactory()
  {
    // not to be instanciated
  }

  public static void addTimeseriesLink( final ObsdiagviewType tpl, final TimeseriesLinkType lnk, final String name, final String diagDateAxis, final String diagValueAxis ) throws JAXBException
  {
    final CurveType c = m_objectFactory.createObsdiagviewTypeCurveType();

    final List curves = tpl.getCurve();
    
    c.setId( String.valueOf( curves.size() + 1 ) );
    c.setName( name );

    c.setLinktype( lnk.getLinktype() );

    c.setType( lnk.getType() );
    c.setHref( lnk.getHref() );
    c.setActuate( lnk.getActuate() );
    
    final List mapping = c.getMapping();

    final TypeAxisMapping mpDate = m_objectFactory.createTypeAxisMapping();
    mpDate.setDiagramAxis( diagDateAxis );
    mpDate.setObservationAxis( lnk.getTimeaxis() );

    final TypeAxisMapping mpValue = m_objectFactory.createTypeAxisMapping();
    mpValue.setDiagramAxis( diagValueAxis );
    mpValue.setObservationAxis( lnk.getValueaxis() );
    
    mapping.add( mpDate );
    mapping.add( mpValue );
    
    curves.add( c );
  }
  
  public static void writeTemplate( final ObsdiagviewType tpl, final OutputStream out ) throws JAXBException
  {
    m_objectFactory.createMarshaller().marshal( tpl, out );
  }
}
