package org.kalypso.model.wspm.tuhh.ui.chart;

import java.net.URL;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.openali.odysseus.chart.framework.logging.impl.Logger;
import de.openali.odysseus.chart.framework.model.data.IDataContainer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.ComparableDataRange;

public class LengthSectionBridgeDataContainer<T_domain, T_target> implements IDataContainer<T_domain, T_target>
{
  final private String m_observationId;

  final private String m_href;

  final private URL m_context;

  private TupleResult m_result;

  private boolean m_isOpen = false;

  public LengthSectionBridgeDataContainer( final URL context, final String href, final String observationId )
  {
    m_context = context;
    m_href = href;
    m_observationId = observationId;

  }

  public LengthSectionBridgeDataContainer( final TupleResult result )
  {
    // Die Observation ist schon vorhanden, also kann das Layer gleich als open ausgegeben werden
    m_isOpen = true;
    m_context = null;
    m_observationId = null;
    m_href = null;

    m_result = result;

  }

  public void open( )
  {
    if( !m_isOpen )
    {
      try
      {
        final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( new URL( m_context, m_href ), null );
        final Feature feature = workspace.getFeature( m_observationId );
        if( feature != null )
        {
          Logger.logInfo( Logger.TOPIC_LOG_GENERAL, "Found feature: " + feature.getId() );
        }
        final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( feature );
        m_result = observation == null ? null : observation.getResult();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    m_isOpen = true;
  }

  public IDataRange<T_domain> getDomainRange( )
  {
    return new ComparableDataRange<T_domain>( getDomainValues() );
  }

  public IDataRange<T_target> getTargetRange( )
  {
    return new ComparableDataRange<T_target>( getTargetValues() );
  }

  public T_domain[] getDomainValues( )
  {
    return (T_domain[]) getValues( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );
  }
  public T_target[] getBridgeValues( )
  {
    return (T_target[]) getValues( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
  }
  private Object[] getValues( final String compName )
  {
    open();
    if( !m_isOpen )
      return new Object[] {};
    final int iComp = m_result.indexOfComponent( compName );
    if( iComp < 0 )
      return new Object[] {};
    final Object[] objArr = new Object[m_result.size()];
    int index = 0;
    for( final IRecord record : m_result )
    {
      final Object objVal = record.getValue( iComp );
      if( objVal instanceof XMLGregorianCalendar )
        objArr[index++] = ((XMLGregorianCalendar) objVal).toGregorianCalendar();
      else
        objArr[index++] = objVal;
    }
    return objArr;
  }

  public T_target[] getTargetValues( )
  {
    return (T_target[]) getValues( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK  );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.data.IDataContainer#close()
   */
  @Override
  public void close( )
  {
    m_result = null;
    m_isOpen = false;

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.data.IDataContainer#isOpen()
   */
  @Override
  public boolean isOpen( )
  {
    return m_isOpen;
  }
}
