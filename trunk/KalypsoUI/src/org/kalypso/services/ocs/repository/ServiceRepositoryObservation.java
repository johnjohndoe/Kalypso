package org.kalypso.services.ocs.repository;

import java.net.URL;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.IServicesConstants;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.DateRangeBean;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.OCSDataBean;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * Act as a proxy to the observation which is delivered by the Kalypso Observation Service.
 * 
 * @author schlienger
 */
public class ServiceRepositoryObservation implements IObservation
{
  private final IObservationService m_srv;
  private final ObservationBean m_ob;
  private IObservation m_obs = null;

  public ServiceRepositoryObservation( final IObservationService srv, final ObservationBean ob )
  {
    m_srv = srv;
    m_ob = ob;
  }
  
  /**
   * Lazy loading.
   * 
   * @throws SensorException
   */
  private IObservation getRemote( final IVariableArguments args ) throws SensorException
  {
    if( m_obs == null )
      m_obs = loadFromServer( args );
    
    return m_obs;        
  }
  
  /**
   * Uses the webservice to request the observation.
   * 
   * @throws SensorException
   */
  private IObservation loadFromServer( final IVariableArguments args ) throws SensorException 
  {
    final DateRangeBean drb;
    
    if( args instanceof DateRangeArgument )
      drb = ProxyFactory.createDateRangeBean( (DateRangeArgument)args );
    else
      drb = new DateRangeBean();
    
    try
    {
      final OCSDataBean db = m_srv.readData( m_ob, drb );

      final IObservation obs = ZmlFactory.parseXML( new URL( db.getLocation() ), db.getObsId() );

      m_srv.clearTempData( db );
      
      return obs;
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new SensorException( e );
    }
  }
  
  /**
   * Identifier is build using service protocol + id of observation on the server.
   * 
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
  {
    return IServicesConstants.URL_PROTOCOL_OBSERVATION_SERVICE + ":" + m_ob.getId();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName()
  {
    return m_ob.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    try
    {
      return getRemote( null ).isEditable();
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return false;
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget()
  {
    try
    {
      return getRemote( null ).getTarget();
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList()
  {
    final MetadataList ml = new MetadataList();
    ml.putAll( m_ob.getMetadataList() );
    
    return ml;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    try
    {
      return getRemote( null ).getAxisList();
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args ) throws SensorException
  {
    return getRemote( args ).getValues( args );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    getRemote( null ).setValues( values );
  }
}
