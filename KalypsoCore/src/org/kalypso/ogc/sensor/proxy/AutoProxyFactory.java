package org.kalypso.ogc.sensor.proxy;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQObservationFilter;

/**
 * AutoProxyFactory: this class can create proxy of an IObservation based on its
 * metadata.
 * 
 * @author schlienger
 */
public class AutoProxyFactory implements IProxyFactory
{
  private static AutoProxyFactory m_instance = null;

  private AutoProxyFactory( )
  {
    // do not instanciate
  }

  /**
   * Return a proxy IObservation that may be a proxy of the original observation
   * if sufficient information is found to create such a proxy.
   * <p>
   * For instance, some observations have WQ-Information in their metadata that
   * allows one to create a WQ-Observation from it.
   * 
   * @param obs
   * @return either a proxy observation or the original observation
   * @throws SensorException
   */
  public IObservation proxyObservation( final IObservation obs ) throws SensorException
  {
    // currently only the wechmann wq-filter as proxy for the observation
    IObservation proxy = proxyForWechmannWQ( obs );

    return proxy;
  }

  /**
   * Checks the metadata of the given observation for the presence of some
   * Wechmann WQ-Parameter. In the positive, it creates a WQ-Filter from this
   * information.
   * 
   * @param obs
   * @return either a WQObservationFilter or the original obs
   * @throws SensorException
   */
  private static IObservation proxyForWechmannWQ( final IObservation obs ) throws SensorException
  {
    final MetadataList mdl = obs.getMetadataList();

    final String wq = mdl.getProperty( TimeserieConstants.MD_WQ, "" );

    if( wq.length() > 0 )
    {
      final WQObservationFilter wqf = new WQObservationFilter();
      
      String type = "";
      final IAxis[] axes = obs.getAxisList();
      for( int i = 0; i < axes.length; i++ )
      {
        if( axes[i].getType().equals( TimeserieConstants.TYPE_RUNOFF) )
        {
          type = TimeserieConstants.TYPE_RUNOFF;
          break;
        }
        else if( axes[i].getType().equals( TimeserieConstants.TYPE_WATERLEVEL) )
        {
          type = TimeserieConstants.TYPE_WATERLEVEL;
          break;
        }
      }
      
      // directly return original observation if the type could not be found
      // from the axes
      if( type.length() == 0 )
        return obs;
      
      // now that we have wq-params and that we know the type of the
      // axis, let's say the filter can be created
      wqf.initFilter( type, obs );

      return wqf;
    }

    return obs;
  }

  /**
   * @return default instance of this factory class
   */
  public static AutoProxyFactory getInstance( )
  {
    if( m_instance == null )
      m_instance = new AutoProxyFactory();
    
    return m_instance;
  }
}