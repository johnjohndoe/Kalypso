package org.kalypso.ogc.sensor.filter.filters;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

/**
 * ZmlFilter
 * 
 * @author schlienger
 */
public final class ZmlFilter extends AbstractObservationFilter
{
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final Object conf, final IObservation obs )
      throws SensorException
  {
    super.initFilter( conf, obs );

    final String href = conf.toString();

    // if the href is empty, simply ignore and let the given obs replace this
    // filter
    if( href.length() != 0 )
    {
      try
      {
        final IObservation observation = ZmlFactory.parseXML( new URL( href ),
            href );

        // override observation from abstract filter (super type)
        m_obs = observation;
      }
      catch( MalformedURLException e )
      {
        throw new SensorException( e );
      }
    }
  }
}