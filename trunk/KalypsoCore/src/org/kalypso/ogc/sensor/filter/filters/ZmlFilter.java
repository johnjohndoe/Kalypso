package org.kalypso.ogc.sensor.filter.filters;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * ZmlFilter
 * 
 * @author schlienger
 */
public final class ZmlFilter extends AbstractObservationFilter
{
  /** TRICKY: allows us to override the default behaviour of URL resolving
   * by directly looking for the href as an identifier within the list
   * of repositories
   */
  private static List REPS = null;

  
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final Object conf, final IObservation obs )
      throws SensorException
  {
    super.initFilter( conf, obs );

    // conf is the href string
    final String href = conf.toString();

    // if the href is empty, simply ignore and let the given obs replace this
    // filter
    if( href.length() != 0 )
    {
      // TRICKY: clients can set the REPS field. This will be used first to make
      // a lookup for the href as an identifier of an Observation.
      // The observation referred by the href can be localized within another
      // repository on the server side for instance. This trick allow us
      // to directly fetch the observation from the repository item.
      if( REPS != null && REPS.size() > 0 )
      {
        // only take id part since href can contain additional query stuff
        final String id = ZmlURL.getIdentifierPart( href );

        final Iterator it = REPS.iterator();
        
        while( it.hasNext() )
        {
          final IRepository rep = (IRepository) it.next();
          
          try
          {
            final IRepositoryItem item = rep.findItem( id );
            
            if( item != null )
            {
              m_obs = (IObservation) item.getAdapter( IObservation.class );
              
              return;
            }
          }
          catch( RepositoryException ignored )
          {
            // ignored
          }
        }
      }

      // no use the standard URL resolving facility
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
  
  /**
   * 
   * @param repositories
   */
  public static void configureFor( final List repositories )
  {
    REPS = repositories;
  }
}