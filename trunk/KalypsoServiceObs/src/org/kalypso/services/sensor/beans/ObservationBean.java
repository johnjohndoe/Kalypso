package org.kalypso.services.sensor.beans;

import org.kalypso.services.repository.beans.ItemBean;

/**
 * An Observation Bean. For classification only.
 * 
 * @author schlienger
 */
public class ObservationBean extends ItemBean
{
  public ObservationBean()
  {
    super();
  }
  
  public ObservationBean( final int id, final String name )
  {
    super( id, name );
  }
}