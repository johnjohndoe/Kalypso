package org.kalypso.ogc.sensor.beans;

import org.kalypso.repository.beans.ItemBean;

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