package org.kalypso.ogc.sensor.beans;

import java.util.Map;

import org.kalypso.repository.beans.ItemBean;

/**
 * An Observation Bean. Specialisation of <code>ItemBean</code>. Can deliver the MetadataList
 * for the IObservation it represents.
 * 
 * @author schlienger
 */
public class ObservationBean extends ItemBean
{
  private Map m_metadataList;

  public ObservationBean()
  {
    this( 0, "", null );
  }
  
  public ObservationBean( final int id, final String name, final Map metadata )
  {
    super( id, name );
    
    m_metadataList = metadata;
  }
  
  public Map getMetadataList()
  {
    return m_metadataList;
  }
  
  public void setMetadataList( final Map md )
  {
    m_metadataList = md;
  }
}