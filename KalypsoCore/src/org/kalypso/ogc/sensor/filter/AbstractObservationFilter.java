package org.kalypso.ogc.sensor.filter;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * AbstractObservationFilter
 * 
 * @author schlienger
 */
public abstract class AbstractObservationFilter implements IObservationFilter
{
  protected IObservation m_obs = null;
  protected String m_conf = "";

  /**
   * @see org.kalypso.ogc.sensor.filter.IObservationFilter#initFilter(java.lang.String, org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final String conf, final IObservation obs )
  {
    m_conf = conf;
    m_obs = obs;
  }

  public boolean equals( Object obj )
  {
    return m_obs.equals( obj );
  }

  public IAxis[] getAxisList( )
  {
    return m_obs.getAxisList();
  }

  public String getIdentifier( )
  {
    return m_obs.getIdentifier();
  }

  public MetadataList getMetadataList( )
  {
    return m_obs.getMetadataList();
  }

  public String getName( )
  {
    return m_obs.getName();
  }

  public IXlink getTarget( )
  {
    return m_obs.getTarget();
  }

  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    return m_obs.getValues( args );
  }

  public int hashCode( )
  {
    return m_obs.hashCode();
  }

  public boolean isEditable( )
  {
    return m_obs.isEditable();
  }

  public void setValues( ITuppleModel values ) throws SensorException
  {
    m_obs.setValues( values );
  }

  public String toString( )
  {
    return m_obs.toString();
  }
}