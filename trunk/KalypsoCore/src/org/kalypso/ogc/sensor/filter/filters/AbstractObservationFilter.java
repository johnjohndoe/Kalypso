package org.kalypso.ogc.sensor.filter.filters;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
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
  protected Object m_conf = null;

  /**
   * @see org.kalypso.ogc.sensor.filter.IObservationFilter#initFilter(java.lang.Object, org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final Object conf, final IObservation obs ) throws SensorException
  {
    m_conf = conf;
    m_obs = obs;
  }

  public boolean equals( Object obj )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.equals( obj );
  }

  public IAxis[] getAxisList( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.getAxisList();
  }

  public String getIdentifier( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.getIdentifier();
  }

  public MetadataList getMetadataList( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.getMetadataList();
  }

  public String getName( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.getName();
  }

  public IXlink getTarget( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.getTarget();
  }

  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.getValues( args );
  }

  public int hashCode( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.hashCode();
  }

  public boolean isEditable( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.isEditable();
  }

  public void setValues( ITuppleModel values ) throws SensorException
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    m_obs.setValues( values );
  }

  public String toString( )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    return m_obs.toString();
  }
  
  
  public void addListener( IObservationListener listener )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    m_obs.addListener( listener );
  }
  
  public void removeListener( IObservationListener listener )
  {
    if( m_obs == null )
      throw new IllegalStateException( "Internal observation is null" );
    
    m_obs.removeListener( listener );
  }
}