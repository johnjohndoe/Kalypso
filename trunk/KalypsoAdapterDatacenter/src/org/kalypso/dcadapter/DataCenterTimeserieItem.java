package org.kalypso.dcadapter;

import java.sql.SQLException;
import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

import com.bce.datacenter.db.timeseries.Timeserie;
import com.bce.datacenter.db.timeseries.TimeserieTupple;

/**
 * DataCenterTimeserieItem
 * 
 * @author marc
 */
public class DataCenterTimeserieItem implements IRepositoryItem, IObservation
{
  private final DataCenterRepository m_rep;
  private final DataCenterChannelItem m_parent;
  private final Timeserie m_ts;
  
  private MetadataList m_metadataList = null;
  private IAxis[] m_axes = null;
  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter( this );

  public DataCenterTimeserieItem( final DataCenterRepository rep, final DataCenterChannelItem parent, final Timeserie ts )
  {
    m_rep = rep;
    m_parent = parent;
    m_ts = ts;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_ts.getName();
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return getName();
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_parent.getIdentifier() + "." + String.valueOf( m_ts.getID() );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent() throws RepositoryException
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository( )
  {
    return m_rep;
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    if( anotherClass == IObservation.class )
      return this;

    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList( )
  {
    if( m_metadataList == null )
    {
      m_metadataList = new MetadataList();
      
      m_metadataList.put( ObservationConstants.MD_NAME, getName() );
      m_metadataList.put( ObservationConstants.MD_DESCRIPTION, 
          "Daten aus: " + m_ts.getDataTableName() );
      
      java.sql.Date begin = m_ts.getRealBegin();
      if( begin != null )
      {
        m_metadataList.put( TimeserieConstants.MD_DATE_BEGIN, TimeserieConstants.DEFAULT_DF.format( begin ) );
        m_metadataList.put( TimeserieConstants.MD_DATE_END, TimeserieConstants.DEFAULT_DF.format( m_ts.getRealEnd() ) );
      }
    }
    
    return m_metadataList;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    if( m_axes == null )
    {
      // TODO status axis...
      m_axes = new IAxis[2];
      
      m_axes[0] = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true, true );
      
      String type = "?";
      String unit = "?";
      
      try
      {
        type = DataCenterUtils.toKalypsoType( m_parent.getChannel().getType() );
      }
      catch( SQLException e )
      {
        e.printStackTrace();
      }

      try
      {
        unit = DataCenterUtils.toKalypsoUnit( m_parent.getChannel().getUnit() );
      }
      catch( SQLException e )
      {
        e.printStackTrace();
      }

      m_axes[1] = new DefaultAxis( m_ts.getName(), type, unit, Double.class, false, true );
    }
    
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    final TimeserieTupple[] tupples;
    
    try
    {
      if( args instanceof DateRangeArgument )
      {
        final DateRangeArgument dra = (DateRangeArgument) args;
        tupples = m_ts.getValues( dra.getFrom(), dra.getTo() );
      }
      else
        tupples = m_ts.getValues( null, null );

      return new DataCenterTuppleModel( tupples, getAxisList() );
    }
    catch( SQLException e )
    {
      throw new SensorException(e);
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    m_ts.setValues( DataCenterTuppleModel.toTupples( values ) );
    
    m_evtPrv.fireChangedEvent();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    return "";
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#addListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void addListener( final IObservationListener listener )
  {
    m_evtPrv.addListener( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#removeListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void removeListener( IObservationListener listener )
  {
    m_evtPrv.removeListener( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#clearListeners()
   */
  public void clearListeners( )
  {
    m_evtPrv.clearListeners();
  }
}
