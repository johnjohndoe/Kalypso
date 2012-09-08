package org.kalypso.dcadapter;

import java.sql.SQLException;
import java.util.Date;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.dcadapter.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.IObservationListener;
import org.kalypso.ogc.sensor.event.ObservationChangeType;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.metadata.IMetadataConstants;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.util.Observations;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.IRepositoryItemVisitor;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.utils.RepositoryVisitors;

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

  @Override
  public String getName( )
  {
    return m_ts.getName();
  }

  @Override
  public String toString( )
  {
    return getName();
  }

  @Override
  public String getIdentifier( )
  {
    return m_parent.getIdentifier() + "." + String.valueOf( m_ts.getID() ); //$NON-NLS-1$
  }

  @Override
  public IRepositoryItem getParent( )
  {
    return m_parent;
  }

  @Override
  public boolean hasChildren( )
  {
    return false;
  }

  @Override
  public IRepositoryItem[] getChildren( )
  {
    return new IRepositoryItem[] {};
  }

  @Override
  public IRepository getRepository( )
  {
    return m_rep;
  }

  @Override
  public Object getAdapter( final Class anotherClass )
  {
    if( anotherClass == IObservation.class )
      return this;

    return null;
  }

  @Override
  public MetadataList getMetadataList( )
  {
    if( m_metadataList == null )
    {
      m_metadataList = new MetadataList();

      m_metadataList.put( IMetadataConstants.MD_NAME, getName() );
      m_metadataList.put( IMetadataConstants.MD_DESCRIPTION, Messages.getString( "org.kalypso.dcadapter.DataCenterTimeserieItem.0" ) + m_ts.getDataTableName() ); //$NON-NLS-1$
      m_metadataList.put( IMetadataConstants.MD_ORIGIN, Messages.getString( "org.kalypso.dcadapter.DataCenterTimeserieItem.1" ) ); //$NON-NLS-1$

      final java.sql.Date begin = m_ts.getRealBegin();
      if( begin != null )
      {
        final String strBegin = DateUtilities.printDateTime( begin, KalypsoCorePlugin.getDefault().getTimeZone() );
        final String strRealEnd = DateUtilities.printDateTime( begin, KalypsoCorePlugin.getDefault().getTimeZone() );
        m_metadataList.put( ITimeseriesConstants.MD_DATE_BEGIN, strBegin );
        m_metadataList.put( ITimeseriesConstants.MD_DATE_END, strRealEnd );
      }
    }

    return m_metadataList;
  }

  @Override
  public IAxis[] getAxes( )
  {
    if( m_axes == null )
    {
      // TODO status axis...
      m_axes = new IAxis[2];

      m_axes[0] = new DefaultAxis( Messages.getString( "org.kalypso.dcadapter.DataCenterTimeserieItem.2" ), ITimeseriesConstants.TYPE_DATE, "", Date.class, true, true ); //$NON-NLS-1$ //$NON-NLS-2$

      String type = "?"; //$NON-NLS-1$
      String unit = "?"; //$NON-NLS-1$

      try
      {
        type = DataCenterUtils.toKalypsoType( m_parent.getChannel().getType() );
      }
      catch( final SQLException e )
      {
        e.printStackTrace();
      }

      try
      {
        unit = DataCenterUtils.toKalypsoUnit( m_parent.getChannel().getUnit() );
      }
      catch( final SQLException e )
      {
        e.printStackTrace();
      }

      m_axes[1] = new DefaultAxis( m_ts.getName(), type, unit, Double.class, false, true );
    }

    return m_axes;
  }

  @Override
  public ITupleModel getValues( final IRequest request ) throws SensorException
  {
    final TimeserieTupple[] tupples;

    try
    {
      if( request.getDateRange() != null )
      {
        final DateRange dra = request.getDateRange();
        tupples = m_ts.getValues( dra.getFrom(), dra.getTo() );
      }
      else
        tupples = m_ts.getValues( null, null );

      return new DataCenterTuppleModel( tupples, getAxes() );
    }
    catch( final SQLException e )
    {
      throw new SensorException( e );
    }
  }

  @Override
  public void setValues( final ITupleModel values ) throws SensorException
  {
    m_ts.setValues( DataCenterTuppleModel.toTupples( values ) );

    m_evtPrv.fireChangedEvent( null, new ObservationChangeType( IObservationListener.STRUCTURE_CHANGE ) );
  }

  @Override
  public String getHref( )
  {
    return ""; //$NON-NLS-1$
  }

  @Override
  public void addListener( final IObservationListener listener )
  {
    m_evtPrv.addListener( listener );
  }

  @Override
  public void removeListener( final IObservationListener listener )
  {
    m_evtPrv.removeListener( listener );
  }

  @Override
  public void fireChangedEvent( final Object source, final ObservationChangeType type )
  {
    m_evtPrv.fireChangedEvent( source, type );
  }

  @Override
  public boolean hasAdapter( final Class< ? > adapter )
  {
    return false;
  }

  @Override
  public boolean isMultipleSourceItem( )
  {
    return false;
  }

  @Override
  public void accept( final IRepositoryItemVisitor visitor ) throws RepositoryException
  {
    RepositoryVisitors.accept( this, visitor );
  }

  @Override
  public void accept( final IObservationVisitor visitor, final IRequest request, final int direction ) throws SensorException
  {
    Observations.accept( this, visitor, request, direction );
  }

  @Override
  public boolean isEmpty( )
  {
    return false;
  }
}
