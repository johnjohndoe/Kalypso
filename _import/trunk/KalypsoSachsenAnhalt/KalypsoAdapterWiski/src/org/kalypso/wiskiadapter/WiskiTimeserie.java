package org.kalypso.wiskiadapter;

import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.util.conversion.units.SIConverter;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;

/**
 * WiskiTimeserie
 * 
 * @author schlienger
 */
public class WiskiTimeserie implements IObservation
{
  private final StationParameter m_parameter;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter(
      this );

  private final IAxis[] m_axes;

  private final SIConverter m_cv;

  public WiskiTimeserie( final StationParameter parameter )
  {
    m_parameter = parameter;
    
    m_axes = new IAxis[3];
    m_axes[0] = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE, TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class, true );
    
    final String wiskiType = parameter.getWiskiType();
    final String kalypsoType = WiskiUtils.wiskiType2Kalypso( wiskiType );
    final String wiskiUnit = parameter.getWiskiUnit();
    final String kalypsoUnit = TimeserieUtils.getUnit( kalypsoType );
    
    m_cv = new SIConverter( wiskiUnit, kalypsoUnit );
    
    m_axes[1] = new DefaultAxis( TimeserieUtils.getName( kalypsoType ), kalypsoType, kalypsoUnit, Double.class, false );
    
    m_axes[2] = KalypsoStatusUtils.createStatusAxisFor( m_axes[1] );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_parameter.getIdentifier();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName( )
  {
    return m_parameter.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
  {
    // TODO only a prognose is editable
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
    return m_parameter.getMetadataList();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args )
      throws SensorException
  {
    final DateRangeArgument dr;

    try
    {
      // tricky: when no date range specified, we create a default one
      if( args == null || !(args instanceof DateRangeArgument) )
        dr = DateRangeArgument.createFromPastDays( Integer.valueOf(
            WiskiUtils.getProperties().getProperty(
                WiskiUtils.PROP_NUMBER_OF_DAYS, "7" ) ).intValue() );
      else
        dr = (DateRangeArgument) args;

      //  data range
      final Timestamp from = new Timestamp( dr.getFrom().getTime() );
      final Timestamp to = new Timestamp( dr.getTo().getTime() );

      final WiskiRepository rep = (WiskiRepository) m_parameter.getRepository();

      //getTsData for specified timeseries (or ts list)
      final HashMap gettsdata = rep.getWiski().getTsData( rep.getUserData(),
          new long[] { Long.parseLong( m_parameter.getWiskiId() ) }, from, to,
          null );

      final LinkedList data = (LinkedList) ((HashMap) ((HashMap) gettsdata
          .get( KiWWDataProviderInterface.KEY_TIMESERIES )).get( m_parameter
          .getWiskiId() )).get( KiWWDataProviderInterface.KEY_TSDATA );

      return new WiskiTuppleModel( getAxisList(), data, m_cv );
    }
    catch( Exception e ) // RepositoryException, RemoteException, KiWWException
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#addListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void addListener( IObservationListener listener )
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
