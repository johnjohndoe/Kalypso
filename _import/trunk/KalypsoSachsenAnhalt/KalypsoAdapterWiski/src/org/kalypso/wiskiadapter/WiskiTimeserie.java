package org.kalypso.wiskiadapter;

import java.rmi.RemoteException;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.repository.RepositoryException;
import org.kalypso.util.conversion.units.SIConverter;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.wiskiadapter.wiskicall.GetAlarmLevelList;
import org.kalypso.wiskiadapter.wiskicall.GetRatingTables;
import org.kalypso.wiskiadapter.wiskicall.GetStationDetailList;
import org.kalypso.wiskiadapter.wiskicall.GetTsData;
import org.kalypso.wiskiadapter.wiskicall.IsTsWritable;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;

/**
 * WiskiTimeserie
 * 
 * @author schlienger
 */
public class WiskiTimeserie implements IObservation
{
  private final TsInfoItem m_tsinfo;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter(
      this );

  private final IAxis[] m_axes;

  private final SIConverter m_cv;

  private MetadataList m_metadata = null;

  private Boolean m_editable = null;

  private ITuppleModel m_cachedValues = null;

  private DateRangeArgument m_cachedDr = null;

  public WiskiTimeserie( final TsInfoItem tsinfo )
  {
    m_tsinfo = tsinfo;

    m_axes = new IAxis[3];
    m_axes[0] = new DefaultAxis( TimeserieUtils
        .getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE,
        TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class,
        true );

    final String wiskiType = tsinfo.getWiskiType();
    final String kalypsoType = WiskiUtils.wiskiType2Kalypso( wiskiType );
    final String wiskiUnit = tsinfo.getWiskiUnit();
    final String kalypsoUnit = TimeserieUtils.getUnit( kalypsoType );

    m_cv = new SIConverter( wiskiUnit, kalypsoUnit );

    m_axes[1] = new DefaultAxis( TimeserieUtils.getName( kalypsoType ),
        kalypsoType, kalypsoUnit, Double.class, false );

    m_axes[2] = KalypsoStatusUtils.createStatusAxisFor( m_axes[1] );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_tsinfo.getIdentifier();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName( )
  {
    return m_tsinfo.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
  {
    if( m_editable == null )
    {
      try
      {
        final IsTsWritable call = new IsTsWritable( m_tsinfo.getWiskiId() );
        final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();
        rep.executeWiskiCall( call );

        m_editable = call.getEditable();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        m_editable = Boolean.FALSE;
      }
    }

    return m_editable.booleanValue();
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
    if( m_metadata == null )
    {
      m_metadata = new MetadataList();

      m_metadata.setProperty( ObservationConstants.MD_NAME, m_tsinfo
          .getWiskiName() );
      m_metadata.setProperty( ObservationConstants.MD_DESCRIPTION, m_tsinfo
          .getWiskiDescription() );

      try
      {
        fetchStationMetadata( m_metadata );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      try
      {
        fetchAlarmLevels( m_metadata );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      // note: WQ-Table is only incorporated into metadata
      // when calling getValues()
    }

    return m_metadata;
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

    final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();

    try
    {
      // tricky: when no date range specified, we create a default one
      if( args == null || !(args instanceof DateRangeArgument) )
      {
        dr = DateRangeArgument.createFromPastDays( Integer.valueOf(
            WiskiUtils.getProperties().getProperty(
                WiskiUtils.PROP_NUMBER_OF_DAYS, "7" ) ).intValue() );
      }
      else
        dr = (DateRangeArgument) args;

      if( dr.equals( m_cachedDr ) )
        return m_cachedValues;

      m_cachedDr = dr;

      try
      {
        // fetch WQTable now since we know the time-range
        fetchWQTable( getMetadataList(), dr.getFrom(), dr.getTo() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

      final GetTsData call = new GetTsData( m_tsinfo.getWiskiId(), dr );
      rep.executeWiskiCall( call );

      if( call.getData() == null )
        m_cachedValues = SimpleTuppleModel.EMPTY_TUPPLEMODEL;
      else
        m_cachedValues = new WiskiTuppleModel( getAxisList(), call.getData(),
            m_cv );

      return m_cachedValues;
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
    // no meaning here
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

  /**
   * Helper for translating Wiski Rating-Tables into Kalypso Metadata
   */
  private void fetchWQTable( final MetadataList metadata, final Date from,
      final Date to ) throws NumberFormatException, WQException,
      RemoteException, KiWWException, RepositoryException
  {
    final String sourceType = m_axes[1].getType();
    final String destType;
    if( sourceType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      destType = TimeserieConstants.TYPE_WATERLEVEL;
    else if( sourceType.equals( TimeserieConstants.TYPE_VOLUME ) )
      destType = TimeserieConstants.TYPE_WATERLEVEL;
    else if( sourceType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      destType = TimeserieConstants.TYPE_RUNOFF; // here we could have also said
    // TYPE_VOLUME but since we don't know what the client
    // wants at this time we leave TYPE_RUNOFF as default
    else
      return;

    final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();

    final GetRatingTables call = new GetRatingTables( m_tsinfo.getWiskiId(), to );
    rep.executeWiskiCall( call );

    final WQTable wqt = new WQTable( from, call.getW(), call.getQ() );
    final WQTableSet set = new WQTableSet( new WQTable[] { wqt }, sourceType,
        destType );
    final String xml = WQTableFactory.createXMLString( set );

    metadata.setProperty( TimeserieConstants.MD_WQTABLE, xml );
  }

  /**
   * Helper for translating wiski alarm levels into kalypso metadata
   */
  private void fetchAlarmLevels( final MetadataList md )
      throws NumberFormatException, RemoteException, KiWWException,
      RepositoryException
  {
    final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();
    final GetAlarmLevelList call = new GetAlarmLevelList( m_tsinfo.getWiskiId() );
    rep.executeWiskiCall( call );
    if( call.getAlarmList() != null )
    {
      for( final Iterator it = call.getAlarmList().iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();

        final String level = (String) map.get( "epv_name" );
        final String value = (String) map.get( "epv_value" );

        md.setProperty( WiskiUtils.wiskiMetadataName2Kalypso( level ), value );
      }
    }
  }

  /**
   * Helper for getting metadata of the corresponding station
   */
  private void fetchStationMetadata( final MetadataList md )
      throws NumberFormatException, RemoteException, KiWWException,
      RepositoryException
  {
    final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();
    final GetStationDetailList call = new GetStationDetailList( Long
        .valueOf( m_tsinfo.getWiskiStationId() ) );
    rep.executeWiskiCall( call );

    final HashMap details = call.getDetails();
    final String carteasting = (String) details.get( "station_carteasting" );
    if( carteasting != null )
      md.setProperty( TimeserieConstants.MD_GKR, carteasting );
    final String cartnorthing = (String) details.get( "station_cartnorthing" );
    if( cartnorthing != null )
      md.setProperty( TimeserieConstants.MD_GKH, cartnorthing );
    final String river = (String) details.get( "river_longname" );
    if( river != null )
      md.setProperty( TimeserieConstants.MD_FLUSS, river );
    final String catchment = (String) details.get( "catchment_name" );
    if( catchment != null )
      md.setProperty( TimeserieConstants.MD_FLUSSGEBIET, catchment );
  }
}
