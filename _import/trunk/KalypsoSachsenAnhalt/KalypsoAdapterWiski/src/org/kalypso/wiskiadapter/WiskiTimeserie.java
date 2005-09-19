package org.kalypso.wiskiadapter;

import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.TimeZone;
import java.util.logging.Logger;

import org.kalypso.commons.conversion.units.SIConverter;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilter;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.wiskicall.GetAlarmLevelList;
import org.kalypso.wiskiadapter.wiskicall.GetRatingTables;
import org.kalypso.wiskiadapter.wiskicall.GetStationDetailList;
import org.kalypso.wiskiadapter.wiskicall.GetTsData;
import org.kalypso.wiskiadapter.wiskicall.IsTsWritable;
import org.kalypso.wiskiadapter.wiskicall.SetTsData;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;

/**
 * WiskiTimeserie
 * 
 * @author schlienger
 */
public class WiskiTimeserie implements IObservation
{
  private final TsInfoItem m_tsinfo;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter( this );

  private final IAxis[] m_axes;

  private final SIConverter m_cv;

  private MetadataList m_metadata = null;

  private Boolean m_editable = null;

  private ITuppleModel m_cachedValues = SimpleTuppleModel.EMPTY_TUPPLEMODEL;

  private DateRange m_cachedDr = null;

  public WiskiTimeserie( final TsInfoItem tsinfo )
  {
    if( tsinfo == null )
      throw new IllegalArgumentException( "TsInfoItem must not be null" );

    m_tsinfo = tsinfo;

    m_axes = new IAxis[3];
    m_axes[0] = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE,
        TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class, true );

    final String wiskiType = tsinfo.getWiskiType();
    final String kalypsoType = WiskiUtils.wiskiType2Kalypso( wiskiType );
    final String wiskiUnit = tsinfo.getWiskiUnit();
    final String kalypsoUnit = TimeserieUtils.getUnit( kalypsoType );

    m_cv = new SIConverter( wiskiUnit, kalypsoUnit );

    m_axes[1] = new DefaultAxis( TimeserieUtils.getName( kalypsoType ), kalypsoType, kalypsoUnit, Double.class, false );

    m_axes[2] = KalypsoStatusUtils.createStatusAxisFor( m_axes[1], true );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_tsinfo.getIdentifier();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName()
  {
    return m_tsinfo.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    if( m_editable == null )
    {
      try
      {
        // 1. check if this is a prognose
        boolean prognosed = m_tsinfo.getWiskiCustomId().indexOf( "Prognose" ) != -1;

        final IsTsWritable call = new IsTsWritable( m_tsinfo.getWiskiId() );
        final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
        rep.executeWiskiCall( call );

        // 2. check if this is editable in the wiski sense
        boolean editable = call.getEditable().booleanValue();

        // 3. fact
        m_editable = new Boolean( prognosed & editable );
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
  public Object getTarget()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList()
  {
    if( m_metadata == null )
    {
      m_metadata = new MetadataList();

      m_metadata.setProperty( ObservationConstants.MD_NAME, m_tsinfo.getWiskiName() );
      m_metadata.setProperty( ObservationConstants.MD_DESCRIPTION, m_tsinfo.getWiskiDescription() );
      m_metadata.put( ObservationConstants.MD_ORIGIN, "Wiski" );

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
  public IAxis[] getAxisList()
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
  public ITuppleModel getValues( final IRequest req ) throws SensorException
  {
    final DateRange dr;

    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();

    try
    {
      // tricky: when no date range specified, we create a default one
      if( req == null || req.getDateRange() == null )
      {
        dr = DateRange.createFromPastDays( Integer.valueOf(
            WiskiUtils.getProperties().getProperty( WiskiUtils.PROP_NUMBER_OF_DAYS, "7" ) ).intValue() );
      }
      else
        dr = req.getDateRange();

      if( dr.equals( m_cachedDr ) )
        return m_cachedValues;

      m_cachedDr = dr;

      try
      {
        final String useType;
        if( req != null && Arrays.binarySearch( req.getAxisTypes(), TimeserieConstants.TYPE_WATERLEVEL ) >= 0 )
          useType = TimeserieConstants.TYPE_WATERLEVEL;
        else if( req != null && Arrays.binarySearch( req.getAxisTypes(), TimeserieConstants.TYPE_VOLUME ) >= 0 )
          useType = TimeserieConstants.TYPE_VOLUME;
        else
          useType = TimeserieConstants.TYPE_RUNOFF;

        // fetch WQTable now since we know the time-range
        fetchWQTable( getMetadataList(), dr.getFrom(), dr.getTo(), useType );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      final GetTsData call = new GetTsData( m_tsinfo.getWiskiId(), dr );
      rep.executeWiskiCall( call );

      if( call.getData() == null )
        m_cachedValues = new SimpleTuppleModel( getAxisList() );
      else
        m_cachedValues = new WiskiTuppleModel( getAxisList(), call.getData(), m_cv, call.getTimeZone() );

      return m_cachedValues;
    }
    catch( final Exception e ) // RepositoryException, RemoteException, KiWWException
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    if( !isEditable() )
      throw new SensorException( toString() + " ist nicht editierbar! Werte. "
          + "Werte dürfen nicht geschrieben werden." );

    // create a fake observation for filter purposes
    final IObservation obs = new SimpleObservation();
    obs.setValues( values );

    // filter values in order to comply with the wiski specification
    final int timeUnit = m_tsinfo.getWiskiDistUnitAsCalendarField();
    final int timeStep = m_tsinfo.getWiskiDistValue();
    final InterpolationFilter intfil = new InterpolationFilter( timeUnit, timeStep, false, 0,
        KalypsoStati.STATUS_USERMOD.intValue() );
    intfil.initFilter( null, obs, null );

    final HashMap timeseries_map = new HashMap();
    final HashMap tsID_map = new HashMap();
    final HashMap ts_values_map = new HashMap();
    final HashMap value_tsinfo_map = new HashMap();
    final LinkedList value_tscoldesc_ll = new LinkedList();
    final HashMap value_tscoldesc_map = new HashMap();
    final LinkedList value_tsdata_ll = new LinkedList();
    final LinkedHashMap value_tstamp_hash_lmap = new LinkedHashMap();

    final IAxis dateAxis = ObservationUtilities.findAxisByClass( values.getAxisList(), Date.class );
    final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( values.getAxisList(), Number.class, true );
    //final IAxis statusAxis = KalypsoStatusUtils.findStatusAxisFor( values
    //  .getAxisList(), valueAxis );

    final ITuppleModel filteredValues = intfil.getValues( null );
    for( int ix = 0; ix < filteredValues.getCount(); ix++ )
    {
      final HashMap row = new HashMap();

      final Date date = (Date)filteredValues.getElement( ix, dateAxis );
      final Number value = (Number)filteredValues.getElement( ix, valueAxis );
      //final Number status = (Number) filteredValues.getElement( ix,
      // statusAxis );

      row.put( "timestamp", new Timestamp( date.getTime() ) );
      row.put( "tsc_value0", new Double( value.doubleValue() ) );
      row.put( "status", new Long( 0 ) );

      value_tsdata_ll.add( row );
    }

    // compose setTsData HashMap

    // HACK: retrieve the server-side-timezone, if specified, from our metadatalist
    final String strtz = getMetadataList().getProperty( "_SERVER_SIDE_TIMEZONE_" );
    final TimeZone tz = strtz != null ? TimeZone.getTimeZone( strtz ) : TimeZone.getDefault();
    final int utcOffset = tz.getOffset( new Date().getTime() ) / 1000 / 60 / 60;
    // END-HACK

    value_tsinfo_map.put( "utcoffset", new Integer( utcOffset ) );

    ts_values_map.put( KiWWDataProviderInterface.KEY_TSINFO, value_tsinfo_map );

    value_tscoldesc_ll.add( value_tscoldesc_map );
    ts_values_map.put( KiWWDataProviderInterface.KEY_TSCOLDESC, value_tscoldesc_ll );
    ts_values_map.put( KiWWDataProviderInterface.KEY_TSDATA, value_tsdata_ll );
    ts_values_map.put( KiWWDataProviderInterface.KEY_TSTAMP_HASH, value_tstamp_hash_lmap );
    tsID_map.put( m_tsinfo.getWiskiIdAsString(), ts_values_map );
    timeseries_map.put( KiWWDataProviderInterface.KEY_TIMESERIES, tsID_map );

    //setTsData
    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
    final SetTsData call = new SetTsData( timeseries_map );
    try
    {
      rep.executeWiskiCall( call );
    }
    catch( final Exception e ) // RemoteException, KiWWException,
    // RepositoryException
    {
      throw new SensorException( e );
    }

    if( !call.isSuccess() )
      throw new SensorException( "Konnte Daten nicht zurückschreiben." );

    m_evtPrv.fireChangedEvent( null );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref()
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
  public void clearListeners()
  {
    m_evtPrv.clearListeners();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#fireChangedEvent(java.lang.Object)
   */
  public void fireChangedEvent( final Object source )
  {
    m_evtPrv.fireChangedEvent( source );
  }

  /**
   * Helper for translating Wiski Rating-Tables into Kalypso Metadata
   */
  private void fetchWQTable( final MetadataList metadata, final Date from, final Date to, String useType )
  {
    final String sourceType = m_axes[1].getType();
    final String destType;
    if( sourceType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      destType = TimeserieConstants.TYPE_WATERLEVEL; // force to W
    else if( sourceType.equals( TimeserieConstants.TYPE_VOLUME ) )
      destType = TimeserieConstants.TYPE_WATERLEVEL; // force to W
    else if( sourceType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      destType = useType; // here use the Type found in the request
    else
      return;

    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();

    WQTableSet wqTableSet = null;

    // 1. first try: using the normal way (our tsinfo)
    WQTable wqt = internFetchTable( m_tsinfo, rep, from, to );
    if( wqt == null )
    {
      // 2. this failed, so next try is using sibling of other type
      // which might also contain a usable rating table

      // try with sibling of other type: W or Q/V depending on our type
      String otherType = null;
      if( m_tsinfo.getWiskiType().equals( "W" ) )
      {
        if( useType.equals( TimeserieConstants.TYPE_RUNOFF ) )
          otherType = "Q";
        else
          otherType = "V";
      }
      else
        otherType = "W";

      final TsInfoItem tsi = m_tsinfo.findSibling( otherType );
      if( tsi != null )
        wqt = internFetchTable( tsi, rep, from, to );
    }

    if( wqt != null )
    {
      // great! we got a table, so let's use it and save it in the cache
      wqTableSet = new WQTableSet( new WQTable[]
      { wqt }, sourceType, destType );

      RatingTableCache.getInstance().check( wqTableSet, m_tsinfo.getIdentifier(), to );
    }
    else
    {
      // still no wqtable, try to load this WQ-Table from the cache
      wqTableSet = RatingTableCache.getInstance().get( m_tsinfo.getIdentifier(), to );
    }

    if( wqTableSet != null )
    {
      try
      {
        final String xml = WQTableFactory.createXMLString( wqTableSet );
        metadata.setProperty( TimeserieConstants.MD_WQTABLE, xml );
      }
      catch( final WQException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * Helper: tries to load the wq table from wiski. If fails or if no table found, null is returned
   */
  private WQTable internFetchTable( final TsInfoItem tsinfo, final WiskiRepository rep, final Date from, final Date to )
  {
    final GetRatingTables call = new GetRatingTables( tsinfo.getWiskiId(), to );
    try
    {
      rep.executeWiskiCall( call );
    }
    catch( Exception e )
    {
      e.printStackTrace();

      return null;
    }

    if( call.hasTable() )
    {
      final WQTable wqt = new WQTable( from, call.getW(), call.getQ() );

      return wqt;
    }

    return null;
  }

  /**
   * Helper for translating wiski alarm levels into kalypso metadata
   */
  private void fetchAlarmLevels( final MetadataList md ) throws NumberFormatException, RemoteException, KiWWException,
      RepositoryException
  {
    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
    final GetAlarmLevelList call = new GetAlarmLevelList( m_tsinfo.getWiskiId() );
    rep.executeWiskiCall( call );
    if( call.getAlarmList() != null )
    {
      for( final Iterator it = call.getAlarmList().iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap)it.next();

        final String level = (String)map.get( "epv_name" );
        final String value = (String)map.get( "epv_value" );

        try
        {
          md.setProperty( WiskiUtils.wiskiMetadataName2Kalypso( level ), value );
        }
        catch( final IllegalArgumentException e )
        {
          Logger.getLogger( getClass().getName() ).warning( "Metadata-Eigenschaft nicht erkannt: " + level );
        }
      }
    }
  }

  /**
   * Helper for getting metadata of the corresponding station
   */
  private void fetchStationMetadata( final MetadataList md ) throws NumberFormatException, RemoteException,
      KiWWException, RepositoryException
  {
    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
    final GetStationDetailList call = new GetStationDetailList( Long.valueOf( m_tsinfo.getWiskiStationId() ) );
    rep.executeWiskiCall( call );

    final HashMap details = call.getDetails();
    final String carteasting = (String)details.get( "station_carteasting" );
    if( carteasting != null )
    {
      md.setProperty( TimeserieConstants.MD_GKR, carteasting.trim() );

      final String crds = TimeserieUtils.getCoordinateSystemNameForGkr( carteasting.trim() );
      md.setProperty( TimeserieConstants.MD_COORDSYS, crds );
    }
    final String cartnorthing = (String)details.get( "station_cartnorthing" );
    if( cartnorthing != null )
      md.setProperty( TimeserieConstants.MD_GKH, cartnorthing.trim() );
    final String river = (String)details.get( "river_longname" );
    if( river != null )
      md.setProperty( TimeserieConstants.MD_GEWAESSER, river );
    final String catchment = (String)details.get( "catchment_name" );
    if( catchment != null )
      md.setProperty( TimeserieConstants.MD_FLUSSGEBIET, catchment );
    final String kennz = (String)details.get( "station_no" );
    if( kennz != null )
      md.setProperty( TimeserieConstants.MD_KENNZIFFER, kennz );
  }
}
