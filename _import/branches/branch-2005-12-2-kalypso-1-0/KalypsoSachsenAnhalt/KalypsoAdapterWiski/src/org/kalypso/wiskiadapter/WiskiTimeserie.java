package org.kalypso.wiskiadapter;

import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.NoSuchElementException;
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

import com.braju.format.Format;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;

/**
 * WiskiTimeserie
 * 
 * @author schlienger
 */
public class WiskiTimeserie implements IObservation, IWiskiConstants
{
  private final TsInfoItem m_tsinfo;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter( this );

  private final IAxis[] m_axes;

  private final SIConverter m_cv;

  private MetadataList m_metadata = null;

  private Boolean m_editable = null;

  private ITuppleModel m_cachedValues = SimpleTuppleModel.EMPTY_TUPPLEMODEL;

  private DateRange m_cachedDr = null;

  private final Logger LOG = Logger.getLogger( WiskiTimeserie.class.getName() );

  public WiskiTimeserie( final TsInfoItem tsinfo )
  {
    if( tsinfo == null )
      throw new IllegalArgumentException( "TsInfoItem must not be null" );

    m_tsinfo = tsinfo;

    m_axes = new IAxis[3];
    m_axes[0] = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE, TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class, true );

    final String wiskiType = tsinfo.getWiskiParameterType();
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
        boolean prognosed = m_tsinfo.isForecast();

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

      m_metadata.setProperty( ObservationConstants.MD_NAME, m_tsinfo.getName() );
      m_metadata.setProperty( ObservationConstants.MD_DESCRIPTION, m_tsinfo.getWiskiDescription() );
      m_metadata.put( ObservationConstants.MD_ORIGIN, "Wiski" );

      // Wiski-specific MD
      m_metadata.put( MD_WISKI_PARAMETER_TYPE, m_tsinfo.getWiskiParameterType() );
      m_metadata.put( MD_WISKI_PARAMETER_TYPE_LONGNAME, m_tsinfo.getWiskiParametertypeLongname() );
      m_metadata.put( MD_WISKI_STATION_NAME, m_tsinfo.getWiskiStationName() );
      m_metadata.put( MD_WISKI_STATION_NO, m_tsinfo.getWiskiStationNo() );
      m_metadata.put( MD_WISKI_STATION_PARAMETER_NAME, m_tsinfo.getWiskiStationparameterName() );
      m_metadata.put( MD_WISKI_STATION_PARAMETER_LONGNAME, m_tsinfo.getWiskiStationparameterLongname() );
      m_metadata.put( MD_WISKI_UNIT, m_tsinfo.getWiskiUnit() );

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
    // if true, incomming and outgoing values are dumped to System.out
    // should be false normally
    /* We would like to fetch true/false from the config files, but this is not easily made.... */
    final boolean bDump = false;

    final DateRange dr;

    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();

    try
    {
      // tricky: when no date range specified, we create a default one
      if( req == null || req.getDateRange() == null )
      {
        dr = DateRange.createFromPastDays( Integer.valueOf( WiskiUtils.getProperty( PROP_NUMBER_OF_DAYS, "7" ) ).intValue() );
      }
      else
      {
        final DateRange dateRange = req.getDateRange();
        final WiskiTimeConverter timeConverter = new WiskiTimeConverter( TimeZone.getDefault(), m_tsinfo );

        final Date from = timeConverter.kalypsoToWiski( dateRange.getFrom() );
        final Date to = timeConverter.kalypsoToWiski( dateRange.getTo() );

        final DateRange wiskiDr = new DateRange( from, to );
        dr = wiskiDr;
      }

      if( dr.equals( m_cachedDr ) )
        return m_cachedValues;

      m_cachedDr = dr;

      if( bDump )
      {
        System.out.println( "" );
        System.out.println( "Fetching timeserie: " + getName() );
        System.out.println( "DateRange: " + dr );
      }

      try
      {
        // fetch WQTable now since we know the time-range
        fetchWQTable( getMetadataList(), dr.getFrom(), dr.getTo() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      final GetTsData call = new GetTsData( m_tsinfo.getWiskiId(), dr );
      rep.executeWiskiCall( call );

      final LinkedList data = call.getData();
      if( data == null )
        m_cachedValues = new SimpleTuppleModel( getAxisList() );
      else
      {
        final WiskiTimeConverter timeConverter = new WiskiTimeConverter( call.getTimeZone(), m_tsinfo );
        m_cachedValues = new WiskiTuppleModel( getAxisList(), data, m_cv, timeConverter );

        if( bDump )
          System.out.println( "timeConverter: " + timeConverter );
      }

      if( bDump )
      {
        if( data != null )
        {
          System.out.println( "Data retrieved from WDP: " );
          for( final Iterator iter = data.iterator(); iter.hasNext(); )
          {
            final Object elt = iter.next();
            System.out.println( elt );
          }
        }

        System.out.println();
        System.out.println( "Generated observation tuple:" );
        System.out.println( ObservationUtilities.dump( m_cachedValues, "\t" ) );
        System.out.println();
        System.out.println();
      }

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
    // if true, incomming and outgoing values are dumped to System.out
    // should be false normally
    final boolean bDump = false;

    if( bDump )
      System.out.println( ObservationUtilities.dump( values, "\t" ) );

    if( !isEditable() )
      throw new SensorException( toString() + " ist nicht editierbar! Werte. " + "Werte dürfen nicht geschrieben werden." );

    // create a fake observation for filter purposes
    final IObservation obs = new SimpleObservation( values.getAxisList() );
    obs.setValues( values );

    // filter values in order to comply with the wiski specification
    final int timeUnit = WiskiUtils.getDistUnitCalendarField( m_tsinfo.getWiskiDistUnit() );
    final int timeStep = m_tsinfo.getWiskiDistValue();
    final InterpolationFilter intfil = new InterpolationFilter( timeUnit, timeStep, false, "0", KalypsoStati.STATUS_USERMOD.intValue() );
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

    // find corresponding axis type
    final String wiskiType = m_tsinfo.getWiskiParameterType();
    final String kalypsoType = WiskiUtils.wiskiType2Kalypso( wiskiType );
    final IAxis valueAxis;
    try
    {
      valueAxis = ObservationUtilities.findAxisByType( values.getAxisList(), kalypsoType );
    }
    catch( final NoSuchElementException e )
    {
      throw new SensorException( "Die Zeitreihenwerte können nicht nach Wiski geschrieben werden" + ". Keine Achse vom Typ " + kalypsoType + " wurde gefunden." );
    }

    final DateFormat dfDump = DateFormat.getInstance();
    dfDump.setTimeZone( TimeZone.getTimeZone( "UTC" ) );

    final ITuppleModel filteredValues = intfil.getValues( null );
    for( int ix = 0; ix < filteredValues.getCount(); ix++ )
    {
      final HashMap row = new HashMap();

      final Date date = (Date)filteredValues.getElement( ix, dateAxis );
      final Number value = (Number)filteredValues.getElement( ix, valueAxis );

      if( bDump )
        System.out.println( "writing wiski: " + date.getTime() + " " + dfDump.format( date ) + " " + value );

      row.put( "timestamp", new Timestamp( date.getTime() ) );
      row.put( "tsc_value0", new Double( value.doubleValue() ) );
      row.put( "status", new Long( 0 ) );

      value_tsdata_ll.add( row );
    }

    // compose setTsData HashMap

    // utc offset in seconds
    // always use offset 0
    final int utcOffset = 0; //m_tzDest.getOffset( firstDate.getTime() ) / 1000;
    value_tsinfo_map.put( "utcoffset", new Integer( utcOffset ) );

    //System.out.println( "utcoffset: " + utcOffset );

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
   * Helper for translating Wiski rating-table into a Kalypso WQ-Table as Metadata
   * <p>
   * The rating table performs conversion from a "sourceType" to a "destType". The sourceType is given from the value
   * axis found in this observation. If the sourceType is:
   * <ul>
   * <li>Q, then the destType is W
   * <li>V, then the destType is W
   * <li>W, then the destType is derived from the requestType
   * </ul>
   * <p>
   * The search for a rating table is performed the following way
   * <nl>
   * <li>the current wiski parameter is asked if it has a rating table
   * <li>if that's not the case, depending on the destType, the sibling wiski group (which can be Wasserstand,
   * Durchfluss or Inhalt, see config.ini in the resources) is asked for the same station and a rating table is searched
   * there
   * <li>if neither here a table is found, the rating table cache of kalypso is asked
   * <li>if a table is found in wiski, it is first cached.
   * <li>the table (if any) is then converted into WQTable metadata.
   * </nl>
   * <p>
   * The rating table cache is only asked if nothing is found in the live system, thus as a last mean.
   */
  private void fetchWQTable( final MetadataList metadata, final Date dateFrom, final Date dateTo )
  {
    // HACK: Q-Förderstrom (Speicherabgabe Bode/Ilse) darf eigentlich keine WQ-Beziehung haben
    final String paramName = m_tsinfo.getStationParameterName();
    if( paramName != null && paramName.startsWith( "QF" ) )
    {
      LOG.info( "Type QF detected, will not search a Rating Table for: " + getName() );
      return;
    }

    final String sourceType = m_axes[1].getType();
    final String destType = findWQDestType( sourceType );

    /////// find table and put into metadata
    final WQTableSetPlusSource wqss = findAndCacheWQTable( dateFrom, dateTo, destType, sourceType );
    if( wqss != null )
    {
      LOG.info( "Schlüsselkurven für " + getName() + " gefunden: " + wqss.source );

      try
      {
        metadata.setProperty( MD_WISKI_WQ_SOURCE, wqss.source );
        metadata.setProperty( TimeserieConstants.MD_WQTABLE, WQTableFactory.createXMLString( wqss.wqs ) );
      }
      catch( final WQException e )
      {
        // should not occur
        LOG.warning( e.getLocalizedMessage() );
      }
    }
    else
      LOG.warning( "Keine Schlüsselkurve gefunden für: " + getName() );
  }

  /**
   * Returns the destination type (of the WQ-Table) for a given source type. <br>
   * This is hard coded, because this information can neither be obtained from WISKI nor from the request.<br>
   * We only support the three different cases:
   * <ul>
   *  <li>W -> Q</li>
   *  <li>Q -> W</li>
   *  <li>V -> NN</li>
   *</ul>
   *
   * So this only works, if we never ask for a W corrsesponding to a V. This is ensured by convention of the WISKI konfiguration. 
   */
  private String findWQDestType( String sourceType )
  {
    if( sourceType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      return TimeserieConstants.TYPE_WATERLEVEL;
    
    if( sourceType.equals( TimeserieConstants.TYPE_VOLUME ) )
      return TimeserieConstants.TYPE_NORMNULL;
    
    if( sourceType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return TimeserieConstants.TYPE_RUNOFF;
    
    return null;
  }

  /**
   * Sucht nach der WQ-Tabelle. Es wird immer zuerst in Wiski gesucht. wird dort nichts gefunden, wird im Cache gesucht.
   * <br>
   * Sobald etwas gefunden wurde, kommt es in den Cache.
   */
  private WQTableSetPlusSource findAndCacheWQTable( final Date from, final Date to, final String fromType, final String toType )
  {
    // Search in Wiski
    final WQTableSetPlusSource wqss = findWQTable( from, to, fromType, toType );
    if( wqss != null )
    {
      // great! we got a table, so let's use it and save it in the cache
      RatingTableCache.getInstance().check( wqss.wqs, m_tsinfo.getIdentifier(), to );
      return wqss;
    }

    // No table found :-( Try to find from cache
    final WQTableSet wqsFromCache = RatingTableCache.getInstance().get( m_tsinfo.getIdentifier(), to );
    if( wqsFromCache != null )
      return new WQTableSetPlusSource( wqsFromCache, "Kalypso-Server (Cache)" );

    return null;
  }

  /**
   * Sucht nach der WQ-Tabelle. Es wird immer zuerst in Wiski gesucht. wird dort nichts gefunden, wird im Cache gesucht.
   * <br>
   * Sobald etwas gefunden wurde, kommt es in den Cache.
   */
  private WQTableSetPlusSource findWQTable( final Date from, final Date to, final String fromType, final String toType )
  {
    final WQTableSet wqsFromTs = fetchWQTable( KiWWDataProviderInterface.OBJECT_TIMESERIES, m_tsinfo.getWiskiId(), from, to, fromType, toType );
    if( wqsFromTs != null )
    {
      final String wqSource = Format.sprintf( "Zeitreihe: %s", new Object[]
      { m_tsinfo.getIdentifier() } );

      return new WQTableSetPlusSource( wqsFromTs, wqSource );
    }

    // If we have a corresponding group; look if we have a timeserie on the same station in that group
    final String siblingGroup = WiskiUtils.getProperty( "WQSEARCH_" + m_tsinfo.getWiskiGroupName(), null );
    final TsInfoItem siblingTs = siblingGroup == null ? null : m_tsinfo.findSibling( siblingGroup );
    if( siblingTs != null )
    {
      final WQTableSet wqsFromSibling = fetchWQTable( KiWWDataProviderInterface.OBJECT_TIMESERIES, siblingTs.getWiskiId(), from, to, toType, fromType );
      if( wqsFromSibling != null )
      {
        final String wqSource = Format.sprintf( "Zeitreihe, indirekt: %s", new Object[]
        { siblingTs.getIdentifier() } );

        return new WQTableSetPlusSource( wqsFromSibling, wqSource );
      }
    }

    // Last resort: looking up via station; we do not do this any more, as more than one different schluesseklkurven can
    // be configured per station
    //    final WQTableSet wqsFromStation = fetchWQTable( KiWWDataProviderInterface.OBJECT_STATION,
    // m_tsinfo.getWiskiStationId(), from, to, fromType, toType );
    //    if( wqsFromStation != null )
    //    {
    //      final String wqSource = Format.sprintf( "Station: %s (%s)", new Object[]
    //      {
    //          m_tsinfo.getWiskiStationName(),
    //          m_tsinfo.getWiskiStationNo() } );
    //      return new WQTableSetPlusSource( wqsFromStation, wqSource );
    //    }

    return null;
  }

  private WQTableSet fetchWQTable( final String type, final Long id, final Date from, final Date to, final String fromType, final String toType )
  {
    //    LOG.info( "Calling getRatingTables() with validity= " + to + " type= " + type + " ts-id= " + id );

    try
    {
      final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
      final GetRatingTables call = new GetRatingTables( id, to, type, from );
      rep.executeWiskiCall( call );
      final WQTable table = call.getTable();
      if( table == null )
        return null;

      return new WQTableSet( new WQTable[]
      { table }, fromType, toType );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * Helper for translating wiski alarm levels into kalypso metadata
   * <p>
   * Alarmlevels are "attached" to the timeserie, meaning that the unit of the alarmlevels is the same as the unit of
   * the timeserie
   */
  private void fetchAlarmLevels( final MetadataList md ) throws NumberFormatException, RemoteException, KiWWException, RepositoryException
  {
    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
    final GetAlarmLevelList call = new GetAlarmLevelList( m_tsinfo.getWiskiId() );
    rep.executeWiskiCall( call );
    if( call.getAlarmList() != null )
    {
      for( final Iterator it = call.getAlarmList().iterator(); it.hasNext(); )
      {
        // Beispiel: {epv_remark=null, epv_value=90, epv_id=513866742, epv_name=Alarmstufe_1, epv_type=0}
        final HashMap map = (HashMap)it.next();

        final String level = (String)map.get( "epv_name" );
        final String value = (String)map.get( "epv_value" );

        try
        {
          md.setProperty( WiskiUtils.wiskiMetadataName2Kalypso( level ), value );
        }
        catch( final IllegalArgumentException e )
        {
          LOG.warning( "Unbekanntes Alarmlevel von WISKI erhalten: " + level );

          // set the property without Kalyso-Meaning, even if not recognized
          md.setProperty( level, value );
        }
      }
    }
  }

  /**
   * Helper for getting metadata of the corresponding station
   */
  private void fetchStationMetadata( final MetadataList md ) throws NumberFormatException, RemoteException, KiWWException, RepositoryException
  {
    final WiskiRepository rep = (WiskiRepository)m_tsinfo.getRepository();
    final GetStationDetailList call = new GetStationDetailList( m_tsinfo.getWiskiStationId() );
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

  /**
   * Helper class: combined returned type WQTableSet + Source-Description
   */
  private final static class WQTableSetPlusSource
  {
    public final String source;
    public final WQTableSet wqs;

    WQTableSetPlusSource( final WQTableSet wqs, final String source )
    {
      this.source = source;
      this.wqs = wqs;
    }
  }
}
