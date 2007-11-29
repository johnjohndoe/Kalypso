package org.kalypso.psiadapter.repository;

import java.util.Calendar;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.kalypso.commons.conversion.units.IValueConverter;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.psiadapter.PSICompactFactory;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ArchiveData;
import de.psi.go.lhwz.PSICompact.ObjectMetaData;

/**
 * Eine Observation aus PSICompact welche auch ein Repository Item ist.
 * 
 * <ol>
 * <li>20060124 schlienger Datenüberschreibungsproblem: jetzt werden auch negativen Werte zurückgeschrieben
 * </ol>
 * 
 * @author Marc Schlienger
 */
public class PSICompactObservationItem implements IObservation
{
  private final String m_name;

  private final String m_identifier;

  private final PSICompact.ObjectInfo m_objectInfo;

  private final int m_valueType;

  private final IAxis[] m_axes;

  private final MetadataList m_metadata;

  // used for caching
  private ITuppleModel m_values = null;

  private Date m_from = null;

  private Date m_to = null;

  private IValueConverter m_vc;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter( this );

  private final ObjectMetaData m_objectMetaData;

  private final int m_arcType;

  /**
   * Constructor
   * 
   * @param valueType
   *          aus PSICompact Sicht (type 'measurement' or type 'value')
   * @param metaData
   *          The object metadata as, got from {@link PSICompact#getObjectMetaData(java.lang.String)}for the
   *          <code>identifier</code>
   * @param arcType
   *          One of the {@link PSICompact#ARC_DAY}constants.
   */
  public PSICompactObservationItem( final String name, final String id, final PSICompact.ObjectInfo info,
      final int valueType, final ObjectMetaData metaData, final int arcType ) throws ECommException
  {
    m_name = name;
    m_identifier = id;
    m_objectInfo = info;
    m_valueType = valueType;
    m_objectMetaData = metaData;
    m_arcType = arcType;

    final PSICompact.WQParamSet[] psiWQ = PSICompactFactory.getConnection().getWQParams( m_objectInfo.getId() );

    m_axes = prepareAxes();

    m_metadata = prepareMetadata( psiWQ );
  }

  /**
   * @return axis list
   */
  private IAxis[] prepareAxes()
  {
    final IAxis[] axes = new IAxis[3];

    // immer Datum Axis
    axes[0] = new DefaultAxis( TimeserieUtils.getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE,
        TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class, true );

    // Wert (Einheit abfragen)
    final int psiUnit = m_objectMetaData.getUnit();
    final String type = measureTypeToString();
    final String unit = TimeserieUtils.getUnit( type );

    final String label = TimeserieUtils.getName( type ) + " " + getName();

    axes[1] = new DefaultAxis( label, type, unit, Double.class, false );

    m_vc = PSICompactRepositoryFactory.getConverter( psiUnit, unit );

    // Status
    axes[2] = KalypsoStatusUtils.createStatusAxisFor( axes[1], true );

    return axes;
  }

  /**
   * Helper für die Erzeugung der Metadaten
   */
  private final MetadataList prepareMetadata( final PSICompact.WQParamSet[] psiWQ ) throws ECommException
  {
    final MetadataList metadata = new MetadataList();

    // TODO: 'name' of ObjectMetaData is never used, why?

    metadata.put( ObservationConstants.MD_NAME, getName() );
    metadata.put( ObservationConstants.MD_DESCRIPTION, m_objectInfo.getDescription() );
    metadata.put( ObservationConstants.MD_ORIGIN, "PSICompact" );

    if( m_objectMetaData != null )
    {
      final String gkr = String.valueOf( m_objectMetaData.getRight() );
      metadata.put( TimeserieConstants.MD_GKR, gkr );
      metadata.put( TimeserieConstants.MD_COORDSYS, TimeserieUtils.getCoordinateSystemNameForGkr( gkr ) );
      metadata.put( TimeserieConstants.MD_GKH, String.valueOf( m_objectMetaData.getHeight() ) );

      metadata.put( TimeserieConstants.MD_HOEHENANGABEART, m_objectMetaData.getLevelUnit() );
      metadata.put( TimeserieConstants.MD_PEGELNULLPUNKT, String.valueOf( m_objectMetaData.getLevel() ) );
      metadata.put( TimeserieConstants.MD_MESSTISCHBLATT, String.valueOf( m_objectMetaData.getMapNo() ) );

      // Bug 80: only waterlevel-timeseries should have alarmlevels
      if( TimeserieConstants.TYPE_WATERLEVEL.equals( measureTypeToString() ) )
      {
        metadata.put( TimeserieConstants.MD_ALARM_1, String.valueOf( m_vc.convert( m_objectMetaData.getAlarm1() ) ) );
        metadata.put( TimeserieConstants.MD_ALARM_2, String.valueOf( m_vc.convert( m_objectMetaData.getAlarm2() ) ) );
        metadata.put( TimeserieConstants.MD_ALARM_3, String.valueOf( m_vc.convert( m_objectMetaData.getAlarm3() ) ) );
        metadata.put( TimeserieConstants.MD_ALARM_4, String.valueOf( m_vc.convert( m_objectMetaData.getAlarm4() ) ) );
      }

      final String unitLabel = PSICompactUtilitites.getLabelForUnit( m_objectMetaData.getUnit() );
      metadata.put( "Einheit in PSI-Compact", unitLabel );
      metadata.put( "Archivtyp", PSICompactUtilitites.getLabelForArcType( m_arcType ) );
      final int[] archiveData = m_objectMetaData.getArchiveData();
      final String[] archiveStrings = new String[archiveData.length];
      for( int i = 0; i < archiveData.length; i++ )
        archiveStrings[i] = PSICompactUtilitites.getLabelForArcType( archiveData[i] );
      metadata.put( "vorhandene Archivetypen", Arrays.implode( archiveStrings, ", ", 0, archiveStrings.length - 1 ) );

      metadata.put( TimeserieConstants.MD_GEWAESSER, m_objectMetaData.getRiver() );
      metadata.put( TimeserieConstants.MD_FLUSSGEBIET, m_objectMetaData.getRiversystem() );
    }

    try
    {
      if( psiWQ != null )
      {
        final WechmannGroup group = PSICompactRepositoryFactory.readWQParams( psiWQ );
        final String xml = WechmannFactory.createXMLString( group );

        metadata.put( TimeserieConstants.MD_WQWECHMANN, xml );
      }
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();

      throw new ECommException( e );
    }

    return metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_identifier;
  }

  /**
   * TODO: this is called too often, probably once in the constructor would be enough!
   * 
   * @return Messwerttyp dieser Zeitreihe (Siehe TimeserieConstants.TYPE_*)
   */
  private String measureTypeToString()
  {
    int measType = PSICompact.MEAS_UNDEF;

    try
    {
      measType = PSICompactFactory.getConnection().getMeasureType( m_objectInfo.getId() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return PSICompactRepositoryFactory.measureTypeToString( measType );
  }

  /**
   * @return das Werttyp dieser Zeitreihe
   */
  private String valueTypeToString()
  {
    return PSICompactRepositoryFactory.valueTypeToString( m_valueType );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getName() + " (" + measureTypeToString() + ") " + valueTypeToString();
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
  public ITuppleModel getValues( final IRequest request ) throws SensorException
  {
    final DateRange dr;

    // tricky: when no date range specified, we create a default one
    // according to the config delivered by our PSICompactFactory
    if( request == null || request.getDateRange() == null )
      dr = DateRange.createFromPastDays( Integer.valueOf(
          PSICompactFactory.getProperties().getProperty( "NUMBER_OF_DAYS", "100" ) ).intValue() );
    else
      dr = request.getDateRange();

    final Date drFrom = dr.getFrom();
    final Date drTo = dr.getTo();
    
    if( m_values != null && drFrom.compareTo( m_from ) == 0 && drTo.compareTo( m_to ) == 0 )
      return m_values;

    try
    {
      m_from = drFrom;
      m_to = drTo;

      final PSICompact.ArchiveData[] data = PSICompactFactory.getConnection().getArchiveData( m_objectInfo.getId(),
          m_arcType, m_from, m_to );

      m_values = new PSICompactTuppleModel( data, getAxisList(), m_vc );
      return m_values;
    }
    catch( final Exception e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel model ) throws SensorException
  {
    if( model.getCount() > 0 )
    {
      final ArchiveData[] data = extendForOverwrite( model );

      // TODO: only write if debug mode is on
      dumpArchiveData( data );

      try
      {
        PSICompactFactory.getConnection()
            .setArchiveData( m_objectInfo.getId(), m_arcType, data[0].getTimestamp(), data );

        // this observation has changed
        m_evtPrv.fireChangedEvent( null );
      }
      catch( final Exception e )
      {
        throw new SensorException( e );
      }
    }
  }

  /**
   * Used for debug purposes
   */
  private void dumpArchiveData( final ArchiveData[] data )
  {
    final StringBuffer msg = new StringBuffer();

    msg.append( "Werte nach PSICompact geschrieben in Zeitreihe mit ID: " );
    msg.append( m_objectInfo.getId() );
    msg.append( '\n' );

    for( int i = 0; i < data.length; i++ )
    {
      final PSICompact.ArchiveData dataItem = data[i];
      msg.append( dataItem.getTimestamp() );
      msg.append( " - " );
      msg.append( dataItem.getValue() );
      msg.append( " - " );
      msg.append( dataItem.getStatus() );
      msg.append( '\n' );
    }

    msg.append( '\n' );
    msg.append( '\n' );

    Logger.getLogger( getClass().getName() ).info( msg.toString() );
  }

  /**
   * Interpolates and extends the model data according to configuration.
   * <p>
   * Extending the data is needed to really overwrite the contents of psi compact, else only the new values get written
   * into the container.
   * 
   * @throws SensorException
   */
  private ArchiveData[] extendForOverwrite( final ITuppleModel model ) throws SensorException
  {
    final IAxis[] axes = model.getAxisList();

    final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );
    // REMARK: when Observations get written to the server, it is ensured that exactly the known axes are written out.
    // this is done by comparing the target observation with the source observation and only copying what is needed.
    // In case of PSICompact, this means we will have exactly one axis, so no problem here...
    final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( axes, Number.class, true );
    //    final IAxis statusAxis = KalypsoStatusUtils.findStatusAxisFor( axes, valueAxis );

    final IAxisRange dateRange = model.getRangeFor( dateAxis );

    /* Get defined properties */
    final int overwriteCalendarField = PSICompactFactory.getOverwriteCalendarField();

    // TODO: remove obsolete properties from config.ini
    /*
     * final int overwriteStep = PSICompactFactory.getOverwriteStep(); final double overwriteValue = m_vc.convert(
     * PSICompactFactory.getOverwriteValue() );
     */

    final int overwriteAmountBefore = PSICompactFactory.getOverwriteAmountBefore();
    final int overwriteAmountAfter = PSICompactFactory.getOverwriteAmountAfter();

    /* Determine start and end */
    final Calendar cal = PSICompactFactory.getCalendarForPSICompact();
    cal.setTime( (Date)dateRange.getLower() );
    cal.add( overwriteCalendarField, -overwriteAmountBefore );
    final Date begin = cal.getTime();

    cal.setTime( (Date)dateRange.getUpper() );
    cal.add( overwriteCalendarField, overwriteAmountAfter );
    final Date end = cal.getTime();

    /* Beware of endles loop */
    if( !begin.before( end ) )
      throw new SensorException( "Failed to determine date-interval for timeserie" );

    /* Create data with overwrite-values for the full time intervall */
    final double fillValue = m_vc.reverse( -1.0 ); // TODO: check if always correct
    final SortedMap fullIntervalMap = createExtendedArchiveDataMap( begin, end, PSICompact.STATUS_AUTO, fillValue );

    /* Overwrite the full interval with values from timeserie to write out */
    final int modelCount = model.getCount();
    for( int i = 0; i < modelCount; i++ )
    {
      final Date date = (Date)model.getElement( i, dateAxis );
      final int status = PSICompact.STATUS_AUTO; // TODO: why do we always use 'AUTO' instead of the given status?
      final double value = m_vc.reverse( ( (Number)model.getElement( i, valueAxis ) ).doubleValue() );

      final ArchiveData archiveData = new ArchiveData( date, status, value );
      fullIntervalMap.put( date, archiveData );
    }

    return (ArchiveData[])fullIntervalMap.values().toArray( new ArchiveData[fullIntervalMap.size()] );
  }

  /**
   * Creates a SorteMap: Date -> ArchiveData for all dates in a given interval.
   * <p>
   * All achive datas get the same status and value.
   * 
   * @param begin
   *          Start of the date interval
   * @param end
   *          End of the date interval
   * @param status
   *          The constant status for all achive datas.
   * @param value
   *          The constant value for all archive datas.
   * @throws SensorException
   */
  private SortedMap createExtendedArchiveDataMap( final Date begin, final Date end, final int status, final double value )
      throws SensorException
  {
    final int stepAmount = PSICompactUtilitites.arcTypeToCalendarAmount( m_arcType );
    final int stepField = PSICompactUtilitites.arcTypeToCalendarField( m_arcType );

    // TODO: how do we make sure that the start time fits to the PSICompact database raster of values?
    final Calendar stepper = Calendar.getInstance();
    stepper.setTime( begin );

    final SortedMap archiveDataMap = new TreeMap();

    /* Make sure we do not get an endless loop */
    if( !begin.before( end ) )
      return archiveDataMap;

    while( true )
    {
      final Date stepTime = stepper.getTime();

      if( stepTime.after( end ) )
        break;

      final ArchiveData archiveData = new ArchiveData( stepTime, status, value );

      archiveDataMap.put( stepTime, archiveData );

      stepper.add( stepField, stepAmount );
    }

    return archiveDataMap;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    // only editable when it represents a forecast
    return m_valueType == PSICompact.TYPE_VALUE;
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
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref()
  {
    return null;
  }
}