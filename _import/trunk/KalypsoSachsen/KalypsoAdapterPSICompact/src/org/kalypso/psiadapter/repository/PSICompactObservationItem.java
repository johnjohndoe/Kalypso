package org.kalypso.psiadapter.repository;

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
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.util.conversion.units.IValueConverter;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;

/**
 * Eine Observation aus PSICompact welche auch ein Repository Item ist.
 * 
 * @author schlienger
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

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter(
      this );

  /**
   * Constructor
   * 
   * @param name
   * @param id
   * @param info
   * @param valueType
   *          aus PSICompact Sicht (type 'measurement' or type 'value')
   * 
   * @throws ECommException
   */
  public PSICompactObservationItem( final String name, final String id,
      final PSICompact.ObjectInfo info, final int valueType )
      throws ECommException
  {
    m_name = name;
    m_identifier = id;
    m_objectInfo = info;
    m_valueType = valueType;

    final PSICompact.ObjectMetaData psiMD = PSICompactFactory.getConnection()
        .getObjectMetaData( m_objectInfo.getId() );

    final PSICompact.WQParamSet[] psiWQ = PSICompactFactory.getConnection().getWQParams(
        m_objectInfo.getId() );

    m_axes = prepareAxes( psiMD );

    m_metadata = prepareMetadata( psiMD, psiWQ );
  }

  /**
   * @param psiMD
   * @return axis list
   */
  private IAxis[] prepareAxes( PSICompact.ObjectMetaData psiMD )
  {
    final IAxis[] axes = new IAxis[3];

    // immer Datum Axis
    axes[0] = new DefaultAxis( TimeserieUtils
        .getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE,
        TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class,
        true );

    // Wert (Einheit abfragen)
    final int psiUnit = psiMD.getUnit();
    final String type = measureTypeToString();
    final String unit = TimeserieUtils.getUnit( type );

    final String label = TimeserieUtils.getName( type ) + " " + getName();

    axes[1] = new DefaultAxis( label, type, unit, Double.class, false );

    m_vc = PSICompactRepositoryFactory.getConverter( psiUnit, unit );

    // Status
    axes[2] = KalypsoStatusUtils.createStatusAxisFor( axes[1] );

    return axes;
  }

  /**
   * Helper für die Erzeugung der Metadaten
   * 
   * @param psiMD
   * @param psiWQ
   * @return metadata
   * 
   * @throws ECommException
   */
  private final MetadataList prepareMetadata(
      final PSICompact.ObjectMetaData psiMD, final PSICompact.WQParamSet[] psiWQ )
      throws ECommException
  {
    final MetadataList metadata = new MetadataList();

    metadata.put( ObservationConstants.MD_NAME, getName() );
    metadata.put( ObservationConstants.MD_DESCRIPTION, m_objectInfo
        .getDescription() );

    if( psiMD != null )
    {
      metadata.put( TimeserieConstants.MD_GKH, String.valueOf( psiMD
          .getHeight() ) );
      metadata.put( TimeserieConstants.MD_GKR, String
          .valueOf( psiMD.getRight() ) );
      metadata
          .put( TimeserieConstants.MD_HOEHENANGABEART, psiMD.getLevelUnit() );
      metadata.put( TimeserieConstants.MD_PEGELNULLPUNKT, String.valueOf( psiMD
          .getLevel() ) );
      metadata.put( TimeserieConstants.MD_MESSTISCHBLATT, String.valueOf( psiMD
          .getMapNo() ) );

      // Bug 80: only waterlevel-timeseries should have alarmlevels
      if( TimeserieConstants.TYPE_WATERLEVEL.equals( measureTypeToString() ) )
      {
        metadata.put( TimeserieConstants.MD_ALARM_1, String.valueOf( m_vc
            .convert( psiMD.getAlarm1() ) ) );
        metadata.put( TimeserieConstants.MD_ALARM_2, String.valueOf( m_vc
            .convert( psiMD.getAlarm2() ) ) );
        metadata.put( TimeserieConstants.MD_ALARM_3, String.valueOf( m_vc
            .convert( psiMD.getAlarm3() ) ) );
        metadata.put( TimeserieConstants.MD_ALARM_4, String.valueOf( m_vc
            .convert( psiMD.getAlarm4() ) ) );
      }

      metadata.put( TimeserieConstants.MD_FLUSS, psiMD.getRiver() );
      metadata.put( TimeserieConstants.MD_FLUSSGEBIET, psiMD.getRiversystem() );
    }

    try
    {
      if( psiWQ != null )
      {
        final WechmannGroup group = PSICompactRepositoryFactory
            .readWQParams( psiWQ );
        final String xml = WechmannFactory.createXMLString( group );

        metadata.put( TimeserieConstants.MD_WQWECHMANN, xml );
      }
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();

      throw new ECommException( e );
    }

    return metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_identifier;
  }

  /**
   * @return Messwerttyp dieser Zeitreihe (Siehe TimeserieConstants.TYPE_*)
   */
  private String measureTypeToString( )
  {
    int measType = PSICompact.MEAS_UNDEF;

    try
    {
      measType = PSICompactFactory.getConnection().getMeasureType(
          m_objectInfo.getId() );
    }
    catch( ECommException e )
    {
      e.printStackTrace();
    }

    return PSICompactRepositoryFactory.measureTypeToString( measType );
  }

  /**
   * @return welche Archivtyp benutzt werden soll
   */
  private int measureTypeToArchiveType( )
  {
    int measType = PSICompact.MEAS_UNDEF;

    try
    {
      measType = PSICompactFactory.getConnection().getMeasureType(
          m_objectInfo.getId() );
    }
    catch( ECommException e )
    {
      e.printStackTrace();
    }

    return PSICompactRepositoryFactory.measureTypeToArchiveType( measType );
  }

  /**
   * @return das Werttyp dieser Zeitreihe
   */
  private String valueTypeToString( )
  {
    return PSICompactRepositoryFactory.valueTypeToString( m_valueType );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return getName() + " (" + measureTypeToString() + ") "
        + valueTypeToString();
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

    // tricky: when no date range specified, we create a default one
    // according to the config delivered by our PSICompactFactory
    if( args == null || !(args instanceof DateRangeArgument) )
      dr = DateRangeArgument.createFromPastDays( Integer.valueOf(
          PSICompactFactory.getProperties().getProperty( "NUMBER_OF_DAYS",
              "100" ) ).intValue() );
    else
      dr = (DateRangeArgument) args;

    if( m_values != null && dr.getFrom().compareTo( m_from ) == 0
        && dr.getTo().compareTo( m_to ) == 0 )
      return m_values;

    try
    {
      m_from = dr.getFrom();
      m_to = dr.getTo();

      final PSICompact.ArchiveData[] data = PSICompactFactory.getConnection()
          .getArchiveData( m_objectInfo.getId(), measureTypeToArchiveType(),
              m_from, m_to );

      m_values = new PSICompactTuppleModel( data, getAxisList(), m_vc );
      return m_values;
    }
    catch( ECommException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    PSICompactTuppleModel model = null;

    if( values instanceof PSICompactTuppleModel )
      model = (PSICompactTuppleModel) values;
    else
      model = PSICompactTuppleModel.copyModel( values, m_vc );

    if( model.getCount() > 0 )
    {
      try
      {
        PSICompactFactory.getConnection().setArchiveData( m_objectInfo.getId(),
            measureTypeToArchiveType(), model.getData()[0].getTimestamp(),
            model.getData() );

        // this observation has changed
        m_evtPrv.fireChangedEvent();
      }
      catch( ECommException e )
      {
        throw new SensorException( e );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
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
  public void clearListeners( )
  {
    m_evtPrv.clearListeners();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    return null;
  }
}