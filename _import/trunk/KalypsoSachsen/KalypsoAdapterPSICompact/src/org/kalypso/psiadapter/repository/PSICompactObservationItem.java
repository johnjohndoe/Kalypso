package org.kalypso.psiadapter.repository;

import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ArchiveData;
import de.psi.go.lhwz.PSICompact.ObjectInfo;
import de.psi.go.lhwz.PSICompact.ObjectMetaData;
import de.psi.go.lhwz.PSICompact.WQData;
import de.psi.go.lhwz.PSICompact.WQParamSet;

/**
 * Eine Observation aus PSICompact welche auch ein Repository Item ist.
 * 
 * TODO: SEHR WICHTIG: EINHEITEN RICHTIG BEHANDELN. Je nach Axistyp
 * sollte ich die PSI Werte dann in die Kalypso Einheit konvertieren.
 * 
 * @author schlienger
 */
public class PSICompactObservationItem implements IObservation
{
  private final String m_name;

  private final String m_identifier;

  private final ObjectInfo m_objectInfo;

  private final int m_valueType;

  private IAxis[] m_axes = null;
  
  /** Metadaten aus PSICompact */
  private ObjectMetaData m_psicMetaData = null;

  /** Metadaten für die Observation */
  private MetadataList m_metadata = null;
  
  private WQParamSet[] m_psicWQParamSet = null;

  // used for caching
  private ITuppleModel m_values = null;
  private Date m_from = null;
  private Date m_to = null;
  
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

    m_psicMetaData = PSICompactFactory.getConnection().getObjectMetaData(
        m_objectInfo.getId() );

    m_psicWQParamSet = PSICompactFactory.getConnection().getWQParams(
        m_objectInfo.getId() );

    constructMetadata();
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
   * Helper für die Erzeugung der Metadaten
   * 
   * @throws ECommException
   */
  private final void constructMetadata( ) throws ECommException
  {
    m_metadata = new MetadataList();

    m_metadata.put( ObservationConstants.MD_NAME, getName() );
    m_metadata.put( ObservationConstants.MD_DESCRIPTION, m_objectInfo
        .getDescription() );

    if( m_psicMetaData != null )
    {
      m_metadata.put( TimeserieConstants.MD_GKH, String.valueOf( m_psicMetaData
          .getHeight() ) );
      m_metadata.put( TimeserieConstants.MD_GKR, String.valueOf( m_psicMetaData
          .getRight() ) );
      m_metadata.put( TimeserieConstants.MD_HOEHENANGABEART, m_psicMetaData
          .getLevelUnit() );
      m_metadata.put( TimeserieConstants.MD_PEGELNULLPUNKT, String
          .valueOf( m_psicMetaData.getLevel() ) );
      m_metadata.put( TimeserieConstants.MD_MESSTISCHBLATT, String
          .valueOf( m_psicMetaData.getMapNo() ) );
      m_metadata.put( TimeserieConstants.MD_ALARM_1, String
          .valueOf( m_psicMetaData.getAlarm1() ) );
      m_metadata.put( TimeserieConstants.MD_ALARM_2, String
          .valueOf( m_psicMetaData.getAlarm2() ) );
      m_metadata.put( TimeserieConstants.MD_ALARM_3, String
          .valueOf( m_psicMetaData.getAlarm3() ) );
      m_metadata.put( TimeserieConstants.MD_ALARM_4, String
          .valueOf( m_psicMetaData.getAlarm4() ) );
      m_metadata.put( TimeserieConstants.MD_FLUSS, m_psicMetaData.getRiver() );
      m_metadata.put( TimeserieConstants.MD_FLUSSGEBIET, m_psicMetaData
          .getRiversystem() );
    }

    try
    {
      if( m_psicWQParamSet != null )
      {
        final WechmannGroup group = readWQParams( m_psicWQParamSet );
        final String xml = WechmannFactory.createXMLString( group );
        
        m_metadata.put( TimeserieConstants.MD_WQ, xml );
      }
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();

      throw new ECommException( e );
    }
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
    if( m_axes == null )
    {
      m_axes = new IAxis[3];

      // immer Datum Axis
      m_axes[0] = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "",
          Date.class, 0, true );

      // Wert (Einheit abfragen)
      String label = toString();
      String unit = PSICompactRepositoryFactory.unitToString( m_psicMetaData
          .getUnit() );
      m_axes[1] = new DefaultAxis( label, measureTypeToString(), unit,
          Double.class, 1, false );

      // PSI-Status
      //m_axes[2] = PSICompactFactory.getAxis( "Status", "", "", String.class,
      // 2 );

      // Status
      m_axes[2] = new DefaultAxis( KalypsoStatusUtils
          .getStatusAxisLabelFor( m_axes[1] ), "kalypso_status", "",
          Integer.class, 2, false );
    }

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
    if( args == null || !(args instanceof DateRangeArgument))
      dr = DateRangeArgument.createFromPastDays( Integer.valueOf( PSICompactFactory.getProperties().getProperty( "NUMBER_OF_DAYS", "100" ) ).intValue() );
    else
      dr = (DateRangeArgument) args;

    if( m_values != null && dr.getFrom().compareTo( m_from ) == 0
        && dr.getTo().compareTo( m_to ) == 0 )
      return m_values;

    try
    {
      m_from = dr.getFrom();
      m_to = dr.getTo();

      final ArchiveData[] data = PSICompactFactory.getConnection()
          .getArchiveData( m_objectInfo.getId(), measureTypeToArchiveType(), m_from,
              m_to );

      m_values = new PSICompactTuppleModel( data, getAxisList() );
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
      model = PSICompactTuppleModel.copyModel( values );

    if( model.getCount() > 0 )
    {
      try
      {
        PSICompactFactory.getConnection().setArchiveData( m_objectInfo.getId(),
            measureTypeToArchiveType(), model.getData()[0].getTimestamp(),
            model.getData() );
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
   * Helper that converts PSICompact WQParamSet objects to a WechmannSets
   * object.
   * 
   * @param pset
   * @return WechmannGroup constructed from the WQParamSet array
   * 
   */
  public static WechmannGroup readWQParams( final WQParamSet[] pset )
  {
    final WechmannSet[] wsets = new WechmannSet[pset.length];
    for( int i = 0; i < pset.length; i++ )
    {
      final WQData[] ds = pset[i].getWqData();
      final WechmannParams[] wps = new WechmannParams[ds.length];

      for( int j = 0; j < ds.length; j++ )
        wps[j] = new WechmannParams( ds[j].getW1(), ds[j].getLNK1(), ds[j]
            .getK2(), ds[j].getWGR() );

      wsets[i] = new WechmannSet( pset[i].getValidFrom(), wps );
    }

    return new WechmannGroup( wsets );
  }
}