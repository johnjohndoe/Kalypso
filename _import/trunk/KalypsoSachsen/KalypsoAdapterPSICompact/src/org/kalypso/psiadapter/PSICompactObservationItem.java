package org.kalypso.psiadapter;

import java.util.Date;
import java.util.List;
import java.util.Vector;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.Metadata;
import org.kalypso.ogc.sensor.SensorException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.*;

/**
 * Eine Observation aus PSICompact was auch ein Repository Item ist.
 * 
 * @author schlienger
 */
public class PSICompactObservationItem extends PSICompactItem implements IObservation
{
  private final ObjectInfo m_objectInfo;

  private final int m_valueType;

  /** Metadaten aus PSICompact */
  private ObjectMetaData m_psicMetaData = null;

  private WQParamSet[] m_psicWQParamSet = null;

  /** Metadaten für die Observation */
  private Metadata m_metadata = null;

  private List m_axes = null;

  public final static String MD_WQ = "WQ-Parameter";

  public final static String MD_GKR = "Rechtswert";

  public final static String MD_GKH = "Hochwert";

  public final static String MD_ALARM_1 = "Alarmstufe 1";

  public final static String MD_ALARM_2 = "Alarmstufe 2";

  public final static String MD_ALARM_3 = "Alarmstufe 3";

  public final static String MD_ALARM_4 = "Alarmstufe 4";

  public final static String MD_PEGELNULLPUNKT = "Pegelnullpunkt";

  public final static String MD_HOEHENANGABEART = "Höhenangabeart";

  public final static String MD_MESSTISCHBLATT = "Messtischblattnummer";

  // used for caching
  private ITuppleModel m_values = null;
  private Date m_from = null;
  private Date m_to = null;

  /**
   * Constructor
   * 
   * @param parent
   *          kann null sein wenn dieses Objekt root ist
   * @param valueType
   *          aus PSICompact Sicht
   */
  public PSICompactObservationItem( final PSICompactItem parent, final String name,
      final PSICompact.ObjectInfo info, final int valueType )
  {
    super( parent, name, info );

    m_objectInfo = info;
    m_valueType = valueType;

    try
    {
      m_psicMetaData = PSICompactFactory.getConnection().getObjectMetaData( m_objectInfo.getId() );

      m_psicWQParamSet = PSICompactFactory.getConnection().getWQParams( m_objectInfo.getId() );

      constructMetadata();
    }
    catch( ECommException e )
    {
      // TODO: logging!
      e.printStackTrace();
    }
  }

  /**
   * Helper für die Erzeugung der Metadaten
   */
  private void constructMetadata()
  {
    m_metadata = new Metadata();

    m_metadata.put( Metadata.MD_NAME, getName() );
    m_metadata.put( Metadata.MD_DESCRIPTION, m_objectInfo.getDescription() );

    if( m_psicMetaData != null )
    {
      m_metadata.put( MD_GKH, String.valueOf( m_psicMetaData.getHeight() ) );
      m_metadata.put( MD_GKR, String.valueOf( m_psicMetaData.getRight() ) );
      m_metadata.put( MD_HOEHENANGABEART, m_psicMetaData.getLevelUnit() );
      m_metadata.put( MD_PEGELNULLPUNKT, String.valueOf( m_psicMetaData.getLevel() ) );
      m_metadata.put( MD_MESSTISCHBLATT, String.valueOf( m_psicMetaData.getMapNo() ) );
      m_metadata.put( MD_ALARM_1, String.valueOf( m_psicMetaData.getAlarm1() ) );
      m_metadata.put( MD_ALARM_2, String.valueOf( m_psicMetaData.getAlarm2() ) );
      m_metadata.put( MD_ALARM_3, String.valueOf( m_psicMetaData.getAlarm3() ) );
      m_metadata.put( MD_ALARM_4, String.valueOf( m_psicMetaData.getAlarm4() ) );
    }

    if( m_psicWQParamSet != null )
    {
      m_metadata.put( MD_WQ, m_psicWQParamSet.toString() );
    }
  }

  /**
   * Gibt das Messwerttyp dieser Zeitreihe zurück
   */
  private String measureTypeToString()
  {
    int measType = -1;

    try
    {
      measType = PSICompactFactory.getConnection().getMeasureType( m_objectInfo.getId() );
    }
    catch( ECommException e )
    {
      e.printStackTrace();
    }

    return PSICompactFactory.measureTypeToString( measType );
  }

  /**
   * Gibt das Werttyp dieser Zeitreihe zurück
   */
  private String valueTypeToString()
  {
    return PSICompactFactory.valueTypeToString( m_valueType );
  }

  /**
   * @see org.kalypso.psiadapter.PSICompactItem#toString()
   */
  public String toString()
  {
    return super.toString() + " (" + measureTypeToString() + ") " + valueTypeToString();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public ITarget getTarget()
  {
    return (ITarget)getParent();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadata()
   */
  public Metadata getMetadata()
  {
    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public List getAxisList()
  {
    if( m_axes == null )
    {
      m_axes = new Vector();

      // immer Datum Axis
      m_axes.add( PSICompactFactory.getAxis( "Datum", "", Date.class, 0 ) );

      // Wert (Einheit abfragen)
      String label = toString();
      String unit = PSICompactFactory.unitToString( m_psicMetaData.getUnit() );
      m_axes.add( PSICompactFactory.getAxis( label, unit, Double.class, 1 ) );

      // TODO: Status Axis?
    }

    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues( Date, Date )
   */
  public ITuppleModel getValues( final Date from, final Date to ) throws SensorException
  {
    if( m_values != null && from.compareTo( m_from ) == 0 && to.compareTo( m_to ) == 0 )
      return m_values;
    
    try
    {
      ArchiveData[] data = PSICompactFactory.getConnection().getArchiveData( m_objectInfo.getId(),
          PSICompact.ARC_MIN15, from, to );

      m_from = from;
      m_to = to;
      m_values = new PSICompactTuppleModel( data );
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
      model = (PSICompactTuppleModel)values;
    else
      model = new PSICompactTuppleModel( values );
    
    if( model.getCount() > 0 )
    {
      try
      {
        PSICompactFactory.getConnection().setArchiveData( m_objectInfo.getId(), PSICompact.ARC_MIN15,
            model.getData()[0].getTimestamp(), model.getData() );
      }
      catch( ECommException e )
      {
        throw new SensorException( e );
      }
    }
  }
}