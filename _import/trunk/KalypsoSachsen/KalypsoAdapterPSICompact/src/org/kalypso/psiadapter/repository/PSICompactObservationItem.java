package org.kalypso.psiadapter.repository;

import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ArchiveData;
import de.psi.go.lhwz.PSICompact.ObjectMetaData;
import de.psi.go.lhwz.PSICompact.WQParamSet;

/**
 * Eine Observation aus PSICompact welche auch ein Repository Item ist.
 * 
 * @author schlienger
 */
public class PSICompactObservationItem extends PSICompactItem
{
  private final int m_valueType;

  /** Metadaten aus PSICompact */
  private ObjectMetaData m_psicMetaData = null;

  private WQParamSet[] m_psicWQParamSet = null;

  /** Metadaten f�r die Observation */
  private MetadataList m_metadata = null;

  private IAxis[] m_axes = null;

  public final static String MD_WQ = "WQ-Parameter";

  public final static String MD_GKR = "Rechtswert";

  public final static String MD_GKH = "Hochwert";

  public final static String MD_ALARM_1 = "Alarmstufe 1";

  public final static String MD_ALARM_2 = "Alarmstufe 2";

  public final static String MD_ALARM_3 = "Alarmstufe 3";

  public final static String MD_ALARM_4 = "Alarmstufe 4";

  public final static String MD_PEGELNULLPUNKT = "Pegelnullpunkt";

  public final static String MD_HOEHENANGABEART = "H�henangabeart";

  public final static String MD_MESSTISCHBLATT = "Messtischblattnummer";
  
  public final static String MD_FLUSSGEBIET = "Flussgebiet";
  
  public final static String MD_FLUSS = "Fluss";

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
   * @see org.kalypso.psiadapter.repository.PSICompactItem#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    if( anotherClass == IObservation.class )
      return this;
        
    return super.getAdapter( anotherClass );
  }
  
  /**
   * Helper f�r die Erzeugung der Metadaten
   */
  private void constructMetadata()
  {
    m_metadata = new MetadataList();

    m_metadata.put( MetadataList.MD_NAME, getName() );
    m_metadata.put( MetadataList.MD_DESCRIPTION, m_objectInfo.getDescription() );

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
      m_metadata.put( MD_FLUSS, m_psicMetaData.getRiver() );
      m_metadata.put( MD_FLUSSGEBIET, m_psicMetaData.getRiversystem() );
    }

    if( m_psicWQParamSet != null )
      m_metadata.put( MD_WQ, PSICompactFactory.wqParamSet2String( m_psicWQParamSet ) );
  }
  
  /**
   * Gibt das Messwerttyp dieser Zeitreihe zur�ck
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
   * Gibt das Werttyp dieser Zeitreihe zur�ck
   */
  private String valueTypeToString()
  {
    return PSICompactFactory.valueTypeToString( m_valueType );
  }

  /**
   * @see org.kalypso.psiadapter.repository.PSICompactItem#toString()
   */
  public String toString()
  {
    return super.toString() + " (" + measureTypeToString() + ") " + valueTypeToString();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget()
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
   * TODO: explain why synchronized (see ZmlObservation)
   * 
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public synchronized IAxis[] getAxisList()
  {
    if( m_axes == null )
    {
      m_axes = new IAxis[3];

      // immer Datum Axis
      m_axes[0] = new DefaultAxis( "Datum", "datum", "", Date.class, 0, true ); 

      // Wert (Einheit abfragen)
      String label = toString();
      String unit = PSICompactFactory.unitToString( m_psicMetaData.getUnit() );
      m_axes[1] = new DefaultAxis( label, "pegel", unit, Double.class, 1, false );

      // PSI-Status
      //m_axes[2] = PSICompactFactory.getAxis( "Status", "", "", String.class, 2 );
      
      // Status
      m_axes[2] = new DefaultAxis( KalypsoStatusUtils.getStatusAxisLabelFor( m_axes[1] ), "kalypso_status", "", Integer.class, 2, false );
    }

    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public synchronized ITuppleModel getValues( final IVariableArguments args ) throws SensorException
  {
    if( !( args instanceof DateRangeArgument ) )
      throw new SensorException( "Brauche DateRange as Argument. Kann sonst die PSICompact Schnittstelle nicht abfragen" );
    
    final DateRangeArgument dr = (DateRangeArgument)args;
    
    if( m_values != null && dr.getFrom().compareTo( m_from ) == 0 && dr.getTo().compareTo( m_to ) == 0 )
      return m_values;
    
    try
    {
      m_from = dr.getFrom();
      m_to = dr.getTo();

      final ArchiveData[] data = PSICompactFactory.getConnection().getArchiveData( m_objectInfo.getId(),
          PSICompact.ARC_MIN15, m_from, m_to );

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
      model = (PSICompactTuppleModel)values;
    else
      model = PSICompactTuppleModel.copyModel( values );
    
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

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    // only editable when it represents a forecast
    return m_valueType == PSICompact.TYPE_VALUE;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_objectInfo.getId();
  }
}