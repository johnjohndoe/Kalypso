package org.kalypso.psiadapter;


import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.ogc.sensor.Metadata;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * A specific item representing a timeserie.
 * 
 * @author schlienger
 */
public class PSICompactObservationItem extends PSICompactItem implements IObservation
{
  private final ObjectInfo m_objectInfo;

  private final int m_valueType;

  private Metadata m_metadata = null;
  
  public final static String MD_NAME = "Name";

  public final static String MD_DESCRIPTION = "Beschreibung";

  public final static String MD_WQ = "WQ-Parameter";

  public final static String MD_GKR = "Rechtswert";

  public final static String MD_GKH = "Hochwert";

  public final static String MD_ALARM_1 = "Alarmstufe 1";

  public final static String MD_ALARM_2 = "Alarmstufe 2";

  public final static String MD_ALARM_3 = "Alarmstufe 3";

  public final static String MD_ALARM_4 = "Alarmstufe 4";

  public final static String MD_PEGELNULLPUNKT = "Pegelnullpunkt";

  public final static String MD_HOEHENANGABEART = "Höhenangabeart";

  public PSICompactObservationItem( final PSICompactItem parent, final String name,
      final PSICompact.ObjectInfo info, final int valueType )
  {
    super( parent, name, info );

    m_objectInfo = info;
    m_valueType = valueType;
    
    // TODO: z.Z: ist es simuliert, aber zukunftig aus der PSICompact Schnittstelle lesen
    constructFakeMetadata();
  }

  private void constructFakeMetadata()
  {
    m_metadata = new Metadata();
    m_metadata.put( MD_NAME, getName() );
    m_metadata.put( MD_DESCRIPTION, m_objectInfo.getDescription() );
    m_metadata.put( MD_GKH, "12345678" );
    m_metadata.put( MD_GKR, "12345678" );
    m_metadata.put( MD_HOEHENANGABEART, "ABC" );
    m_metadata.put( MD_PEGELNULLPUNKT, "456,789" );
    m_metadata.put( MD_WQ, "X=12;Y=23;Z=34" );
    m_metadata.put( MD_ALARM_1, "4,5" );
    m_metadata.put( MD_ALARM_2, "5,6" );
    m_metadata.put( MD_ALARM_3, "6,7" );
    m_metadata.put( MD_ALARM_4, "7,8" );
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
    return null;
  }
}
