package org.kalypso.psiadapter;


import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * A specific item representing a timeserie.
 * 
 * @author schlienger
 */
public class PSICompactTimeserieItem extends PSICompactItem
{
  private final ObjectInfo m_objectInfo;

  private final int m_valueType;

  public PSICompactTimeserieItem( final PSICompactItem parent, final String name,
      final PSICompact.ObjectInfo info, final int valueType )
  {
    super( parent, name );

    m_objectInfo = info;
    m_valueType = valueType;
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
}
