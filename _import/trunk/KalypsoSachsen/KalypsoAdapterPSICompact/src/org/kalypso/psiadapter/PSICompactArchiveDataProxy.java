package org.kalypso.psiadapter;

import java.util.Date;
import java.util.List;
import java.util.Vector;

import org.kalypso.ogc.sensor.SensorException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ArchiveData;
import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * Proxy class. Hold die ArchiveData[] aus der PSICompact Schnittstelle bei Bedarf heraus.
 * 
 * @author schlienger
 */
public class PSICompactArchiveDataProxy
{
  private final ObjectInfo m_objectInfo;
  
  private Date m_from = null;
  private Date m_to = null;
  private ArchiveData[] m_data = null;

  public PSICompactArchiveDataProxy( final ObjectInfo objectInfo )
  {
    m_objectInfo = objectInfo;
  }

  private ArchiveData[] getValues( Date from, Date to ) throws SensorException
  {
    if( m_data != null && m_from == from && m_to == to )
      return m_data;
    
    try
    {
      m_from = from;
      m_to = to;
      m_data = PSICompactFactory.getConnection().getArchiveData( m_objectInfo.getId(),
          PSICompact.ARC_MIN15, from, to );
      
      return m_data;
    }
    catch( ECommException e )
    {
      throw new SensorException( e );
    }
  }

  public List getDateList( Date from, Date to )
  {
    return new Vector();
  }
  
  public List getValueList( Date from, Date to )
  {
    return new Vector();
  }

  public List getStatusList( Date from, Date to )
  {
    return new Vector();
  }
}
