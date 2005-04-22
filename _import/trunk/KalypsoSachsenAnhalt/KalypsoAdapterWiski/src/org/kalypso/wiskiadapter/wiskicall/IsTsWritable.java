package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * IsTsWritable
 * 
 * @author schlienger
 */
public class IsTsWritable implements IWiskiCall
{
  private final Long m_id;
  private Boolean m_editable;

  public IsTsWritable( final Long id )
  {
    m_id = id;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException, RemoteException
  {
    final HashMap map = wiski.isTsWritable( userData,
        new Long[] { m_id }, null );
    
    m_editable = (Boolean) map.get( m_id );
  }
  
  public Boolean getEditable( )
  {
    return m_editable;
  }
}
