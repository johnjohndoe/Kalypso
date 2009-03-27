package org.kalypso.calculation.connector;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

public abstract class AbstractInternalStatusJob
{

  private IStatus m_status = StatusUtilities.createInfoStatus( "Init", new Object[0] );

  protected static enum STATUS
  {
    OK,
    INFO,
    ERROR
  }

  protected void setStatus( final STATUS status, final String message )
  {
    switch( status )
    {
      case OK:
        m_status = StatusUtilities.createOkStatus( message, new Object[0] );
        break;
      case INFO:
        m_status = StatusUtilities.createInfoStatus( message, new Object[0] );
        break;
      default:
        m_status = StatusUtilities.createErrorStatus( message, new Object[0] );
        break;
    }
  }

  protected boolean isOkStatus( )
  {
    return m_status.isOK();
  }

}
