package org.kalypso.kalypsomodel1d2d.sim;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;

public class SimulationMonitorAdaptor implements IProgressMonitor
{
  private final ISimulationMonitor m_monitor;

  private int m_worked;

  private int m_totalWork;

  public SimulationMonitorAdaptor( final ISimulationMonitor monitor )
  {
    m_monitor = monitor;
  }

  @Override
  public void beginTask( final String name, final int totalWork )
  {
    m_totalWork = totalWork;
    m_monitor.setMessage( name );
  }

  public void done( final IStatus status )
  {
    m_monitor.setProgress( 100 );
    m_monitor.setFinishInfo( status.getSeverity(), status.getMessage() ); //$NON-NLS-1$
  }

  @Override
  public void done( )
  {
    done( StatusUtilities.createOkStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SimulationMonitorAdaptor.0" ) ) );
  }

  @Override
  public void internalWorked( final double work )
  {
  }

  @Override
  public boolean isCanceled( )
  {
    return m_monitor.isCanceled();
  }

  @Override
  public void setCanceled( final boolean value )
  {
    if( value )
      m_monitor.cancel();
  }

  @Override
  public void setTaskName( final String name )
  {
    m_monitor.setMessage( name );
  }

  @Override
  public void subTask( final String name )
  {
    m_monitor.setMessage( name );
  }

  @Override
  public void worked( final int work )
  {
    m_monitor.setProgress( (m_worked += work) / m_totalWork );
  }

}
