package org.kalypso.eclipse.debug.core.model;

import java.util.Properties;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.ITerminate;
import org.kalypso.eclipse.jface.operation.IProgressRunnable;

/**
 * Standard implementation of an <code>IProcess</code> that wrappers a system
 * thread (<code>java.lang.Thread</code>).
 */
public class RunnableProcessJob extends Job implements IProcess, IAdaptable
{
  private final ILaunch m_launch;

  private boolean m_terminated;

  private final Properties m_attribProps = new Properties();

  private final String m_label;

  private IProgressRunnable m_runnable;

  /**
   * Constructs a RunnableProcessJob on the given system thread with the given
   * name, adding this thread to the given launch.
   */
  public RunnableProcessJob( final ILaunch launch, final IProgressRunnable runnable,
      final String label, final Properties attribProps )
  {
    super( label );

    setPriority( Job.INTERACTIVE );

    m_launch = launch;

    m_attribProps.putAll( attribProps );

    m_runnable = runnable;
    m_label = label;

    launch.addProcess( this );

    fireCreationEvent();

    schedule();
  }
  
  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus run( final IProgressMonitor monitor )
  {
    final IStatus status = m_runnable.run( monitor );
    
    onTerminated();
    
    return status;
  }


  /**
   * @see ITerminate#canTerminate()
   */
  public boolean canTerminate()
  {
    return !m_terminated;
  }

  /**
   * @see IProcess#getLabel()
   */
  public String getLabel()
  {
    return m_label;
  }

  /**
   * @see IProcess#getLaunch()
   */
  public ILaunch getLaunch()
  {
    return m_launch;
  }

  /**
   * @see ITerminate#isTerminated()
   */
  public boolean isTerminated()
  {
    return m_terminated;
  }

  /**
   * @see ITerminate#terminate()
   */
  public void terminate()
  {
    if( !isTerminated() )
      cancel();
  }

  /**
   * Called from monitor
   */
  protected void onTerminated()
  {
    m_terminated = true;

    fireTerminateEvent();
  }

  /**
   * Fires a creation event.
   */
  protected void fireCreationEvent()
  {
    fireEvent( new DebugEvent( this, DebugEvent.CREATE ) );
  }

  /**
   * Fires the given debug event.
   * 
   * @param event
   *          debug event to fire
   */
  protected void fireEvent( final DebugEvent event )
  {
    DebugPlugin manager = DebugPlugin.getDefault();
    if( manager != null )
    {
      manager.fireDebugEventSet( new DebugEvent[]
      { event } );
    }
  }

  /**
   * Fires a terminate event.
   */
  protected void fireTerminateEvent()
  {
    fireEvent( new DebugEvent( this, DebugEvent.TERMINATE ) );
  }

  /**
   * Fires a change event.
   */
  protected void fireChangeEvent()
  {
    fireEvent( new DebugEvent( this, DebugEvent.CHANGE ) );
  }

  /**
   * @see IProcess#setAttribute(String, String)
   */
  public void setAttribute( final String key, final String value )
  {
    final Object origVal = m_attribProps.get( key );
    if( origVal != null && origVal.equals( value ) )
      return; //nothing changed.

    m_attribProps.put( key, value );
    fireChangeEvent();
  }

  /**
   * @see IProcess#getAttribute(String)
   */
  public String getAttribute( final String key )
  {
    return (String)m_attribProps.get( key );
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    if( adapter.equals( IProcess.class ) )
    {
      return this;
    }

    if( adapter.equals( IDebugTarget.class ) )
    {
      ILaunch launch = getLaunch();
      IDebugTarget[] targets = launch.getDebugTargets();
      for( int i = 0; i < targets.length; i++ )
      {
        if( this.equals( targets[i].getProcess() ) )
          return targets[i];
      }
      return null;
    }

    return null;
  }

  /**
   * @see org.eclipse.debug.core.model.IProcess#getStreamsProxy()
   */
  public IStreamsProxy getStreamsProxy()
  {
    // not supported
    return null;
  }

  /**
   * @see org.eclipse.debug.core.model.IProcess#getExitValue()
   */
  public int getExitValue()
  {
    // unsupported
    return 0;
  }
}