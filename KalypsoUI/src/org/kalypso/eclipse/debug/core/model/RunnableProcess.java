package org.kalypso.eclipse.debug.core.model;

import java.util.Properties;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.ITerminate;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * Standard implementation of an <code>IProcess</code> that wrappers a system
 * thread (<code>java.lang.Thread</code>).
 */
public class RunnableProcess implements IProcess, IAdaptable
{
  private final ILaunch m_launch;

  private RunnableJob m_monitorJob;

  private boolean m_terminated;

  private final Properties m_attribProps = new Properties();

  private final String m_label;

  private IRunnableWithProgress m_runnable;

  /**
   * Constructs a RunnableProcess on the given system thread with the given name,
   * adding this thread to the given launch.
   */
  public RunnableProcess( final ILaunch launch, final IRunnableWithProgress runnable, final String label,
      final Properties attribProps )
  {
    m_launch = launch;

    if( attribProps != null )
      m_attribProps.putAll( attribProps );

    m_runnable = runnable;

    m_label = label;

    m_monitorJob = new RunnableJob( this, m_runnable );
    m_monitorJob.getClass(); // suppress warning

    launch.addProcess( this );

    fireCreationEvent();
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
    {
      m_monitorJob.cancel();
      
      m_monitorJob = null;
    }
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
  protected void fireEvent( DebugEvent event )
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