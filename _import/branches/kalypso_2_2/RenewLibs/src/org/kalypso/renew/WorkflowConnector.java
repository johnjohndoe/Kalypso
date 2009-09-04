package org.kalypso.renew;

import java.io.IOException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Collections;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PlatformUI;

import de.renew.access.LoginInfo;
import de.renew.remote.RemoteServerRegistry;
import de.renew.remote.SocketFactoryDeterminer;
import de.renew.remote.RemoteServerRegistry.ServerDescriptor;
import de.renew.util.ConcurrencyPreventer;
import de.renew.workflow.Activity;
import de.renew.workflow.Agenda;
import de.renew.workflow.AgendaChangeEvent;
import de.renew.workflow.AgendaChangeListener;
import de.renew.workflow.Client;
import de.renew.workflow.ClientImpl;
import de.renew.workflow.WorkItem;
import de.renew.workflow.WorkflowManager;
import de.renew.workflow.WorkflowManagerImpl;
import de.renew.workflow.connector.IWorkflowConnector;
import de.renew.workflow.connector.IWorklistChangeListener;
import de.renew.workflow.connector.Messages;
import de.renew.workflow.connector.WorkflowConnectorPlugin;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.cases.ICase;
import de.renew.workflow.connector.context.IActiveScenarioChangeListener;

public class WorkflowConnector implements IWorkflowConnector, IActiveScenarioChangeListener<ICase>
{
  private static final String SERVICE_NAME = "de.renew.workflow.WorkflowManager"; //$NON-NLS-1$

  private static Logger logger = Logger.getLogger( WorkflowConnector.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "de.renew.workflow.connector/debug" ) ); //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final boolean m_isWorkflowMode = true;

  private static final boolean m_disableRmi = true;

  private boolean _connected = false;

  private static WorkflowConnector _connector;

  private WorkflowManager m_manager;

  private LoginInfo m_login;

  private Client m_client;

  private final List<IWorklistChangeListener> m_listenerCache = Collections.synchronizedList( new Vector<IWorklistChangeListener>() );

  private AgendaChangeListener m_listenerProxy;

  private ICase m_activeCase;

  /**
   * @return the WorkflowConnector if connection is established, null otherwise
   */
  public static IWorkflowConnector getConnector( )
  {
    if( _connector == null )
    {
      _connector = new WorkflowConnector();
    }
    return _connector;
  }

  public boolean isConnected( )
  {
    return _connected;
  }

  private WorkflowConnector( )
  {
  }

  public void connect( )
  {
    boolean success = false;
    try
    {
      if( !m_disableRmi )
      {
        final RemoteServerRegistry instance = RemoteServerRegistry.instance();
        final ServerDescriptor descriptor = instance.connectServer( "localhost", "default" ); //$NON-NLS-1$ //$NON-NLS-2$
        m_manager = (WorkflowManager) Naming.lookup( descriptor.getUrl( SERVICE_NAME ) );
      }
      else
        m_manager = (WorkflowManager) WorkflowManagerImpl.getInstance();
      m_login = new LoginInfo( "Stefan", "Stefan" ); //$NON-NLS-1$ //$NON-NLS-2$
      m_manager.checkLogin( m_login );
      m_listenerProxy = new AgendaChangeListenerProxy();
      getAgenda().addChangeListener( m_listenerProxy );
      m_listenerProxy.notifyAgendaChange( new AgendaChangeEvent( this, AgendaChangeEvent.AVAILABLES_CHANGED ) );
      m_listenerProxy.notifyAgendaChange( new AgendaChangeEvent( this, AgendaChangeEvent.REQUESTEDS_CHANGED ) );
      success = true;
    }
    catch( final RemoteException e )
    {
      handleRemoteException( e );
    }
    catch( final IOException e )
    {
      // TODO: depends on Messages in another plugin... should be moved to
			// this plugin
      logger.log( Level.SEVERE, Messages.getString( "WorkflowConnector.7" ), e ); //$NON-NLS-1$
      WorkflowConnectorPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, 0, "", e ) ); //$NON-NLS-1$
    }
    catch( final NotBoundException e )
    {
      logger.log( Level.SEVERE, Messages.getString( "WorkflowConnector.8" ), e ); //$NON-NLS-1$
      WorkflowConnectorPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, 0, "", e ) ); //$NON-NLS-1$
    }

    _connected = success;
    if( success )
    {
      logger.info( Messages.getString( "WorkflowConnector.10" ) ); //$NON-NLS-1$
    }
    else
    {
      logger.info( Messages.getString( "WorkflowConnector.11" ) ); //$NON-NLS-1$
    }
  }

  private Agenda getAgenda( ) throws RemoteException, IOException
  {
    if( m_login != null )
    {
      return m_manager.getAgenda( m_login );
    }
    else
    {
      return null;
    }
  }

  public WorkItem[] getAvailables( )
  {
    if( m_isWorkflowMode && !isConnected() )
      _connector.connect();

    if( m_login != null )
    {
      try
      {
        if( m_activeCase != null )
        {
          // final String uri = m_activeCase.getURI();
          // return getAgenda().getAvailables( uri );
          return getAgenda().getAvailables( null );
        }
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
    return new WorkItem[0];
  }

  public Activity[] getRequesteds( )
  {
    if( m_isWorkflowMode && !isConnected() )
      _connector.connect();

    if( m_login != null )
    {
      try
      {
        return getAgenda().getRequesteds();
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
    return new Activity[0];
  }

  private class AgendaChangeListenerProxy extends UnicastRemoteObject implements AgendaChangeListener
  {

    private static final long serialVersionUID = -4720319067463967570L;

    /**
     * The concurrent update preventer for the availables.
     */
    private final ConcurrencyPreventer availablesConcurrencyPreventer;

    /**
     * Creates a new agenda change listener.
     *
     * @exception RemoteException
     *              An RMI problem occurred.
     */
    AgendaChangeListenerProxy( ) throws RemoteException
    {
      super( 0, SocketFactoryDeterminer.getInstance(), SocketFactoryDeterminer.getInstance() );
      availablesConcurrencyPreventer = new ConcurrencyPreventer( new AvailablesUpdater() );
    }

    /**
     * Notifies the listener about a change in an agenda.
     *
     * @param event
     *          The agenda change event.
     * @exception RemoteException
     *              An RMI problem occurred.
     */
    public void notifyAgendaChange( final AgendaChangeEvent event )
    {
      availablesConcurrencyPreventer.requestRun();
    }

    /**
     * The runnable for the concurrency preventer to update the availables.
     */
    private class AvailablesUpdater implements Runnable
    {
      public AvailablesUpdater( )
      {
      }

      public void run( )
      {
        if( PlatformUI.isWorkbenchRunning() )
        {
          PlatformUI.getWorkbench().getDisplay().asyncExec( new Runnable()
          {
            @SuppressWarnings("synthetic-access")
            public void run( )
            {
              for( final IWorklistChangeListener listener : m_listenerCache )
              {
                listener.worklistChanged();
              }
            }
          } );
        }
      }
    }
  }

  protected void disconnect( )
  {
    if( m_login != null )
    {
      try
      {
        getAgenda().removeChangeListener( m_listenerProxy );
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        m_listenerCache.clear();
        m_login = null;
      }
    }
    logger.info( Messages.getString( "WorkflowConnector.12" ) ); //$NON-NLS-1$
  }

  public void addWorklistChangeListener( final IWorklistChangeListener worklistChangeListener )
  {
    m_listenerCache.add( worklistChangeListener );
    worklistChangeListener.worklistChanged();
  }

  public void removeWorklistChangeListener( final IWorklistChangeListener worklistChangeListener )
  {
    m_listenerCache.remove( worklistChangeListener );
  }

  /**
   * Returns true if the work item with the id is currently available
   */
  public boolean canRequest( final String id )
  {
    return isWorkflowMode() && getWorkItem( id ) != null;
  }

  public boolean isActive( final String id )
  {
    return isWorkflowMode() && getActivity( id ) != null;
  }

  /**
   * Requests a new WorkItem and confirms the active Activity, if there is one
   */
  public Object request( final String id )
  {
    if( canRequest( id ) )
    {
      try
      {
        if( m_client == null )
        {
          m_client = new ClientImpl( m_login );
        }
        final Activity activity = getWorkItem( id ).request( m_login, m_client );
        final WorkItem workItem = activity.getWorkItem();
        logger.info( Messages.getString( "WorkflowConnector.13" ) + workItem.getTask().getName() ); //$NON-NLS-1$
        return workItem.getParameter();
      }
      catch( final RemoteException e )
      {
        handleRemoteException( e );
      }
    }
    logger.info( Messages.getString( "WorkflowConnector.14" ) + id ); //$NON-NLS-1$
    return null;
  }

  /**
   * Confirms an activity that was previously requested
   */
  public void confirm( final String id, final Object result )
  {
    if( isActive( id ) )
    {
      try
      {
        final Activity activity = getActivity( id );
        final boolean confirmed = activity.confirm( m_login, m_client, result );
        if( confirmed )
        {
          logger.info( Messages.getString( "WorkflowConnector.15" ) + activity.getWorkItem().getTask().getName() ); //$NON-NLS-1$
        }
        else
        {
          logger.info( Messages.getString( "WorkflowConnector.16" ) + activity.getWorkItem().getTask().getName() ); //$NON-NLS-1$
        }
      }
      catch( final RemoteException e )
      {
        handleRemoteException( e );
      }
      catch( final SecurityException e )
      {
        handleSecurityException( e );
      }
    }
    logger.info( Messages.getString( "WorkflowConnector.17" ) + id ); //$NON-NLS-1$
  }

  /**
   * Cancels an activity that was previously requested
   */
  public void cancel( final String id )
  {
    if( isActive( id ) )
    {
      try
      {
        final Activity activity = getActivity( id );
        activity.cancel( m_login, m_client );
        logger.info( Messages.getString( "WorkflowConnector.18" ) + activity.getWorkItem().getTask().getName() ); //$NON-NLS-1$
      }
      catch( final RemoteException e )
      {
        handleRemoteException( e );
      }
      catch( final SecurityException e )
      {
        handleSecurityException( e );
      }
    }
    logger.info( Messages.getString( "WorkflowConnector.19" ) + id ); //$NON-NLS-1$
  }

  private Activity getActivity( final String id )
  {
    try
    {
      for( final Activity a : getAgenda().getRequesteds() )
      {
        final String name = a.getWorkItem().getTask().getName();
        if( name.equals( id ) )
        {
          return a;
        }
        else
        {
          continue;
        }
      }
    }
    catch( final RemoteException e )
    {
      handleRemoteException( e );
    }
    catch( final IOException e )
    {
      logger.log( Level.SEVERE, "", e ); //$NON-NLS-1$
    }
    return null;
  }

  private WorkItem getWorkItem( final String id )
  {
    try
    {
      for( final WorkItem w : getAvailables() )
      {
        final String name = w.getTask().getName();
        if( name.equals( id ) )
        {
          return w;
        }
        else
        {
          continue;
        }
      }
    }
    catch( final RemoteException e )
    {
      handleRemoteException( e );
    }
    return null;
  }

  private void handleRemoteException( final RemoteException e )
  {
    logger.log( Level.SEVERE, Messages.getString( "WorkflowConnector.21" ), e ); //$NON-NLS-1$
    WorkflowConnectorPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, 0, "", e ) ); //$NON-NLS-1$
  }

  private void handleSecurityException( final SecurityException e )
  {
    logger.log( Level.SEVERE, Messages.getString( "WorkflowConnector.23" ), e ); //$NON-NLS-1$
    WorkflowConnectorPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, 0, "", e ) ); //$NON-NLS-1$
  }

  public boolean isWorkflowMode( )
  {
    return m_isWorkflowMode && isConnected();
  }

  /**
   * @see de.renew.workflow.connector.context.IActiveContextChangeListener#activeContextChanged(de.renew.workflow.connector.context.CaseHandlingProjectNature,
   *      de.renew.workflow.cases.Case)
   */
  public void activeScenarioChanged( final CaseHandlingProjectNature newProject, final ICase caze )
  {
    m_activeCase = caze;
  }
}
