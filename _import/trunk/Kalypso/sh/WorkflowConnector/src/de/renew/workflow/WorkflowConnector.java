package de.renew.workflow;

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

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;

import de.renew.access.LoginInfo;
import de.renew.access.Manager;
import de.renew.remote.RemoteServerRegistry;
import de.renew.remote.SocketFactoryDeterminer;
import de.renew.remote.RemoteServerRegistry.ServerDescriptor;
import de.renew.util.ConcurrencyPreventer;
import de.renew.workflow.event.IWorklistChangeListener;

public class WorkflowConnector
{
  private static final String TASK_NAMESPACE_PREFIX = "http://www.tu-harburg.de/wb/kalypso/kb/workflow/test#";

  private static final String SERVICE_NAME = "de.renew.workflow.WorkflowManager";

  public static Logger logger = Logger.getLogger( WorkflowConnector.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "de.renew.workflow.connector/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  public static boolean isWorkflowMode( )
  {
    return false;
  }

  private LoginInfo m_login;

  List<IWorklistChangeListener> m_listenerCache = Collections.synchronizedList( new Vector<IWorklistChangeListener>() );

  private AgendaChangeListener m_listenerProxy;

  private Manager m_manager;

  private static WorkflowConnector _connector;

  private boolean _connected = false;

  private Client m_client;

  /**
   * @return the WorkflowConnector if connection is established, null otherwise
   */
  public static WorkflowConnector getConnector( )
  {
    if( _connector == null )
    {
      _connector = new WorkflowConnector();
    }
    if( !_connector.isConnected() )
    {
      _connector.connect();
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

  /**
   * Starts a login-dialog and - if the login is successful - the AgendaGUI.
   * 
   * @return true, if the GUI properly started. false, if an error occured (e.g. the login was not successful)
   */
  protected void connect( )
  {
    boolean success = false;
    try
    {
      final RemoteServerRegistry instance = RemoteServerRegistry.instance();
//      waitForServer(instance);
      final ServerDescriptor descriptor = instance.connectServer( "localhost", "default" );
      m_manager = (Manager) Naming.lookup( descriptor.getUrl( SERVICE_NAME ) );
      m_login = new LoginInfo( "Stefan", "Stefan" );
      m_manager.checkLogin( m_login );
      m_listenerProxy = new AgendaChangeListenerProxy();
      getAgenda().addChangeListener( m_listenerProxy );
      m_listenerProxy.notifyAgendaChange( new AgendaChangeEvent( this, AgendaChangeEvent.AVAILABLES_CHANGED ) );
      m_listenerProxy.notifyAgendaChange( new AgendaChangeEvent( this, AgendaChangeEvent.REQUESTEDS_CHANGED ) );
      success = true;
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( NotBoundException e )
    {
      e.printStackTrace();
    }

    _connected = success;
    if( success )
      logger.info( "Connected to workflow server" );
    else
      logger.info( "Could not connect to workflow server" );
  }

//  private void waitForServer(final RemoteServerRegistry instance )
//  {
//    int timeoutSeconds = 10;
//    while( timeoutSeconds-- > 0 && instance.getSallServers().length == 0 )
//    {
//      try
//      {
//        Thread.sleep( 1000 );
//      }
//      catch( final InterruptedException e )
//      {
//        logger.log( Level.WARNING, "interrupted", e );
//      }
//    }
//    if( instance.allServers().length == 0 )
//    {
//      logger.log( Level.SEVERE, "timeout reached" );
//    }
//  }

  private Agenda getAgenda( ) throws RemoteException, IOException
  {
    if( m_login != null )
    {
      return ((WorkflowManager) m_manager).getAgenda( m_login );
    }
    else
    {
      return null;
    }
  }

  public WorkItem[] getAvailables( )
  {
    if( m_login != null )
    {
      try
      {
        return getAgenda().getAvailables();
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

  class AgendaChangeListenerProxy extends UnicastRemoteObject implements AgendaChangeListener
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
    class AvailablesUpdater implements Runnable
    {

      public void run( )
      {
        PlatformUI.getWorkbench().getDisplay().asyncExec( new Runnable()
        {
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
    logger.info( "Disconnected from workflow server." );
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
    return getWorkItem( id ) != null;
  }

  public boolean isActive( String id )
  {
    return getActivity( id ) != null;
  }

  /**
   * Requests a new WorkItem and confirms the active Activity, if there is one
   */
  public Activity request( final String id )
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
        logger.info( "requested " + activity.getWorkItem().getTask().getName() );
        return activity;
      }
      catch( final RemoteException e )
      {
        handleRemoteException( e );
      }
    }
    return null;
  }

  /**
   * Confirms an activity that was previously requested
   */
  public void confirm( String id, final Object result )
  {
    if( isActive( id ) )
    {
      try
      {
        final Activity activity = getActivity( id );
        activity.confirm( m_login, m_client, result );
        logger.info( "confirmed " + activity.getWorkItem().getTask().getName() );
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
    else
    {
      logger.info( "no activity " + id );
    }
  }

  /**
   * Cancels an activity that was previously requested
   */
  public void cancel( final String id )
  {
    if(isActive( id ) )
    {
      try
      {
        final Activity activity = getActivity( id );
        activity.cancel( m_login, m_client );
        logger.info( "cancelled " + activity.getWorkItem().getTask().getName() );
      }
      catch( final RemoteException e )
      {
        handleRemoteException( e );
      }
      catch( final SecurityException e )
      {
        handleSecurityException( e );
      }
    }else
    {
      logger.info( "no activity " + id );
    }
  }

  private Activity getActivity( final String id )
  {
    try
    {
      for( final Activity a : getAgenda().getRequesteds() )
      {
        final String name = TASK_NAMESPACE_PREFIX + a.getWorkItem().getTask().getName();
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
      logger.log( Level.SEVERE, "", e );
    }
    return null;
  }

  private WorkItem getWorkItem( final String id )
  {
    try
    {
      for( final WorkItem w : getAvailables() )
      {
        final String name = TASK_NAMESPACE_PREFIX + w.getTask().getName();
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
    logger.log( Level.SEVERE, "connection problem", e );
  }

  private void handleSecurityException( final SecurityException e )
  {
    logger.log( Level.SEVERE, "cannot cancel", e );
  }

  public static boolean checkWorkflowMode( )
  {
    return !isWorkflowMode() || getConnector().isConnected();
  }

}
