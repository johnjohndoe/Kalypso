package de.renew.workflow;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * The implementation of the FormTask.WorkItem.Proxy instance.
 * 
 * @see de.renew.workflow.Proxy
 */
public class ProxyImpl extends UnicastRemoteObject implements Proxy
{

  private static final long serialVersionUID = -3380690880914728985L;

  /**
   * The activity to create the proxy for.
   */
  private final Activity activity;

  private final Client m_client;

  /**
   * Creates a new proxy.
   * 
   * @param activity
   *          The activity to create the proxy for.
   * @param login
   *          The login info used when requesting the work item.
   * @exception RemoteException
   *              An RMI problem occurred.
   */
  public ProxyImpl( final Activity activity, final Client client ) throws RemoteException
  {
    this.activity = activity;
    m_client = client;
  }

  /**
   * Notifies the work item about the completion of the form dialog.
   * 
   * @param client
   *          The client object of the client where the form dialog was displayed.
   * @param formInstance
   *          The new filled form instance.
   * @exception RemoteException
   *              An RMI problem occurred.
   */
  public void notifyCompletion( final Object result ) throws RemoteException
  {
    try
    {
      if( result != null && result.equals( Proxy.CANCEL ) )
      {
        activity.cancel( m_client.getLogin(), m_client );
      }
      else
      {
        activity.confirm( m_client.getLogin(), m_client, result );
      }
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
    }
  }
}
