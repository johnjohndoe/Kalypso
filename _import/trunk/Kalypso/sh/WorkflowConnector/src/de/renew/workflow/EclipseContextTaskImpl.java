package de.renew.workflow;

import java.rmi.RemoteException;
import java.util.Map;

import de.renew.net.TransitionInstance;

/**
 * @author Stefan Kurzbach
 */
public class EclipseContextTaskImpl extends ConfirmedTaskImpl
{
  private static final long serialVersionUID = -309633924634403286L;

  /**
   * @param adaptor
   * @param name
   * @param description
   * @param priority
   * @param settings
   * @throws RemoteException
   */
  public EclipseContextTaskImpl( WorkflowAdaptor adaptor, String name, String description, int priority, Map settings ) throws RemoteException
  {
    super( adaptor, name, description, priority, settings );
  }

  /**
   * @see de.renew.workflow.ConfirmedTaskImpl#createWorkItem(de.renew.net.TransitionInstance,
   *      de.renew.net.TransitionInstance, de.renew.net.TransitionInstance, java.lang.Object, int)
   */
  @Override
  protected TaskImpl.WorkItemImpl createWorkItem( TransitionInstance requestTransition, TransitionInstance confirmTransition, TransitionInstance cancelTransition, Object parameter, int priority ) throws RemoteException
  {
    return new WorkItemImpl( requestTransition, confirmTransition, cancelTransition, parameter, priority );
  }

  /**
   * @author Stefan Kurzbach
   */
  public class WorkItemImpl extends de.renew.workflow.ConfirmedTaskImpl.WorkItemImpl implements EclipseContextTask.WorkItem
  {

    private static final long serialVersionUID = -6770835318635105656L;

    /**
     * @param requestTransition
     * @param confirmTransition
     * @param cancelTransition
     * @param parameter
     * @param priority
     * @throws RemoteException
     */
    protected WorkItemImpl( TransitionInstance requestTransition, TransitionInstance confirmTransition, TransitionInstance cancelTransition, Object parameter, int priority ) throws RemoteException
    {
      super( requestTransition, confirmTransition, cancelTransition, parameter, priority );
    }

    /**
     * @see de.renew.workflow.TaskImpl$WorkItemImpl#notifyRequested(de.renew.workflow.ActivityImpl,
     *      de.renew.workflow.Client)
     */
    @Override
    protected void notifyRequested( final ActivityImpl activity, final Client client )
    {
      try
      {
        final Map settings = activity.getWorkItem().getTask().getSettings();
        final String runtimeParameter = activity.getWorkItem().getParameter().toString();
        settings.put( EclipseContextTask.RUNTIME_PARAMETER, runtimeParameter );
        final Proxy proxy = new ProxyImpl( activity, client );
        client.execute( new EclipseOperationDelegate( settings, proxy ) );
      }
      catch( final RemoteException e )
      {
        e.printStackTrace();
      }
    }

    /**
     * Notifies an implementation of the work item that is has been restored from the database after e.g. a server
     * crash. This implementation cancels the activity then, because the task is no longer active and the client may be
     * no longer valid. This method is automatically called by the workflow package.
     * 
     * @param activity
     *          The corresponding activity.
     */
    protected void notifyRestored( ActivityImpl activity )
    {
      activity.cancelChecked( null );
    }
  }
}
