package de.renew.workflow;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * The implementation of the FormTask.WorkItem.Proxy instance.
 * 
 * @see de.renew.workflow.FormTask.WorkItem.Proxy
 */
public interface Proxy extends Remote
{

  public static final int OK = 0;

  public static final int CANCEL = 1;

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
  public void notifyCompletion( final Object result ) throws RemoteException;
}