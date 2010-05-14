package org.kalypso.renew.debug;

import java.rmi.RemoteException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import de.renew.workflow.WorkItem;
import de.renew.workflow.connector.WorkflowConnector;

public class RequestWorkItemActionDelegate implements IObjectActionDelegate
{

  private static final Logger logger = Logger.getLogger( RequestWorkItemActionDelegate.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "de.renew.workflow.connector/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private String m_workItem;

  public void run( IAction action )
  {
    WorkflowConnector.getConnector().request( m_workItem );
  }

  public void selectionChanged( IAction action, ISelection selection )
  {
    if( selection instanceof IStructuredSelection )
    {
      IStructuredSelection ss = (IStructuredSelection) selection;
      final Object firstElement = ss.getFirstElement();
      if( firstElement instanceof WorkItem )
      {
        try
        {
          m_workItem = ((WorkItem) firstElement).getTask().getName();
        }
        catch( final RemoteException e )
        {
          logger.log( Level.SEVERE, "connection problem", e );
        }
      }
    }
  }

  public void setActivePart( IAction action, IWorkbenchPart targetPart )
  {
    // 
  }

}
