package org.kalypso.ogc.sensor.tableview.swing.event;

import java.lang.reflect.InvocationTargetException;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * ObservationModelChangeListener
 * 
 * @author schlienger
 */
public class ObservationModelChangeListener implements TableModelListener
{
  private final IRunnableWithProgress m_rwp;

  private final String m_message;

  public ObservationModelChangeListener( final String message,
      final IRunnableWithProgress rwp )
  {
    m_message = message;
    m_rwp = rwp;
  }

  /**
   * @see javax.swing.event.TableModelListener#tableChanged(javax.swing.event.TableModelEvent)
   */
  public void tableChanged( TableModelEvent e )
  {
    final IRunnableWithProgress rwp = m_rwp;
//    final String msg = m_message;

    // all this stuff commented out because of synchronisation problems.
    // and it is actually bad code to make a thread for each change in the gui
    // TODO remoce all these comments once testing has been fully done
//    final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
//    {
//      protected void execute( IProgressMonitor monitor ) throws CoreException,
//          InvocationTargetException, InterruptedException
//      {
//        rwp.run( monitor );
//      }
//    };

//    final Job job = new Job( m_message )
//    {
//      protected IStatus run( IProgressMonitor monitor )
//      {
//        try
//        {
//          op.run( monitor );
//        }
//        catch( InvocationTargetException ex )
//        {
//          ex.printStackTrace();
//          return KalypsoGisPlugin.createErrorStatus( msg, ex );
//        }
//        catch( InterruptedException ex )
//        {
//          ex.printStackTrace();
//        }
//
//        return Status.OK_STATUS;
//      }
//    };

//    job.schedule();
    
    try
    {
      rwp.run( new NullProgressMonitor() );
    }
    catch( InvocationTargetException e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }
    catch( InterruptedException e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }
  }
}