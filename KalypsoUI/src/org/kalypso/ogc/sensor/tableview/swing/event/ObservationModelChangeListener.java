package org.kalypso.ogc.sensor.tableview.swing.event;

import java.lang.reflect.InvocationTargetException;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * ObservationModelChangeListener
 * 
 * @author schlienger
 */
public class ObservationModelChangeListener implements TableModelListener
{
  private final IRunnableWithProgress m_rwp;

  public ObservationModelChangeListener( final IRunnableWithProgress rwp )
  {
    m_rwp = rwp;
  }

  /**
   * @see javax.swing.event.TableModelListener#tableChanged(javax.swing.event.TableModelEvent)
   */
  public void tableChanged( TableModelEvent e )
  {
    try
    {
      m_rwp.run( new NullProgressMonitor() );
    }
    catch( InvocationTargetException e1 )
    {
      e1.printStackTrace();
    }
    catch( InterruptedException e1 )
    {
      e1.printStackTrace();
    }
  }
}