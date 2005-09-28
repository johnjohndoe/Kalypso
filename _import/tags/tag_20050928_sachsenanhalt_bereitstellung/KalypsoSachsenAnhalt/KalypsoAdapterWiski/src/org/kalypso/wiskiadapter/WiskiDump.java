/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.wiskiadapter;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.repository.IRepositoryItem;

/**
 * Wiski-Dump action delegate that is displayed in the observation browser context menu
 * 
 * @author schlienger
 */
public class WiskiDump implements IActionDelegate
{
  private WiskiRepository m_wiskiRep = null;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( m_wiskiRep == null )
      throw new IllegalStateException( "Kann Wiski-Dump nicht erstellen weil WiskiRepository ist null" );

    final Counter counter = new Counter();

    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( final IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException
      {
        monitor.beginTask( "Wiski-Dump", IProgressMonitor.UNKNOWN );

        try
        {
          final IRepositoryItem[] supergroups = m_wiskiRep.getChildren();
          for( int i = 0; i < supergroups.length; i++ )
          {
            final SuperGroupItem sgi = (SuperGroupItem)supergroups[i];

            final IRepositoryItem[] groups = sgi.getChildren();
            for( int j = 0; j < groups.length; j++ )
            {
              final GroupItem gi = (GroupItem)groups[j];

              final IRepositoryItem[] stations = gi.getChildren();
              for( int k = 0; k < stations.length; k++ )
              {
                if( monitor.isCanceled() )
                  throw new InterruptedException();
                
                final TsInfoItem tsi = (TsInfoItem)stations[k];

                System.out.println( tsi.getName() + "\t" + tsi.getWiskiStationName() + "\t" + tsi.getIdentifier());
                counter.increment();
                
                monitor.subTask( counter + " Items behandelt..." );
              }
            }
          }
        }
        catch( final Exception e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    try
    {
      Workbench.getInstance().getProgressService().busyCursorWhile( runnable );

      MessageDialog.openInformation( Display.getDefault().getActiveShell(), "Wiski-Dump", counter
          + " Items wurden gelistet. Siehe Stdout." );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      MessageDialog
          .openWarning( Display.getDefault().getActiveShell(), "Struktur exportieren", e.getLocalizedMessage() );
    }
    catch( final InterruptedException ignored )
    {
      // empty
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    final Object object = ( (IStructuredSelection)selection ).getFirstElement();

    if( object instanceof WiskiRepository )
    {
      m_wiskiRep = (WiskiRepository)object;

      action.setEnabled( true );
    }
    else
      action.setEnabled( false );
  }

  private static class Counter
  {
    private int m_count = 0;

    public void increment()
    {
      m_count++;
    }

    public String toString()
    {
      return String.valueOf( m_count );
    }
  }
}
