/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.obstableeditor.actions;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.eclipse.core.runtime.MultiStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorActionDelegate;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;
import org.kalypso.util.pool.ResourcePool;

/**
 * Save data
 * 
 * @author schlienger
 */
public class SaveDataAction extends AbstractEditorActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    boolean atLeastOneDirty = false;

    final MultiStatus status = new MultiStatus( IStatus.OK, KalypsoGisPlugin.getId(), 0, "Zeitreihen speichern" );

    final ObservationTableModel model = ( (ObservationTableEditor)getEditor() ).getModel();

    final Map map = model.getMappedColumns();
    for( final Iterator it = map.entrySet().iterator(); it.hasNext(); )
    {
      final Map.Entry entry = (Entry)it.next();
      final IObservation obs = (IObservation)entry.getKey();
      final List cols = (List)entry.getValue();

      boolean obsSaved = false;

      for( final Iterator itCols = cols.iterator(); itCols.hasNext(); )
      {
        final TableViewColumn col = (TableViewColumn)itCols.next();

        if( col.isDirty() && !obsSaved )
        {
          atLeastOneDirty = true;

          final String msg = "Sie haben �nderungen in " + obs.getName() + " vorgenommen. Wollen \n"
              + "Sie die �nderungen �bernehmen?";

          final boolean bConfirm = MessageDialog.openQuestion( getShell(), "�nderungen speichern", msg );

          if( !bConfirm )
            break;

          final Job job = new Job( "ZML-Speichern: " + obs.getName() )
          {
            protected IStatus run( IProgressMonitor monitor )
            {
              try
              {
                final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
                pool.saveObject( obs, monitor );
              }
              catch( final Exception e )
              {
                e.printStackTrace();
                status.addMessage( "Fehler beim Speichern von " + obs, e );
              }

              return Status.OK_STATUS;
            }
          };

          job.schedule();

          // flag se to true so next time we don't save obs if already done
          obsSaved = true;
        }

        col.setDirty( false );
      }
    }

    if( !atLeastOneDirty )
      MessageDialog.openInformation( getShell(), "Keine �nderung", "Keine ge�nderte Zeitreihe" );

    if( !status.isOK() )
      ErrorDialog.openError( getShell(), "Zeitreihen speichern", "Fehler sind aufgetreten. Siehe Details.", status );
  }
}