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

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.TableViewTemplate;
import org.kalypso.ogc.sensor.tableview.TableViewTheme;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorActionDelegate;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;

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
    boolean atLeastOneDirtySave = false;
    
    final ObservationTableEditor editor = (ObservationTableEditor) getEditor();
    final TableViewTemplate template = (TableViewTemplate) editor.getTemplate();
    final ObservationTableModel model = editor.getModel();

    final Collection themes = template.getThemes();

    for( final Iterator it = themes.iterator(); it.hasNext(); )
    {
      final TableViewTheme theme = (TableViewTheme) it.next();

      boolean dirtySave = false;

      for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
      {
        final TableViewColumn col = (TableViewColumn) itcol.next();
        dirtySave = col.isDirtySave();

        // at least one col dirty-save?
        if( dirtySave )
        {
          atLeastOneDirtySave = true;
          break;
        }
      }

      final IObservation obs = theme.getObservation();

      if( dirtySave )
      {
        final String msg = "Sie haben Änderungen in " + obs.getName()
            + " vorgenommen. Wollen \n" + "Sie die Änderungen übernehmen?";

        final boolean b = MessageDialog.openQuestion( getShell(),
            "Änderungen speichern", msg );

        if( b )
        {
          final ITuppleModel values;
          try
          {
            values = model.getValues( theme );
          }
          catch( SensorException e1 )
          {
            e1.printStackTrace();
            return;
          }
          
          final Job job = new Job( "ZML-Speichern: " + obs.getName() )
          {
            protected IStatus run( IProgressMonitor monitor )
            {
              try
              {
                obs.setValues( values );

                template.saveObservation( obs, monitor );
                
                for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
                  ((TableViewColumn) itcol.next()).resetDirtySave( );
              }
              catch( Exception e )
              {
                e.printStackTrace();
                return KalypsoGisPlugin.createErrorStatus( "", e );
              }

              return Status.OK_STATUS;
            }
          };
          
          job.schedule();
        }
      }
    }
    
    if( !atLeastOneDirtySave )
      MessageDialog.openInformation( getShell(), "Keine Änderung", "Keine geänderte Zeitreihe" );
  }
}