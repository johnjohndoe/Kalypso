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
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.impl.LinkedTableViewTemplate;
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
    final ObservationTableEditor editor = (ObservationTableEditor) getEditor();
    final LinkedTableViewTemplate template = editor.getTemplate();
    final ObservationTableModel model = editor.getModel();

    final Collection themes = template.getThemes();

    for( final Iterator it = themes.iterator(); it.hasNext(); )
    {
      final ITableViewTheme theme = (ITableViewTheme) it.next();

      boolean dirty = false;

      for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
      {
        dirty = ((ITableViewColumn) itcol.next()).isDirty();

        // at least one col dirty?
        if( dirty )
          break;
      }

      final IObservation obs = theme.getObservation();

      if( dirty )
      {
        final String msg = "Sie haben Änderungen in " + obs.getName()
            + " vorgenommen. Wollen \n" + "Sie die Änderungen übernehmen?";

        final boolean b = MessageDialog.openQuestion( getShell(),
            "Änderungen speichern", msg );

        if( b )
        {
          for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
            ((ITableViewColumn) itcol.next()).setDirty( false );

          final ITuppleModel values = model.getValues( theme.getColumns() );

          final Job job = new Job( "ZML-Speichern: " + obs.getName() )
          {
            protected IStatus run( IProgressMonitor monitor )
            {
              try
              {
                obs.setValues( values );

                template.saveObservation( obs, monitor );
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
  }
}