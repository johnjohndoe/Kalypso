package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.sensor.tableview.swing.ExportableObservationTable;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;
import org.kalypso.ui.metadoc.AbstractExportActionDelegate;
import org.kalypso.ui.metadoc.table.ExportTableBerichtWizard;

/**
 * @author schlienger
 */
public class ExportBerichtActionDelegate extends AbstractExportActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    final ObservationTableEditor editor = (ObservationTableEditor) getEditor();
    
    final ExportableObservationTable exportable = new ExportableObservationTable( editor.getTable() );
    
    runExportAction( ExportTableBerichtWizard.class, exportable, editor.getSite().getShell() );
  }
}