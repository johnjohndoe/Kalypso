package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.sensor.commands.RemoveRowCommand;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.editor.AbstractEditorActionDelegate;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;

/**
 * RemoveRowAction
 * 
 * @author schlienger
 */
public class RemoveRowAction extends AbstractEditorActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    final ObservationTableEditor editor = (ObservationTableEditor) getEditor();
    final ObservationTableModel model = editor.getModel();
    
    if( model.getRowCount() == 0 )
      return;
    
    final int rowIndex = editor.getTable().getSelectedRow();
    
    if( rowIndex < 0 )
      return;
    
    editor.postCommand( new RemoveRowCommand( model, rowIndex), null );
  }
}
