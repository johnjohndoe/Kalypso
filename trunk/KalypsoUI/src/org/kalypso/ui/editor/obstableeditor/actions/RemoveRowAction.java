package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.sensor.commands.RemoveRowCommand;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;

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
    final ObservationTableModel model = getEditor().getModel();
    
    if( model.getRowCount() == 0 )
      return;
    
    final int rowIndex = getEditor().getTable().getSelectedRow();
    
    getEditor().postCommand( new RemoveRowCommand( model, rowIndex), null );
  }
}
