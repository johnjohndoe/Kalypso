package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.kalypso.eclipse.jface.dialogs.TypeBasedInputValidator;
import org.kalypso.ogc.sensor.commands.AddRowCommand;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.editor.AbstractEditorActionDelegate;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;

/**
 * AddRow
 * 
 * @author schlienger
 */
public class AddRowAction extends AbstractEditorActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final ObservationTableEditor editor = (ObservationTableEditor) getEditor();
    final ObservationTableModel model = editor.getModel();
    
    if( model.getColumnCount() <= 0 )
      return;

    final Class columnClass = model.getColumnClass( 0 );
    final String columnName = model.getColumnName( 0 );

    final TypeBasedInputValidator inpv = new TypeBasedInputValidator(
        columnClass );

    final InputDialog dlg = new InputDialog( getEditor().getSite().getShell(), "Neue Zeile",
        "Geben Sie bitte den Wert für die Spalte " + columnName + " ein.", inpv
            .defaultValue(), inpv );

    if( dlg.open() == Window.OK )
    {
      AddRowCommand command = new AddRowCommand( editor.getModel(), inpv.toValue( dlg.getValue() ) );
      
      editor.postCommand( command, null );
    }
  }
}
