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

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.kalypso.eclipse.jface.dialogs.TypeBasedInputValidator;
import org.kalypso.ogc.sensor.commands.AddRowCommand;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.editor.AbstractMapEditorActionDelegate;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;

/**
 * AddRow
 * 
 * @author schlienger
 */
public class AddRowAction extends AbstractMapEditorActionDelegate
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
