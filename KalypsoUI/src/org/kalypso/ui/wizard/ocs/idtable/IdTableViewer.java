package org.kalypso.ui.wizard.ocs.idtable;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.kalypso.eclipse.jface.viewers.DefaultTableViewer;


/**
 * IdTableViewer
 * 
 * @author schlienger
 */
public class IdTableViewer extends DefaultTableViewer
{
  public IdTableViewer( Composite parent, int style )
  {
    super( parent, style );
    
    setContentProvider( new IdTableContentProvider() );
    setLabelProvider( new IdTableLabelProvider() );
    //setCellEditors(  );
    
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );
    
    addColumn( "Zeitreihen", "Zeitreihen", 250, true );
    addColumn( "Server-KZ", "Server-KZ", 400, true );

    refreshColumnProperties();
  }
}