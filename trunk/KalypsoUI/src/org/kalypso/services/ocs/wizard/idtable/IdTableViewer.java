package org.kalypso.services.ocs.wizard.idtable;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;


/**
 * 
 * IdTableViewer
 * 
 * @author schlienger
 */
public class IdTableViewer extends TableViewer
{
  public IdTableViewer( Composite parent, int style )
  {
    super( parent, style );
    
    final Table table = getTable();
    
    final TableColumn colFile = new TableColumn( table, SWT.RIGHT );
    colFile.setText( "Zeitreihen" );
    
    final TableColumn colId = new TableColumn( table, SWT.RIGHT );
    colId.setText( "Server-KZ" );
    
    setColumnProperties( new String[] { "Zeitreihen", "Server-KZ" } );
    setContentProvider( new IdTableContentProvider() );
  }
}