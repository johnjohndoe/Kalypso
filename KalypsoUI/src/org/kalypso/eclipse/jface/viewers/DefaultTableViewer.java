package org.kalypso.eclipse.jface.viewers;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

/**
 * DefaultTableViewer handles common functionality that you wish you had when working
 * with a TableViewer.
 * 
 * @author schlienger
 */
public class DefaultTableViewer extends TableViewer
{
  public static final String COLUMN_PROP_NAME = "columnName";
  public static final String COLUMN_PROP_EDITABLE = "columnEditable";
  public static final String COLUMN_PROP_WIDTH = "columnWidth";

  /**
   * @param parent
   */
  public DefaultTableViewer( Composite parent )
  {
    super( parent );
  }

  /**
   * @param parent
   * @param style
   */
  public DefaultTableViewer( Composite parent, int style )
  {
    super( parent, style );
  }

  /**
   * @param table
   */
  public DefaultTableViewer( Table table )
  {
    super( table );
  }

  /**
   * Adds a column to the underlying table control.
   * 
   * @param name
   * @param title
   * @param width
   * @param isEditable
   * @return the newly added column
   */
  protected TableColumn addColumn( final String name, final String title, final int width, final boolean isEditable )
  {
    final Table table = getTable();
    
    final TableColumn tc = new TableColumn( table, SWT.CENTER );
    tc.setData( COLUMN_PROP_NAME, name );
    tc.setData( COLUMN_PROP_EDITABLE, Boolean.valueOf( isEditable ) );
    tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
    tc.setWidth( width );

    tc.setText( title );
    
    return tc;
  }
  
  /**
   * Refreshes the column properties according to the current list of columns
   * 
   * @see org.eclipse.jface.viewers.TableViewer#getColumnProperties()
   */
  protected void refreshColumnProperties()
  {
    final Table table = getTable();
    if( table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    final String[] properties = new String[columns.length];

    for( int i = 0; i < properties.length; i++ )
      properties[i] = columns[i].getData( COLUMN_PROP_NAME ).toString();

    setColumnProperties( properties );
  }
}
