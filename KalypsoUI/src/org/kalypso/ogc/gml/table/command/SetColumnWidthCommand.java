package org.kalypso.ogc.gml.table.command;

import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.util.command.ICommand;

/**
 * Kommando zum ändern der Spaltenbreite. Es wird davon ausgegangen, dass die
 * Breite der Spalte (d.h. des Widgets) bereits gesetzt wurde.
 * 
 * @author Belger
 */
public class SetColumnWidthCommand implements ICommand
{
  private final TableColumn m_tableColumn;

  private final int m_oldWidth;

  private final int m_newWidth;

  public SetColumnWidthCommand( final TableColumn tableColumn, final int width )
  {
    m_tableColumn = tableColumn;

    m_newWidth = width;
    m_oldWidth = ( (Integer)tableColumn.getData( LayerTableViewer.COLUMN_PROP_WIDTH ) ).intValue();
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    setWidth( m_newWidth, false );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    setWidth( m_newWidth, true );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    setWidth( m_oldWidth, true );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Spaltenbreite ändern";
  }

  private void setWidth( final int width, final boolean bSetControlWidth )
  {
    final TableColumn tableColumn = m_tableColumn;
    if( !tableColumn.isDisposed() )
    {
      m_tableColumn.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          tableColumn.setData( LayerTableViewer.COLUMN_PROP_WIDTH, new Integer( width ) );
          if( bSetControlWidth )
            tableColumn.setWidth( width );
        }
      } );
    }
  }

}