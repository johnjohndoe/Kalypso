package org.kalypso.ogc.gml.table.command;

import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.ogc.gml.table.LayerTableSorter;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.util.command.ICommand;

/**
 * @author gernot
 */
public class ChangeSortingCommand implements ICommand
{
  private final LayerTableViewer m_viewer;

  private final LayerTableSorter m_sorter;

  private final String m_oldPropertyName;

  private final String m_newPropertyName;

  private final boolean m_oldInverse;

  private final boolean m_newInverse;

  public ChangeSortingCommand( final LayerTableViewer viewer, final TableColumn tableColumn )
  {
    m_viewer = viewer;

    final String propertyName = (String)tableColumn.getData( LayerTableViewer.COLUMN_PROP_NAME );

    m_sorter = (LayerTableSorter)m_viewer.getSorter();

    m_oldPropertyName = m_sorter.getPropertyName();
    m_oldInverse = m_sorter.isInverse();

    if( m_oldPropertyName != null && m_oldPropertyName.equals( propertyName ) )
    {
      m_newInverse = !m_oldInverse;
      m_newPropertyName = m_oldPropertyName;
    }
    else
    {
      m_newInverse = false;
      m_newPropertyName = propertyName;
    }
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
    changeSorter( m_newInverse, m_newPropertyName );
  }

  private void changeSorter( final boolean bInverse, final String propertyName )
  {
    m_sorter.setInverse( bInverse );
    m_sorter.setPropertyName( propertyName );

    final LayerTableViewer viewer = m_viewer;
    viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.refresh();
      }
    } );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    changeSorter( m_oldInverse, m_oldPropertyName );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return ( m_newInverse ? "absteigend" : "aufsteigend" ) + " sortieren nach: " + m_newPropertyName;
  }

}