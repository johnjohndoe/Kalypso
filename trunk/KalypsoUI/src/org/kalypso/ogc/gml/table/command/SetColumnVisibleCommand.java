package org.kalypso.ogc.gml.table.command;

import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.util.command.ICommand;

/**
 * @author Belger
 */
public class SetColumnVisibleCommand implements ICommand
{
  private final String m_propertyName;

  private final boolean m_bVisible;

  private final LayerTableViewer m_viewer;

  private final boolean m_wasEditable;

  private final int m_oldWidth;

  public SetColumnVisibleCommand( final LayerTableViewer viewer, final String propertyName,
      final boolean bVisible )
  {
    m_viewer = viewer;
    m_propertyName = propertyName;
    m_bVisible = bVisible;
    m_wasEditable = viewer.isEditable( propertyName );
    m_oldWidth = viewer.getWidth( propertyName );
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
    doIt( m_viewer, m_propertyName, m_bVisible, 100, true );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    doIt( m_viewer, m_propertyName, m_bVisible, 100, true );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    doIt( m_viewer, m_propertyName, !m_bVisible, m_oldWidth, m_wasEditable );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Spalte '" + m_propertyName + "' " + ( m_bVisible ? "anzeigen" : "verstecken" );
  }
  
  private void doIt( final LayerTableViewer viewer, final String propertyName, final boolean bVisible, final int width, final boolean editable )
  {
    m_viewer.getControl().getDisplay().syncExec( new Runnable()
    {
      public void run()
      {
        if( bVisible )
          viewer.addColumn( propertyName, width, editable, true );
        else
          viewer.removeColumn( propertyName );
      }
    } );
  }
}