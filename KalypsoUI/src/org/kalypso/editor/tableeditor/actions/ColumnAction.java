package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.Action;
import org.kalypso.editor.tableeditor.layerTable.LayerTableViewer;
import org.kalypso.editor.tableeditor.layerTable.command.SetColumnVisibleCommand;

public final class ColumnAction extends Action
{
  private final LayerTableViewer m_viewer;
  private final String m_propertyName;

  public ColumnAction( final LayerTableViewer viewer,
      final String propertyName )
  {
    super( propertyName );

    m_viewer = viewer;
    m_propertyName = propertyName;

    setChecked( viewer.hasColumn( propertyName ) );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final SetColumnVisibleCommand setColumnVisibleCommand = new SetColumnVisibleCommand(
        m_viewer, m_propertyName, isChecked() );

    m_viewer.postCommand( setColumnVisibleCommand, null );
  }
}