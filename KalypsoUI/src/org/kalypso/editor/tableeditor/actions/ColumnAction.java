package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.action.Action;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.command.SetColumnVisibleCommand;
import org.kalypso.util.command.ICommandManager;


public final class ColumnAction extends Action
{
  private final FeatureTypeProperty m_ftp;

  private final ICommandManager m_commandManager;

  private final LayerTable m_layerTable;

  public ColumnAction( final ICommandManager commandManager, final LayerTable layerTable,
      final FeatureTypeProperty ftp, final boolean bVisible )
  {
    super( ftp.getName() );

    m_ftp = ftp;
    m_commandManager = commandManager;
    m_layerTable = layerTable;

    setChecked( bVisible );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    m_commandManager.postCommand( new SetColumnVisibleCommand( m_layerTable.getModel(), m_ftp,
        isChecked() ), null );
  }
}