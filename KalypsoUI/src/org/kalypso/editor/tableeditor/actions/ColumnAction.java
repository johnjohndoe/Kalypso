package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.action.Action;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.command.SetColumnVisibleCommand;
import org.kalypso.util.command.ICommandTarget;

public final class ColumnAction extends Action
{
  private final FeatureTypeProperty m_ftp;

  private final LayerTable m_layerTable;

  private ICommandTarget m_commandTarget;

  public ColumnAction( final ICommandTarget commandTarget, final LayerTable layerTable,
      final FeatureTypeProperty ftp, final boolean bVisible )
  {
    super( ftp.getName() );

    m_ftp = ftp;
    m_commandTarget = commandTarget;
    m_layerTable = layerTable;

    setChecked( bVisible );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final SetColumnVisibleCommand setColumnVisibleCommand = new SetColumnVisibleCommand(
        m_layerTable.getModel(), m_ftp, isChecked() );

    m_commandTarget.postCommand( setColumnVisibleCommand, null );
  }
}