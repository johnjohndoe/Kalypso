package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.action.Action;
import org.kalypso.editor.tableeditor.GisTableEditor;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.command.SetColumnVisibleCommand;
import org.kalypso.util.command.CommandJob;


public final class ColumnAction extends Action
{
  private final FeatureTypeProperty m_ftp;

  private final GisTableEditor m_editor;

  private final LayerTable m_layerTable;

  public ColumnAction( final GisTableEditor editor, final LayerTable layerTable,
      final FeatureTypeProperty ftp, final boolean bVisible )
  {
    super( ftp.getName() );

    m_ftp = ftp;
    m_editor = editor;
    m_layerTable = layerTable;

    setChecked( bVisible );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    try
    {
      final SetColumnVisibleCommand setColumnVisibleCommand = new SetColumnVisibleCommand( m_layerTable.getModel(), m_ftp,
          isChecked() );
      
      new CommandJob( setColumnVisibleCommand, m_editor.getCommandManager(), m_editor.getSchedulingRule(), null, CommandJob.POST );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
}