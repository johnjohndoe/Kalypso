package org.kalypso.ui.editor.gistableeditor.actions;

import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;
import org.kalypso.util.command.CommandJob;

/**
 * @author belger
 */
public class UndoRedoDelegate extends GisTableAbstractActionDelagate implements ModellEventListener
{
  private final boolean m_undo;

  public UndoRedoDelegate( final boolean undo )
  {
    m_undo = undo;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final GisTableEditor editor = getEditor();
    if( editor == null )
      return;

    final IKalypsoFeatureTheme theme = editor.getLayerTable().getTheme();

    final CommandableWorkspace workspace = theme.getWorkspace();

    if( ( m_undo && workspace.canUndo() ) || ( !m_undo && workspace.canRedo() ) )
      new CommandJob( null, workspace, theme.getSchedulingRule(), null, m_undo ? CommandJob.UNDO
          : CommandJob.REDO );

    refreshAction();
  }

  protected void refreshAction()
  {
    boolean bEnabled = false;

    final GisTableEditor editor = getEditor();
    if( editor != null )
    {
      final IKalypsoFeatureTheme theme = editor.getLayerTable().getTheme();
      if( theme != null )
      {
        final CommandableWorkspace workspace = theme.getWorkspace();
        if( workspace != null )
          bEnabled = m_undo ? workspace.canUndo() : workspace.canRedo();
      }
    }

    if( getAction() != null )
      getAction().setEnabled( bEnabled );
  }
}