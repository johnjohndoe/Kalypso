package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.util.command.CommandJob;

/**
 * @author belger
 */
public class UndoRedoDelegate extends AbstractThemeDelegate implements ModellEventListener
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
    final GisMapEditor editor = getEditor();
    if( editor == null )
      return;

    final IMapModell mapModell = editor.getMapPanel().getMapModell();
    if( mapModell != null )
    {
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      if( activeTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)activeTheme;

        final CommandableWorkspace workspace = theme.getWorkspace();

        if( ( m_undo && workspace.canUndo()) || ( !m_undo && workspace.canRedo() ) )
          new CommandJob( null, workspace, theme.getSchedulingRule(), null, m_undo ? CommandJob.UNDO  : CommandJob.REDO );
      }
    }

    refreshAction();
  }

  protected void refreshAction()
  {
    boolean bEnabled = false;

    final GisMapEditor editor = getEditor();
    if( editor != null )
    {
      final IMapModell mapModell = editor.getMapPanel().getMapModell();
      if( mapModell != null )
      {
        final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
        if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)activeTheme;
          final CommandableWorkspace workspace = theme.getWorkspace();
          if( workspace != null )
            bEnabled = m_undo ? workspace.canUndo() : workspace.canRedo();
        }
      }
    }

    if( getAction() != null )
      getAction().setEnabled( bEnabled );
  }
}