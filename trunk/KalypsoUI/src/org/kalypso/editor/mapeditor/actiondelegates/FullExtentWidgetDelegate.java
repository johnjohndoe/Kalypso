package org.kalypso.editor.mapeditor.actiondelegates;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.editor.mapeditor.GisMapEditor;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.widgets.ChangeExtentCommand;
import org.kalypso.util.command.CommandJob;

/**
 * @author belger
 */
public class FullExtentWidgetDelegate implements IEditorActionDelegate
{
  private GisMapEditor m_editor;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    m_editor = (GisMapEditor)targetEditor;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( m_editor == null )
      return;
    
    final MapModell modell = m_editor.getMapModell();
    if( modell == null )
      return;

    final GM_Envelope fullExtent = modell.getFullExtentBoundingBox();
    new CommandJob( new ChangeExtentCommand( modell, fullExtent ), m_editor.getCommandManager(), m_editor.getSchedulingRule(), null, CommandJob.POST );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    // nix tun
  }
}
