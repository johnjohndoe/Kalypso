package org.kalypso.editor.mapeditor.actiondelegates;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.editor.mapeditor.GisMapEditor;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.ChangeExtentCommand;

/**
 * @author belger
 */
public class FullExtentWidgetDelegate implements IEditorActionDelegate
{
  private GisMapEditor m_editor;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
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

    final MapPanel mapPanel = m_editor.getMapPanel();
    if( mapPanel == null )
      return;
    final IMapModell modell = mapPanel.getMapModell();

    final GM_Envelope fullExtent = modell.getFullExtentBoundingBox();
    m_editor.postCommand( new ChangeExtentCommand( mapPanel, fullExtent ), null );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // nix tun
  }
}