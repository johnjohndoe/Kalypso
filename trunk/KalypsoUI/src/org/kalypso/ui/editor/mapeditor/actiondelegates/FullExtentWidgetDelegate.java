package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

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