package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

/**
 * @author belger
 */
public abstract class AbstractThemeDelegate implements IEditorActionDelegate, ModellEventListener
{
  private GisMapEditor m_editor;

  private IAction m_action;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    m_action = action;

    if( m_editor != null )
    {
      final IMapModell mapModell = m_editor.getMapPanel().getMapModell();
      if( mapModell != null )
        mapModell.removeModellListener( this );
    }

    m_editor = (GisMapEditor)targetEditor;

    if( m_editor != null )
    {
      final IMapModell mapModell = m_editor.getMapPanel().getMapModell();
      if( mapModell != null )
        mapModell.addModellListener( this );
    }

    refreshAction();
  }
  
  protected IAction getAction()
  {
    return m_action;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
  }

  protected abstract void refreshAction();

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    refreshAction();
  }

  protected GisMapEditor getEditor()
  {
    return m_editor;
  }
}