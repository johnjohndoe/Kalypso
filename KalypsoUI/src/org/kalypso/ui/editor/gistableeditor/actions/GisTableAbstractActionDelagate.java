package org.kalypso.ui.editor.gistableeditor.actions;

import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;

/**
 * @author belger
 */
public abstract class GisTableAbstractActionDelagate implements IEditorActionDelegate, ModellEventListener
{
  private GisTableEditor m_editor;

  private IAction m_action;
  
  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    m_action = action;

    if( m_editor != null )
      m_editor.getLayerTable().removeModellListener( this );

    m_editor = (GisTableEditor)targetEditor;

    if( m_editor != null )
      m_editor.getLayerTable().addModellListener( this );

    refreshAction();
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
  
  protected GisTableEditor getEditor()
  {
    return m_editor;
  }
  
  protected IAction getAction()
  {
    return m_action;
  }
  
  /**
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( final IAction action )
  {
    m_action = action;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    refreshAction();
  }

}