package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.editor.tableeditor.GisTableEditor;

/**
 * @author belger
 */
public abstract class GisTableAbstractActionDelagate implements IEditorActionDelegate, ISelectionChangedListener
{
  private GisTableEditor m_editor;
  private IAction m_action;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    if( m_editor != null )
      m_editor.removeSelectionChangedListener( this );
    
    m_editor = (GisTableEditor)targetEditor;
    m_action = action;
    
    if( m_editor != null )
      m_editor.addSelectionChangedListener( this );

    final boolean bEnabled = m_editor == null ? false : isEnabled( m_editor.getSelection() );
    action.setEnabled(bEnabled);
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    // nichts tun
  }

  public GisTableEditor getEditor()
  {
    return m_editor;
  }
  
  public void selectionChanged( final SelectionChangedEvent event )
  {
    final boolean bEnabled = m_editor == null ? false : isEnabled( m_editor.getSelection() );
    m_action.setEnabled(bEnabled);
  }

  protected abstract boolean isEnabled( final ISelection selection );
}