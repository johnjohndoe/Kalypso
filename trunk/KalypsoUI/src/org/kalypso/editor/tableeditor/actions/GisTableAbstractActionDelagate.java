package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.editor.tableeditor.GisTableEditor;

/**
 * @author belger
 */
public abstract class GisTableAbstractActionDelagate implements IActionDelegate2,
    IEditorActionDelegate, ISelectionChangedListener
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
    action.setEnabled( bEnabled );
    
    final boolean bChecked = m_editor == null ? false : isChecked();
    action.setChecked(bChecked);
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

  /**
   * @see org.eclipse.ui.IActionDelegate2#dispose()
   */
  public void dispose()
  {
    if( m_editor != null )
      m_editor.removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( final IAction action )
  {
    // nichts zu tun  
  }

  public void selectionChanged( final SelectionChangedEvent event )
  {
    final boolean bEnabled = m_editor == null ? false : isEnabled( m_editor.getSelection() );
    m_action.setEnabled( bEnabled );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  public void runWithEvent( final IAction action, final Event event )
  {
    // event wird ignoriert

    run( action );
  }

  protected abstract boolean isEnabled( final ISelection selection );
  protected abstract boolean isChecked();
}