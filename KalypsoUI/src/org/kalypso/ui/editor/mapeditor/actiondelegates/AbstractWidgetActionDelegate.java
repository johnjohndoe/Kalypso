package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

/**
 * 
 * 
 * @author bce
 */
public abstract class AbstractWidgetActionDelegate implements IEditorActionDelegate
{
  private IWidget m_widget;

  private GisMapEditor m_editor;

  private MapPanel myActualMapPanel;

  public AbstractWidgetActionDelegate( final AbstractWidget widget )
  {
    m_widget = widget;
  }

  public IWidget getWidget()
  {
    return m_widget;
  }

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    // wenn der editor wechselt:
    // alte view widget entfernen
    if( myActualMapPanel != null )
      myActualMapPanel.changeWidget( null );

    if( targetEditor != null )
    {
      m_editor = (GisMapEditor)targetEditor;
      myActualMapPanel = m_editor.getMapPanel();
      if( myActualMapPanel != null )
      {
        if( action.isChecked() )
          myActualMapPanel.changeWidget( m_widget );
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    // wenn der Button wechselt aber editor gleich bleibt
    if( action.isChecked() && myActualMapPanel.getActualWidget() != m_widget )
      myActualMapPanel.changeWidget( m_widget );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // nix tun?
  }
}