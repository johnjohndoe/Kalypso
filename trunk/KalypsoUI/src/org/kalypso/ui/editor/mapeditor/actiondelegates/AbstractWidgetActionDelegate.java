package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

/**
 * 
 * 
 * @author bce
 */
public abstract class AbstractWidgetActionDelegate implements IEditorActionDelegate
{
  private final String m_widgetID;
  
  private GisMapEditor m_editor;

  private MapPanel myActualMapPanel;

  public AbstractWidgetActionDelegate( final String widgetID )
  {
    m_widgetID = widgetID;
  }

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    if( targetEditor != null )
    {
      m_editor = (GisMapEditor)targetEditor;
      myActualMapPanel = m_editor.getMapPanel();
      if( myActualMapPanel != null )
          action.setChecked( m_widgetID.equals( myActualMapPanel.getActualWidgetID() ) );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( action.isChecked() && !m_widgetID.equals( myActualMapPanel.getActualWidgetID() ) )
      myActualMapPanel.changeWidget( m_widgetID );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // nix tun?
    action.getClass();
  }
}