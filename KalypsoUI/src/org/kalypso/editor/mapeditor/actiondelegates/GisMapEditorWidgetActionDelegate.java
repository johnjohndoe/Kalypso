package org.kalypso.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.editor.mapeditor.GisMapEditor;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.widgets.AbstractWidget;
import org.kalypso.ogc.widgets.IWidget;
import org.kalypso.ogc.widgets.WidgetManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * 
 * 
 * @author bce
 */
public abstract class GisMapEditorWidgetActionDelegate implements IEditorActionDelegate,
    ICommandTarget
{
  private IWidget m_widget;

  private GisMapEditor m_editor;

  private WidgetManager m_widgetManager;

  private MapPanel myActualMapPanel;

  public GisMapEditorWidgetActionDelegate( final AbstractWidget widget )
  {
    m_widget = widget;

    widget.setCommandPoster( this );
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
    if( m_widgetManager != null )
      m_widgetManager.changeWidget( null );

    // alte view WIDGET_CHANGE senden: TODO: WARUM????
    if( myActualMapPanel != null )
      myActualMapPanel.getMapModell()
          .fireModellEvent( new ModellEvent( null, ModellEvent.WIDGET_CHANGE ) );

    if( targetEditor != null )
    {
      m_editor = (GisMapEditor)targetEditor;
      myActualMapPanel = m_editor.getMapPanel();
      if( myActualMapPanel != null )
      {
        m_widgetManager = myActualMapPanel.getWidgetManager();
        if( action.isChecked() )
          m_widgetManager.changeWidget( m_widget );
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    // wenn der Button wechselt aber editor gleich bleibt
    if( action.isChecked() && m_widgetManager.getActualWidget() != m_widget )
      m_widgetManager.changeWidget( m_widget );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // nix tun?
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public final void postCommand( final ICommand command, final Runnable runnable )
  {
    m_editor.postCommand( command, runnable );
  }
}