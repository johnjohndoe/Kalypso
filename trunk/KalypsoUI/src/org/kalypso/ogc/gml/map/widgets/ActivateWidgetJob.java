package org.kalypso.ogc.gml.map.widgets;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.editor.mapeditor.views.MapWidgetView;

/**
 * Activates a given {@link IWidget} (code extracted from {@link SelectWidgetHandler})
 * 
 * @author Thomas Jung
 */
public final class ActivateWidgetJob extends UIJob
{
  private final IWidget m_widget;

  private final MapPanel m_mapPanel;

  private final IWorkbenchPart m_activePart;

  public ActivateWidgetJob( final Display jobDisplay, final String name, final IWidget widget, final MapPanel mapPanel, final IWorkbenchPart activePart )
  {
    super( jobDisplay, name );
    m_widget = widget;
    m_mapPanel = mapPanel;
    m_activePart = activePart;
  }

  @Override
  public IStatus runInUIThread( final IProgressMonitor monitor )
  {
    try
    {
      if( m_widget instanceof IWidgetWithOptions )
      {
        final MapWidgetView widgetView = (MapWidgetView) m_activePart.getSite().getPage().showView( MapWidgetView.ID, null, IWorkbenchPage.VIEW_VISIBLE );
        widgetView.setWidgetForPanel( m_mapPanel, (IWidgetWithOptions) m_widget );
      }
      else
        m_mapPanel.getWidgetManager().setActualWidget( m_widget );
    }
    catch( final PartInitException e )
    {
      return e.getStatus();
    }

    return Status.OK_STATUS;
  }
}