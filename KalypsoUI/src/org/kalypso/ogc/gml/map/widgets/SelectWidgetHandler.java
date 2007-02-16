package org.kalypso.ogc.gml.map.widgets;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.views.map.MapView;
import org.osgi.framework.Bundle;

/**
 * This abstract {@link IHandler} implementation
 * 
 * @author Stefan Kurzbach
 */
public class SelectWidgetHandler extends AbstractHandler implements IHandler
{

  @Override
  public Object execute( final ExecutionEvent event )
  {
    final String widgetParameter = event.getParameter( SelectWidgetCommandActionDelegate.PARAM_WIDGET_CLASS );
    final String pluginParameter = event.getParameter( SelectWidgetCommandActionDelegate.PARAM_PLUGIN_ID );
    final IWidget widget = getWidgetFromBundle( pluginParameter, widgetParameter );
    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();    
    final IEditorPart editor = (IEditorPart) applicationContext.getVariable( ISources.ACTIVE_EDITOR_NAME );
    final IWorkbenchPart workbenchPart;
    if( editor != null && editor.getEditorSite().getId().equals( GisMapEditor.ID ) )
    {
      workbenchPart = editor;
    }
    else
    {
      final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) applicationContext.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
      final IViewPart mapView = workbenchWindow.getActivePage().findView( MapView.ID );
      workbenchPart = mapView;
    }
    
    if( workbenchPart != null )
    {
      final MapPanel mapPanel = (MapPanel) workbenchPart.getAdapter( MapPanel.class );
      if( mapPanel != null && widget != null )
      {
        mapPanel.getWidgetManager().setActualWidget( widget );
      }
    }
    return null;
  }

  private IWidget getWidgetFromBundle( final String pluginId, final String widgetName )
  {
    try
    {
      final Bundle bundle = Platform.getBundle( pluginId );
      final Class widgetClass = bundle.loadClass( widgetName );
      return (IWidget) widgetClass.newInstance();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

}
