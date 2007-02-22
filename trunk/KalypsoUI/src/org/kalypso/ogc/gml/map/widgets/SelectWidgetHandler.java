package org.kalypso.ogc.gml.map.widgets;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
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
public class SelectWidgetHandler extends AbstractHandler implements IHandler, IExecutableExtension
{
  private String m_widgetClassFromExtension;

  private String m_pluginIdFromExtension;

  @Override
  public Object execute( final ExecutionEvent event )
  {
    final String widgetFromEvent = event.getParameter( SelectWidgetCommandActionDelegate.PARAM_WIDGET_CLASS );
    final String widgetParameter;
    if( widgetFromEvent != null )
    {
      widgetParameter = widgetFromEvent;
    }
    else
    {
      widgetParameter = m_widgetClassFromExtension;
    }

    final String pluginFromEvent = event.getParameter( SelectWidgetCommandActionDelegate.PARAM_PLUGIN_ID );
    final String pluginParameter;
    if( pluginFromEvent != null )
    {
      pluginParameter = pluginFromEvent;
    }
    else
    {
      pluginParameter = m_pluginIdFromExtension;
    }
    final IWidget widget = getWidgetFromBundle( pluginParameter, widgetParameter );
    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();
    final IEditorPart editor = (IEditorPart) applicationContext.getVariable( ISources.ACTIVE_EDITOR_NAME );
    IWorkbenchPart workbenchPart = null;
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) applicationContext.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    if( workbenchWindow == null )
    {
      return null;
    }

    final IWorkbenchPage activePage = workbenchWindow.getActivePage();
    if( editor != null && editor.getEditorSite().getId().equals( GisMapEditor.ID ) )
    {
      workbenchPart = editor;
    }
    else
    {
      final IViewPart mapView = activePage.findView( MapView.ID );
      workbenchPart = mapView;
    }

    if( workbenchPart != null )
    {
      activePage.activate( workbenchPart );
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

  /**
   * @see org.kalypso.ui.GenericCommandActionDelegate#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data ) throws CoreException
  {
    if( data != null && data instanceof Map )
    {
      Map parameterMap = (Map) data;
      m_widgetClassFromExtension = (String) parameterMap.get( SelectWidgetCommandActionDelegate.PARAM_WIDGET_CLASS );
      m_pluginIdFromExtension = (String) parameterMap.get( SelectWidgetCommandActionDelegate.PARAM_PLUGIN_ID );
    }
  }

}
