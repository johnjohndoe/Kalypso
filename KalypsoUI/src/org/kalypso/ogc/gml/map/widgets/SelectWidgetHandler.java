package org.kalypso.ogc.gml.map.widgets;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.views.map.MapView;
import org.osgi.framework.Bundle;

/**
 * This abstract {@link IHandler} implementation
 * 
 * @author Stefan Kurzbach
 */
public class SelectWidgetHandler extends AbstractHandler implements IHandler, IElementUpdater, IExecutableExtension
{
  public static final String COMMAND_ID = "org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand";

  public static final String PARAM_CONTEXT = COMMAND_ID + ".context";

  public static final String PARAM_WIDGET_CLASS = COMMAND_ID + ".widget";

  public static final String PARAM_PLUGIN_ID = COMMAND_ID + ".plugin";

  public static final String PARAM_WIDGET_ICON = COMMAND_ID + ".icon";

  private static final Object PARAM_WIDGET_TOOLTIP = COMMAND_ID + ".tooltip";

  private String m_widgetClassFromExtension;

  private String m_pluginIdFromExtension;

  private String m_widgetIconFromExtension;

  private String m_widgetTooltipFromExtension;

  @Override
  public Object execute( final ExecutionEvent event )
  {
    final String widgetFromEvent = event.getParameter( PARAM_WIDGET_CLASS );
    final String widgetParameter;
    if( widgetFromEvent != null )
    {
      widgetParameter = widgetFromEvent;
    }
    else
    {
      widgetParameter = m_widgetClassFromExtension;
    }

    final String pluginFromEvent = event.getParameter( PARAM_PLUGIN_ID );
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
    AbstractMapPart abstractMapPart = null;
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) applicationContext.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    if( workbenchWindow == null )
    {
      return null;
    }

    final IWorkbenchPage activePage = workbenchWindow.getActivePage();
    if( editor != null && editor.getEditorSite().getId().equals( GisMapEditor.ID ) )
    {
      abstractMapPart = (AbstractMapPart) editor;
    }
    else
    {
      final IViewPart mapView = activePage.findView( MapView.ID );
      abstractMapPart = (AbstractMapPart) mapView;
    }

    if( abstractMapPart != null )
    {
      activePage.activate( abstractMapPart );
      final MapPanel mapPanel = (MapPanel) abstractMapPart.getAdapter( MapPanel.class );
      if( mapPanel != null && widget != null )
      {
        final UIJob job = new UIJob( workbenchWindow.getShell().getDisplay(), "Widget auswählen" )
        {
          @Override
          public IStatus runInUIThread( IProgressMonitor monitor )
          {
            mapPanel.getWidgetManager().setActualWidget( widget );
            return Status.OK_STATUS;
          }
        };
        job.setRule( abstractMapPart.getSchedulingRule().getSelectWidgetSchedulingRule() );
        job.setUser( true );
        job.schedule();
      }
    }
    return null;
  }

  private ImageDescriptor getIconFromBundle( final String pluginId, final String imageFilePath )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( pluginId, imageFilePath );
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
      m_pluginIdFromExtension = (String) parameterMap.get( PARAM_PLUGIN_ID );
      m_widgetClassFromExtension = (String) parameterMap.get( PARAM_WIDGET_CLASS );
      m_widgetIconFromExtension = (String) parameterMap.get( PARAM_WIDGET_ICON );
      m_widgetTooltipFromExtension = (String) parameterMap.get( PARAM_WIDGET_TOOLTIP );
    }
    if( m_pluginIdFromExtension == null )
    {
      m_pluginIdFromExtension = config.getContributor().getName();
    }
    
  }

  /**
   * @see org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.menus.UIElement, java.util.Map)
   */
  public void updateElement( final UIElement element, final Map parameters )
  {
    if( m_widgetIconFromExtension != null )
    {
      final ImageDescriptor iconFromBundle = getIconFromBundle( m_pluginIdFromExtension, m_widgetIconFromExtension );
      element.setIcon( iconFromBundle );
    }
    if( m_widgetTooltipFromExtension != null )
    {
      element.setTooltip( m_widgetTooltipFromExtension );
    }
  }
}
