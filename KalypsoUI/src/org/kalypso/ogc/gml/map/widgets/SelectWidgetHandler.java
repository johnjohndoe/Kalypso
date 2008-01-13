package org.kalypso.ogc.gml.map.widgets;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.osgi.framework.Bundle;

/**
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
    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();

    // TODO: this gets called twice if radio buttons are involved
    // it would be nice to find out the check state of the command
    // Maybe use Command#setState / #getState to do this?

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
    if( widget == null )
    {
      final String msg = String.format( "Widget cannot be selected. PluginParameter: %s   WidgetParameter: %s", pluginParameter, widgetParameter );
      final IStatus status = StatusUtilities.createWarningStatus( msg );
      KalypsoGisPlugin.getDefault().getLog().log( status );
      return status;
    }

    final IWorkbenchPart activePart = (IWorkbenchPart) applicationContext.getVariable( ISources.ACTIVE_PART_NAME );
    final Shell shell = (Shell) applicationContext.getVariable( ISources.ACTIVE_SHELL_NAME );
    final Display display = shell.isDisposed() ? activePart.getSite().getShell().getDisplay() : shell.getDisplay();
    final MapPanel mapPanel = activePart == null ? null : (MapPanel) activePart.getAdapter( MapPanel.class );
    if( mapPanel == null )
      return StatusUtilities.createWarningStatus( "No map panel available" );

    /* Always make sure that the map was fully loaded */
    // TODO: this is too slow here!
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "", "" ) )
      return null;

    final UIJob job = new ActivateWidgetJob( display, "Widget auswählen", widget, mapPanel, activePart );
    // Probably not necessary
    final AbstractMapPart abstractMapPart = (AbstractMapPart) activePart;
    job.setRule( abstractMapPart.getSchedulingRule().getSelectWidgetSchedulingRule() );
    job.schedule();

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
      final Class<IWidget> widgetClass = bundle.loadClass( widgetName );
      return widgetClass.newInstance();
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
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data != null && data instanceof Map )
    {
      final Map parameterMap = (Map) data;
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
