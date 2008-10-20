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
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
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
  public static final String COMMAND_ID = "org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand"; //$NON-NLS-1$

  public static final String PARAM_CONTEXT = COMMAND_ID + ".context"; //$NON-NLS-1$

  public static final String PARAM_WIDGET_CLASS = COMMAND_ID + ".widget"; //$NON-NLS-1$

  public static final String PARAM_PLUGIN_ID = COMMAND_ID + ".plugin"; //$NON-NLS-1$

  public static final String PARAM_WIDGET_ICON = COMMAND_ID + ".icon"; //$NON-NLS-1$

  private static final Object PARAM_WIDGET_TOOLTIP = COMMAND_ID + ".tooltip"; //$NON-NLS-1$

  private String m_widgetClassFromExtension;

  private String m_pluginIdFromExtension;

  private String m_widgetIconFromExtension;

  private String m_widgetTooltipFromExtension;

  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();

    if( isDeselecting( event.getTrigger() ) )
      return null;

    final String widgetFromEvent = event.getParameter( PARAM_WIDGET_CLASS );
    final String widgetParameter;
    if( widgetFromEvent != null )
      widgetParameter = widgetFromEvent;
    else
      widgetParameter = m_widgetClassFromExtension;

    final String pluginFromEvent = event.getParameter( PARAM_PLUGIN_ID );
    final String pluginParameter;
    if( pluginFromEvent != null )
      pluginParameter = pluginFromEvent;
    else
      pluginParameter = m_pluginIdFromExtension;

    final IWidget widget = getWidgetFromBundle( pluginParameter, widgetParameter );
    if( widget == null )
    {
      final String msg = String.format( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectWidgetHandler.6" ), pluginParameter, widgetParameter ); //$NON-NLS-1$
      final IStatus status = StatusUtilities.createWarningStatus( msg );
      KalypsoGisPlugin.getDefault().getLog().log( status );
      return status;
    }

    final Shell shell = (Shell) applicationContext.getVariable( ISources.ACTIVE_SHELL_NAME );

    final IWorkbenchWindow window = (IWorkbenchWindow) applicationContext.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final AbstractMapPart abstractMapPart = findMapPart( window );
    final IMapPanel mapPanel = abstractMapPart == null ? null : abstractMapPart.getMapPanel();
    if( mapPanel == null )
      return StatusUtilities.createStatus( IStatus.WARNING, Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectWidgetHandler.7" ), new IllegalStateException() ); //$NON-NLS-1$

    /* Always make sure that the map was fully loaded */
    // REMARK: we first test directly, without ui-operation, in order to enhance performance if the map already is open.
    final IMapModell model = mapPanel.getMapModell();
    if( !MapModellHelper.isMapLoaded( model ) )
    {
      // TODO: this is too slow here!
      if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "", "" ) ) //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }

    final UIJob job = new ActivateWidgetJob( Messages.getString( "org.kalypso.ogc.gml.map.widgets.SelectWidgetHandler.10" ), widget, mapPanel, abstractMapPart ); //$NON-NLS-1$
    // Probably not necessary
    job.setRule( abstractMapPart.getSchedulingRule().getSelectWidgetSchedulingRule() );
    job.schedule();

    return null;
  }

  /**
   * Checks if this command was executed as de-selection of a radio button/menu.<br>
   * If this is the case, we just ignore it.<br>
   * In doubt, we always execute.
   */
  private boolean isDeselecting( final Object trigger )
  {
    if( !(trigger instanceof Event) )
      return false;

    final Event event = (Event) trigger;
    final Widget widget = event.widget;
    if( widget instanceof ToolItem )
    {
      final ToolItem item = (ToolItem) widget;
      return !item.getSelection();
    }
    if( widget instanceof MenuItem )
    {
      final MenuItem item = (MenuItem) widget;
      return !item.getSelection();
    }

    return false;
  }

  /**
   * Search for a workbench part in the active page of the given window that is an {@link AbstractMapPart}.<br>
   * Search order is: active part, view parts, editor parts.
   */
  private AbstractMapPart findMapPart( final IWorkbenchWindow window )
  {
    if( window == null )
      return null;

    final IWorkbenchPage page = window.getActivePage();
    if( page == null )
      return null;

    final IWorkbenchPart activePart = page.getActivePart();
    if( activePart instanceof AbstractMapPart )
      return (AbstractMapPart) activePart;

    final IViewReference[] viewReferences = page.getViewReferences();
    for( final IViewReference viewReference : viewReferences )
    {
      final IWorkbenchPart part = viewReference.getView( false );
      if( part instanceof AbstractMapPart )
        return (AbstractMapPart) part;
    }

    final IEditorReference[] editorReferences = page.getEditorReferences();
    for( final IEditorReference editorReference : editorReferences )
    {
      final IWorkbenchPart part = editorReference.getEditor( false );
      if( part instanceof AbstractMapPart )
        return (AbstractMapPart) part;
    }

    return null;
  }

  private ImageDescriptor getIconFromBundle( final String pluginId, final String imageFilePath )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( pluginId, imageFilePath );
  }

  @SuppressWarnings("unchecked")
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
      final Map< ? , ? > parameterMap = (Map< ? , ? >) data;
      m_pluginIdFromExtension = (String) parameterMap.get( PARAM_PLUGIN_ID );
      m_widgetClassFromExtension = (String) parameterMap.get( PARAM_WIDGET_CLASS );
      m_widgetIconFromExtension = (String) parameterMap.get( PARAM_WIDGET_ICON );
      m_widgetTooltipFromExtension = (String) parameterMap.get( PARAM_WIDGET_TOOLTIP );
    }

    if( m_pluginIdFromExtension == null )
      m_pluginIdFromExtension = config.getContributor().getName();
  }

  /**
   * @see org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.menus.UIElement, java.util.Map)
   */
  @SuppressWarnings("unchecked")
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

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    if( window == null )
      return;

    final IWorkbenchPart activePart = window.getPartService().getActivePart();
    if( activePart == null )
      return;

    final IMapPanel mapPanel = (IMapPanel) activePart.getAdapter( IMapPanel.class );
    if( mapPanel != null )
    {
      final IWidget actualWidget = mapPanel.getWidgetManager().getActualWidget();
      // SelectWidgetHandler ist mehrfach vorhanden, daher muss explizit auf die Klasse des Widgets geprüft werden.
      if( actualWidget != null )
      {
        final String actualWidgetClass = actualWidget.getClass().getName();
        if( actualWidgetClass.equals( m_widgetClassFromExtension ) )
          element.setChecked( true );
        else
          element.setChecked( false );
      }
      else
        element.setChecked( false );
    }

  }
}
