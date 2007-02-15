package org.kalypso.ogc.gml.map.widgets;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
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
    final IWorkbenchPart workbenchPart = (IWorkbenchPart) applicationContext.getVariable( ISources.ACTIVE_PART_NAME );    
    final MapPanel mapPanel = (MapPanel) workbenchPart.getAdapter( MapPanel.class );
    if( mapPanel != null && widget != null )
    {
      mapPanel.getWidgetManager().setActualWidget( widget );
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
