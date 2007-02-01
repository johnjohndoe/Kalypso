package org.kalypso.ogc.gml.map.widgets;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * This abstract {@link IHandler} implementation 
 * @author Stefan Kurzbach
 */
public abstract class SelectWidgetHandler extends AbstractHandler implements IHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final String widgetParameter = event.getParameter( SelectWidgetCommandActionDelegate.PARAM_WIDGET_CLASS );    
  IWidget widget = getWidgetForName(widgetParameter);
    MapPanel mapPanel = (MapPanel) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart().getAdapter( MapPanel.class );    
    if( mapPanel == null )
      return null;
    mapPanel.getWidgetManager().setActualWidget( widget );
    return null;
  }

  protected abstract IWidget getWidgetForName( final String widgetName );
}
