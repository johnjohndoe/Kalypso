package org.kalypso.ogc.gml.mapactions;

import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;


/**
 * @author belger
 */
public class AbstractSelectWidgetAction extends AbstractMapAction 
{
  private final IWidget m_widget;
  private final MapPanel m_mapPanel;

  public AbstractSelectWidgetAction( final MapPanel mapPanel, final IWidget widget, final ImageDescriptor imageDescriptor, final String tooltiptext )
  {
    super( mapPanel, null, imageDescriptor, tooltiptext, AS_RADIO_BUTTON );
   
    setToolTipText( tooltiptext );
    setImageDescriptor( imageDescriptor );
    
    m_mapPanel = mapPanel;
    m_widget = widget;
  }
  
  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    m_mapPanel.changeWidget( m_widget );
  }
}
