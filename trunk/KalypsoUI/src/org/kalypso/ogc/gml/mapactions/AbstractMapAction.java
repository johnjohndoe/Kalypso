package org.kalypso.ogc.gml.mapactions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.gml.mapmodel.MapPanel;


/**
 * @author belger
 */
public class AbstractMapAction extends Action 
{
  private final MapPanel m_mapPanel;

  public AbstractMapAction( final MapPanel mapPanel, final String text, final ImageDescriptor imageDescriptor, final String tooltiptext, final int style )
  {
    super( text, style );
   
    setToolTipText( tooltiptext );
    setImageDescriptor( imageDescriptor );
    
    m_mapPanel = mapPanel;
  }
  
  protected final MapPanel getMapPanel()
  {
    return m_mapPanel;
  }
}