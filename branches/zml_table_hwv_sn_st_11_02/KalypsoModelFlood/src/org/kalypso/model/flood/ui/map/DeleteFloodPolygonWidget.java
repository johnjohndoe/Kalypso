package org.kalypso.model.flood.ui.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.util.MapUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class DeleteFloodPolygonWidget extends AbstractDelegateWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  public DeleteFloodPolygonWidget( )
  {
    super( Messages.getString("org.kalypso.model.flood.ui.map.DeleteFloodPolygonWidget.0"), Messages.getString("org.kalypso.model.flood.ui.map.DeleteFloodPolygonWidget.1"), new SelectFeatureWidget( "", "", new QName[] { IFloodPolygon.QNAME }, IFloodPolygon.QNAME_PROP_AREA ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.model.flood.ui.map.DeleteFloodPolygonWidget.4") ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.model.flood.ui.map.DeleteFloodPolygonWidget.5") + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_DELETE )
    {
      e.consume();

      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
      final EasyFeatureWrapper[] eFeatures = selectionManager.getAllFeatures();

      final Feature[] features = FeatureSelectionHelper.getFeatures( selectionManager );

      if( features.length == 0 )
        return;

      final CommandableWorkspace workspace = eFeatures[0].getWorkspace();

      try
      {
        MapUtils.removeFeature( workspace, getMapPanel(), features );
      }
      catch( final Exception e1 )
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
    }
    super.keyPressed( e );
  }

}
