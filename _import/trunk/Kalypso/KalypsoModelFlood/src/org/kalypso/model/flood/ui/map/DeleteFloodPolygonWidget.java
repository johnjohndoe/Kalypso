package org.kalypso.model.flood.ui.map;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.ogc.gml.map.MapPanel;
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
    super( "WSP-Anpassungen löschen", "löscht WSP-Anpassungen", new SelectFeatureWidget( "", "", new QName[] { IFloodPolygon.QNAME }, IFloodPolygon.QNAME_PROP_AREA ) );

    m_toolTipRenderer.setTooltip( "Selektieren Sie die WSP-Anpassungen in der Karte.\n    'Del': selektierte WSP-Anpassungen löschen." );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final MapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( "Selektieren Sie WSP-Anpassungen in der Karte.\n    'Del': selektierte WSP-Anpassungen löschen.\n" + delegateTooltip );

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

      Feature[] features = FeatureSelectionHelper.getFeatures( selectionManager );

      if( features.length == 0 )
        return;

      CommandableWorkspace workspace = eFeatures[0].getWorkspace();

      try
      {
        MapUtils.removeFeature( workspace, getMapPanel(), features );
      }
      catch( Exception e1 )
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
    }
    super.keyPressed( e );
  }

}
