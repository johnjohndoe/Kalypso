/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.map.widgets;

import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Position;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.ogc.gml.featureview.FeatureviewHelper;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.util.command.ICommand;

/**
 * @author Belger
 */
public class EditFeatureWidget extends AbstractWidget
{
  // todo: replace with customized factory?
  private final FeatureviewHelper m_featureControlHelper = new FeatureviewHelper();

  private Point m_point;

  private Shell m_shell;

  public void setShell( final Shell shell )
  {
    m_shell = shell;
  }

  public void leftPressed( final Point p )
  {
    m_point = p;
    perform();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#performIntern()
   */
  public ICommand performIntern()
  {
    if( m_point != null )
    {
      final GM_Position position = getPosition( m_point );
      final IKalypsoLayer activeLayer = getActiveLayer();
      if( activeLayer instanceof KalypsoFeatureLayer )
      {
        final KalypsoFeatureLayer featureLayer = (KalypsoFeatureLayer)activeLayer;
        featureLayer.getSort().query( position, null );

        // TODO: schlimmer hack!
        final GeoTransform transform = getMapPanel().getProjection();
        final double gisRadius = transform.getSourceX( m_point.getX() + 20 ) - position.getX();

        final JMSelector selector = new JMSelector( JMSelector.MODE_COLLECT );

        final KalypsoFeature fe = selector.selectNearest( position, gisRadius, featureLayer, false,
            -1 );

        editFeature( featureLayer, fe );
      }
    }
    return null;
  }

  private void editFeature( final KalypsoFeatureLayer layer, final Feature feature )
  {
    // TODO: ok abbrechen
    if( m_shell != null && feature != null )
    {
      // TODO: uncomment this line
//      final FeatureviewHelper helper = m_featureControlFactory;
      // nur zu Debug Zwecken jedesmal eine neue Factory machen
      final FeatureviewHelper helper = new FeatureviewHelper();
      
      final Shell shell = m_shell;
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          final FeatureviewDialog dialog = new FeatureviewDialog( shell, layer, feature, 
              helper );
          dialog.open();
        }
      } );
    }
  }
}