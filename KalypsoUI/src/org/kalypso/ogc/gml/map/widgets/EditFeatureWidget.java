package org.kalypso.ogc.gml.map.widgets;

import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Position;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.util.command.ICommand;

/**
 * @author Belger
 */
public class EditFeatureWidget extends AbstractWidget
{
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

        // todo: Sollte so etwas nicht eine Zentrale Stelle machen?
        final GeoTransform transform = getMapPanel().getProjection();
        final double gisRadius = transform.getSourceX( m_point.getX() + 20 ) - position.getX();

        final JMSelector selector = new JMSelector( JMSelector.MODE_COLLECT );

        final Feature fe = selector.selectNearest( position, gisRadius, featureLayer, false,
            -1 );

        editFeature( featureLayer, fe );
      }
    }
    return null;
  }

  private void editFeature( final KalypsoFeatureLayer layer, final Feature feature )
  {
    if( m_shell != null && feature != null )
    {
      final FeatureComposite helper = new FeatureComposite( feature );
      
      final Shell shell = m_shell;
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          final FeatureviewDialog dialog = new FeatureviewDialog( shell, layer, helper, getCommandTarget() );
          dialog.open();
        }
      } );
    }
  }
}