package org.kalypso.ogc.gml.map.widgets;

import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.feature.visitors.FindNearestVisitor;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author Belger
 */
public class EditFeatureWidget extends AbstractWidget
{
  private Point m_point;

  private final Shell m_shell;

  public EditFeatureWidget( final Shell shell )
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
      final IKalypsoTheme activeTheme = getActiveTheme();
      if( activeTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)activeTheme;

        // TODO: Sollte so etwas nicht eine Zentrale Stelle machen?
        final GeoTransform transform = getMapPanel().getProjection();
        final double gisRadius = transform.getSourceX( m_point.getX() + 20 ) - position.getX();
        
        final FindNearestVisitor visitor = new FindNearestVisitor( position, gisRadius );
        final FeatureList featureList = featureTheme.getFeatureList();
        featureList.accept( visitor );
        final Feature result = visitor.getResult();
        if( result != null )
          editFeature( featureTheme, featureTheme.getWorkspace(), result );
      }
    }
    return null;
  }

  private void editFeature( final ICommandTarget commandTarget, final GMLWorkspace workspace, final Feature feature )
  {
    if( m_shell != null && feature != null )
    {
      final FeatureComposite helper = new FeatureComposite( feature );
      
      final Shell shell = m_shell;
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          final FeatureviewDialog dialog = new FeatureviewDialog( shell, workspace, helper, commandTarget );
          dialog.open();
        }
      } );
    }
  }
}