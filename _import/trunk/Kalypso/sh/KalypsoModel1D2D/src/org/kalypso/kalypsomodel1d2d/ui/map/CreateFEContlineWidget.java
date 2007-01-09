package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Graphics;
import java.awt.Point;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.ContinuityLineOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class CreateFEContlineWidget extends AbstractWidget
{
  private Point m_currentPoint = null;

  private LineGeometryBuilder m_builder = null;

  public CreateFEContlineWidget( )
  {
    super( "New Continuity Line", "Creates a new Continuity Line" );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // find the right themes to edit i.e. the discretisation model

    reinit();
  }

  private final void reinit( )
  {
    m_builder = null;

    final CS_CoordinateSystem targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    m_builder = new LineGeometryBuilder( 0, targetCrs );
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      if( m_builder != null )
      {
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );

        m_builder.addPoint( currentPos );
      }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    try
    {
      final GM_Curve curve = (GM_Curve) m_builder.finish();

      // validate geometry: doppelte punkte

      final FE1D2DDiscretisationModel model = findDiscretisationModel();

      /* final IFE1D2DContinuityLine<IFE1D2DComplexElement,IFE1D2DEdge> continuityLine = */ContinuityLineOps.contilineFromCurve( curve, model );
      // TODO: add contiline to modell

      // TODO: validate against existing contilines?

      reinit();
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * we must have an active node, edge or element theme, one of them must be active
   */
  private FE1D2DDiscretisationModel findDiscretisationModel( )
  {
    final IKalypsoTheme theme = getMapPanel().getMapModell().getActiveTheme();
    if( theme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
      final IFeatureType featureType = featureTheme.getFeatureType();
      if( GMLSchemaUtilities.substitutes( featureType, FE1D2DNode.QNAME_FE1D2DNode ) || GMLSchemaUtilities.substitutes( featureType, FE1D2DEdge.QNAME_FE1D2DEdge )
          || GMLSchemaUtilities.substitutes( featureType, FE1D2D_2DElement.QNAME_FE1D2D_2DElement ) )
      {
        final Feature parentFeature = featureTheme.getFeatureList().getParentFeature();

        return new FE1D2DDiscretisationModel( parentFeature );
      }
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      if( m_builder != null )
        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
  }

}
