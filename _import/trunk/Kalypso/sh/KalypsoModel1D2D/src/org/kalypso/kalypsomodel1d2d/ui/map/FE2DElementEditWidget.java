package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DNode;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.MapfunctionHelper;
import org.kalypso.ogc.gml.map.widgets.providers.QNameFeaturesProvider;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class FE2DElementEditWidget extends AbstractWidget
{
  private Point m_currentPoint = null;

  private ElementGeometryBuilder m_builder = null;

  private FE1D2DNode m_snapNode = null;

  private IKalypsoFeatureTheme m_nodeTheme;

  private final int m_radius = 20;

  private final QNameFeaturesProvider m_provider = new QNameFeaturesProvider( FE1D2DNode.QNAME_FE1D2DNode );

  public FE2DElementEditWidget( )
  {
    super( "New FE-Element", "Creates a new FE-Element" );
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

    // we must have the node and the element theme, one of them must be active
    final IKalypsoTheme activeTheme = mapPanel.getMapModell().getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
      final IFeatureType featureType = theme.getFeatureType();
      if( GMLSchemaUtilities.substitutes( featureType, FE1D2DNode.QNAME_FE1D2DNode ) )
      {
        m_nodeTheme = theme;
        m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
      }
    }
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;

    /* Try to snap to next node */
    m_snapNode = null;

    /* find nearest node */
    // TODO: exclude already found nodes?
    final MapPanel mapPanel = getMapPanel();
    final EasyFeatureWrapper[] allNodeWrappers = m_provider.getFeatures( mapPanel );
    final EasyFeatureWrapper[] nearNodeWrappers = MapfunctionHelper.findFeatureToSelect( mapPanel, new Rectangle( p.x, p.y, 0, 0 ), allNodeWrappers, m_radius );
    if( nearNodeWrappers.length > 0 )
    {
      final FE1D2DNode nearestNode = new FE1D2DNode( nearNodeWrappers[0].getFeature() );
      m_snapNode = nearestNode;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      final ICommand command;
      if( m_snapNode != null )
        command = m_builder.addNode( m_snapNode );
      else
      {
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );
        command = m_builder.addNode( currentPos );
      }

      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );
        m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
      }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      m_snapNode = null;
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
      final ICommand command = m_builder.finish();
      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );
        m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
      }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      m_snapNode = null;
    }

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint;
    if( m_snapNode == null )
      currentPoint = m_currentPoint;
    else
    {
      final GM_Point snapPoint = m_snapNode.getPoint();
      currentPoint = MapUtilities.retransform( getMapPanel(), snapPoint );
    }

    if( currentPoint != null )
    {
      if( m_builder != null )
        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
  }

}
