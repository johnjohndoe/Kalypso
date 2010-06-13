package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gml.ui.map.CoverageManagementWidget;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * Widget for drawing a line geometry and creating a profile from a DEM.
 * 
 * @author Holger Albert
 */
public class CreateProfileFromDEMWidget extends AbstractWidget
{
  private static final String STR_DEFAULT_TOOLTIP = "CLICK-LEFT: add a new point\n" + "DOUBLE-CLICK: finish\n" + "BACKSPACE: remove last point\nSPACE: switch mode\nCurrent mode: ";

  private final ToolTipRenderer m_standardTooltip = ToolTipRenderer.createStandardTooltip();

  private final ToolTipRenderer m_errorTooltip = ToolTipRenderer.createErrorTooltip();

  private ToolTipRenderer m_tooltip = null;

  /**
   * The builder for creating a line.
   */
  private LineGeometryBuilder m_geoBuilder;

  /**
   * The current point on the map screen.
   */
  private Point m_currentPoint;

  private ICreateProfileStrategy m_strategy = null;

  private boolean m_strategyExtendProfile = true;

  public CreateProfileFromDEMWidget( )
  {
    super( "", "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * This function resets the widget.
   */
  private void reset( )
  {
    m_strategy = null;
    m_tooltip = null;
    m_currentPoint = null;

    if( m_geoBuilder != null )
    {
      m_geoBuilder.reset();
      m_geoBuilder = null;
    }

    /* Reset the cursor to default. */
    final Cursor cursor = Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );
    getMapPanel().setCursor( cursor );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.IMapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reset();

    final IMapModell model = mapPanel == null ? null : mapPanel.getMapModell();
    if( model == null )
    {
      m_errorTooltip.setTooltip( "Unable to find map." );
      m_tooltip = m_errorTooltip;
      return;
    }

    final ICoverageCollection coverages = initCoverages( model );
    if( coverages == null )
    {
      m_errorTooltip.setTooltip( "No coverages available. Please add a coverage theme to the map." );
      m_tooltip = m_errorTooltip;
      return;
    }

    m_strategy = initStrategy( model, coverages, mapPanel );
    if( m_strategy == null )
    {
      m_errorTooltip.setTooltip( "Unable to find a theme containing profiles." );
      m_tooltip = m_errorTooltip;
      return;
    }

    m_tooltip = m_standardTooltip;
    m_geoBuilder = new LineGeometryBuilder( 0, model.getCoordinatesSystem() );

    /* Init the cursor. */
    final Cursor cursor = Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );
    mapPanel.setCursor( cursor );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    reset();

    repaintMap();

    super.finish();
  }

  private ICreateProfileStrategy initStrategy( final IMapModell model, final ICoverageCollection coverages, final IMapPanel mapPanel )
  {
    final IKalypsoTheme activeTheme = model.getActiveTheme();
    if( !(activeTheme instanceof IKalypsoFeatureTheme) )
      return null;

    final CommandableWorkspace commandableWorkspace = ((IKalypsoFeatureTheme) activeTheme).getWorkspace();

    final FeatureList themeFeatureList = ((IKalypsoFeatureTheme) activeTheme).getFeatureList();
    if( themeFeatureList == null )
      return null;

    final Feature parentFeature = themeFeatureList.getParentFeature();

    final WspmWaterBody water = ProfileUiUtils.findWaterbody( parentFeature );
    final TuhhReach reach = ProfileUiUtils.findReach( parentFeature );

    if( m_strategyExtendProfile )
      return new ExtendProfileJob( this, commandableWorkspace, mapPanel, water, reach, coverages );
    else
      return new CreateNewProfileJob( this, commandableWorkspace, mapPanel, water, reach, coverages );
  }

  private ICoverageCollection initCoverages( final IMapModell model )
  {
    final KalypsoThemeVisitor coverageSearcher = new KalypsoThemeVisitor( CoverageManagementWidget.COVERAGE_PREDICATE );
    model.accept( coverageSearcher, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] foundThemes = coverageSearcher.getFoundThemes();
    if( foundThemes.length == 0 )
      return null;
    else if( foundThemes.length == 1 )
      return getCoverageTheme( (IKalypsoFeatureTheme) foundThemes[0] );
    else
    {
      // TODO: let user choose what theme to use
      return getCoverageTheme( (IKalypsoFeatureTheme) foundThemes[0] );
    }
  }

  private ICoverageCollection getCoverageTheme( final IKalypsoFeatureTheme coverageTheme )
  {
    final FeatureList featureList = coverageTheme.getFeatureList();
    final Feature coveragesFeature = featureList == null ? null : featureList.getParentFeature();
    return (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    leftPressed( p );

    if( m_strategy != null )
      m_strategy.run();
  }

  protected GM_Curve createNewProfileCurve( ) throws Exception
  {
    /* Get the object. */
    final GM_Object finish = m_geoBuilder.finish();

    /* Really finished? */
    if( finish == null )
      return null;

    return (GM_Curve) finish;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    if( m_geoBuilder == null || m_strategy == null )
      return;

    try
    {
      final GM_Point pos = MapUtilities.transform( getMapPanel(), p );
      final GM_Point adjustedPos = (m_strategy).adjustPoint( pos, m_geoBuilder.getPointCount() );
      if( adjustedPos != null )
      {
        m_geoBuilder.addPoint( pos );
        repaintMap();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      /* Reset the widget. */
      activate( getCommandTarget(), getMapPanel() );
    }
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;

    repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_tooltip != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();

      m_standardTooltip.setTooltip( STR_DEFAULT_TOOLTIP + m_strategy.getLabel() );

      m_tooltip.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }

    if( m_geoBuilder != null )
      m_geoBuilder.paint( g, mapPanel.getProjection(), m_currentPoint );
  }

  @Override
  public void keyReleased( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    switch( keyCode )
    {
      case KeyEvent.VK_ESCAPE:
        activate( getCommandTarget(), getMapPanel() );
        repaintMap();
        return;

      case KeyEvent.VK_BACK_SPACE:
        if( m_geoBuilder != null )
        {
          m_geoBuilder.removeLastPoint();
          repaintMap();
        }
        break;

      case KeyEvent.VK_SPACE:
        m_strategyExtendProfile = !m_strategyExtendProfile;
        activate( getCommandTarget(), getMapPanel() );
        repaintMap();
        break;
    }
  }
}