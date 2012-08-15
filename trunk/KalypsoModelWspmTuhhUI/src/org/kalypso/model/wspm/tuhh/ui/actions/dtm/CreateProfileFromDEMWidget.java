package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.gml.ui.coverage.CoverageManagementWidget;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ogc.gml.widgets.DeprecatedMouseWidget;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * Widget for drawing a line geometry and creating a profile from a DEM.
 * 
 * @author Holger Albert
 */
public class CreateProfileFromDEMWidget extends DeprecatedMouseWidget
{
  private static final String STR_DEFAULT_TOOLTIP = Messages.getString( "CreateProfileFromDEMWidget_0" ) + Messages.getString( "CreateProfileFromDEMWidget_1" ) + Messages.getString( "CreateProfileFromDEMWidget_2" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private static final String SETTINGS_MODE = "mode"; //$NON-NLS-1$

  private final ToolTipRenderer m_standardTooltip = ToolTipRenderer.createStandardTooltip();

  private final ToolTipRenderer m_errorTooltip = ToolTipRenderer.createErrorTooltip();

  private ToolTipRenderer m_tooltip = null;

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
    if( m_strategy != null )
    {
      m_strategy.dispose();
      m_strategy = null;
    }
    m_tooltip = null;
    m_currentPoint = null;

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
      m_errorTooltip.setTooltip( "Unable to find map." ); //$NON-NLS-1$
      m_tooltip = m_errorTooltip;
      return;
    }

    final ICoverageCollection coverages = initCoverages( model );
    if( coverages == null )
    {
      m_errorTooltip.setTooltip( Messages.getString( "CreateProfileFromDEMWidget_5" ) ); //$NON-NLS-1$
      m_tooltip = m_errorTooltip;
      return;
    }

    readSettings();
    m_strategy = initStrategy( model, coverages, mapPanel );
    if( m_strategy == null )
    {
      m_errorTooltip.setTooltip( Messages.getString( "CreateProfileFromDEMWidget_6" ) ); //$NON-NLS-1$
      m_tooltip = m_errorTooltip;
      return;
    }

    m_tooltip = m_standardTooltip;

    /* Init the cursor. */
    final Cursor cursor = Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );
    mapPanel.setCursor( cursor );
  }

  @Override
  public void finish( )
  {
    reset();

    repaintMap();

    super.finish();
  }

  private ICreateProfileStrategy initStrategy( final IMapModell model, final ICoverageCollection coverages, final IMapPanel mapPanel )
  {
    final IKalypsoFeatureTheme[] profileThemes = findProfileThemes( model );
    if( ArrayUtils.isEmpty( profileThemes ) )
      return null;

    // TODO: magic number.... get from user
    final double simplifyDistance = 0.01;

    if( m_strategyExtendProfile )
      return new ExtendProfileJob( this, mapPanel, coverages, simplifyDistance, profileThemes );
    else
      return new CreateNewProfileJob( this, mapPanel, coverages, simplifyDistance, profileThemes );
  }

  private IKalypsoFeatureTheme[] findProfileThemes( final IMapModell model )
  {
    final IKalypsoThemePredicate profilePredicate = new ProfileThemePredicate();
    final IKalypsoTheme activeTheme = model.getActiveTheme();
    if( profilePredicate.decide( activeTheme ) )
      return new IKalypsoFeatureTheme[] { (IKalypsoFeatureTheme) activeTheme };

    final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( profilePredicate );
    model.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] themes = visitor.getFoundThemes();
    return Arrays.castArray( themes, new IKalypsoFeatureTheme[themes.length] );
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
      // TODO: let user choose what theme to use
      return getCoverageTheme( (IKalypsoFeatureTheme) foundThemes[0] );
  }

  private ICoverageCollection getCoverageTheme( final IKalypsoFeatureTheme coverageTheme )
  {
    final FeatureList featureList = coverageTheme.getFeatureList();
    if( featureList == null )
      return null;

    final Feature coveragesFeature = featureList.getOwner();

    return (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_strategy == null )
    {
      activate( getCommandTarget(), getMapPanel() );
    }
    else
    {
      m_strategy.run();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    if( m_strategy == null )
      return;

    try
    {
      final GM_Point pos = MapUtilities.transform( getMapPanel(), p );
      m_strategy.addPoint( pos );
      repaintMap();
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

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_strategy != null )
    {
      m_strategy.paint( g, mapPanel, m_currentPoint );
    }

    if( m_tooltip != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();

      if( m_strategy != null )
      {
        m_standardTooltip.setTooltip( STR_DEFAULT_TOOLTIP + m_strategy.getLabel() );
      }

      m_tooltip.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
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
        if( m_strategy != null )
        {
          m_strategy.removeLastPoint();
          repaintMap();
        }
        break;

      case KeyEvent.VK_SPACE:
        m_strategyExtendProfile = !m_strategyExtendProfile;
        final IDialogSettings settings = getSettings();
        if( settings != null )
        {
          settings.put( SETTINGS_MODE, m_strategyExtendProfile );
        }
        activate( getCommandTarget(), getMapPanel() );
        repaintMap();
        break;
    }
  }

  private void readSettings( )
  {
    final IDialogSettings settings = getSettings();
    if( settings == null )
      return;

    m_strategyExtendProfile = settings.getBoolean( SETTINGS_MODE );
  }

  private IDialogSettings getSettings( )
  {
    return DialogSettingsUtils.getDialogSettings( KalypsoGisPlugin.getDefault(), getClass().getName() );
  }

}