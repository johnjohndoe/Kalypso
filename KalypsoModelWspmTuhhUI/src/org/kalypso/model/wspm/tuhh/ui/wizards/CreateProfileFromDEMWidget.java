package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.gml.coverages.CoverageProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
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
  /**
   * The builder for creating a line.
   */
  private LineGeometryBuilder m_geoBuilder;

  /**
   * The current point on the map screen.
   */
  private Point m_currentPoint;

  private GisMapEditor m_gisMapEditor;

  private ICoverageCollection m_coverages;

  private WspmWaterBody m_water;

  public CreateProfileFromDEMWidget( )
  {
    super( "", "" );

    try
    {
      m_gisMapEditor = (GisMapEditor) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
    }
    catch( final Exception e )
    {
      System.out.println( "Failed to acquire GisMapEditor." );
      e.printStackTrace();
    }

    try
    {
      final IMapModell model = m_gisMapEditor.getMapPanel().getMapModell();

      final FeatureList themeFeatureList = ((IKalypsoFeatureTheme) model.getActiveTheme()).getFeatureList();
      final Feature reachFeature = (Feature) themeFeatureList.first();

      m_water = (WspmWaterBody) reachFeature.getOwner();

      /* locate grid data - coverages feature */
      for( final IKalypsoTheme theme : model.getAllThemes() )
      {
        final String name = theme.getName().getValue();

        if( "GridCoverage".equals( name ) == false || (theme instanceof IKalypsoFeatureTheme) == false )
          continue;

        final FeatureList featureList = ((IKalypsoFeatureTheme) theme).getFeatureList();
        final Feature coveragesFeature = featureList == null ? null : featureList.getParentFeature();

        m_coverages = (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
        break;
      }

    }
    catch( final Exception e )
    {
      System.out.println( "Failed to gather required data." );
      e.printStackTrace();
    }

    /* No geometry builder at this point. */
    m_geoBuilder = null;

    /* No current point so far. */
    m_currentPoint = null;

  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    try
    {
      /* Init the widget. */
      init();

      /* Trasform to a GM_Point. */
      final GM_Point pos = MapUtilities.transform( getMapPanel(), p );

      /* Add the point to the builder. */
      m_geoBuilder.addPoint( pos );

      /* Get the object. */
      final GM_Object finish = m_geoBuilder.finish();

      /* Really finished? */
      if( finish == null )
        return;

      /* It should be a GM_Curve. */
      final GM_Curve curve = (GM_Curve) finish;

      /* The builder for a profile from a DEM. */
      final CoverageProfile cProfile = new CoverageProfile( m_coverages.get( 0 ), TuhhProfil.PROFIL_TYPE );

      /* building new profile. */
      final IProfil profile = cProfile.createProfile( curve );

      /* update profile: add durchstroemte bereiche, trennflaechen, rauheiten */
      final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
      final IRecord[] points = profile.getPoints();

      if( points.length > 1 )
      {
        final Object defaultValue = provider.getDefaultValue( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
        profile.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, points[0] ).setValue( defaultValue );
        profile.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, points[points.length - 1] ).setValue( defaultValue );
      }

      final WspmWaterBody waterBody = m_water;

      new UIJob( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ) )
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor arg0 )
        {
          final IWorkbenchWizard wizard = new WizardAddProfileFromDEM( profile );
          wizard.init( PlatformUI.getWorkbench(), null );
          final WizardDialog dialog = new WizardDialog( PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard );
          if( dialog.open() == Status.OK )
          {
            // final step, profile to the feature
            IProfileFeature profileFeature;
            try
            {
              profileFeature = waterBody.createNewProfile();
              ProfileFeatureFactory.toFeature( profile, profileFeature );

              final GMLWorkspace workspace = profileFeature.getWorkspace();

              final Feature[] changed_features = new Feature[1];
              changed_features[0] = profileFeature;
              final ModellEvent event = new FeatureStructureChangeModellEvent( workspace, waterBody, changed_features, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
              workspace.fireModellEvent( event );
            }
            catch( final GMLSchemaException e )
            {
              e.printStackTrace();
            }
          }

          return Status.OK_STATUS;
        }

      }.schedule();

      reset();
    }
    catch( final Exception e )
    {
      /* Error. */
      e.printStackTrace();

      /* Reset the widget. */
      reset();
    }
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Reset the widget. */
    reset();
    repaintMap();

    super.finish();
  }

  /**
   * This function initializes the widget.
   */
  private void init( )
  {
    /* Init only the first time. */
    if( m_geoBuilder == null )
    {
      /* Create a line geometry builder with no rule, regarding the amount of points. */
      m_geoBuilder = new LineGeometryBuilder( 0, getMapPanel().getMapModell().getCoordinatesSystem() );

      /* Init the cursor. */
      final Cursor cursor = Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );
      getMapPanel().setCursor( cursor );
    }
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      /* Init the widget. */
      init();

      /* Trasform to a GM_Point. */
      final GM_Point pos = MapUtilities.transform( getMapPanel(), p );

      /* Add the point to the builder. */
      m_geoBuilder.addPoint( pos );
    }
    catch( final Exception e )
    {
      /* Error. */
      e.printStackTrace();

      /* Reset the widget. */
      reset();
    }
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    /* Init the widget. */
    init();

    /* Store the current point. */
    m_currentPoint = p;

    /* Repaint. */
    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_geoBuilder != null )
      m_geoBuilder.paint( g, getMapPanel().getProjection(), m_currentPoint );
  }

  /**
   * This function resets the widget.
   */
  private void reset( )
  {
    if( m_geoBuilder != null )
    {
      m_geoBuilder.reset();
      m_geoBuilder = null;
    }
    /* Reset the cursor to default. */
    final Cursor cursor = Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );
    getMapPanel().setCursor( cursor );
  }

  @Override
  public void rightClicked( final Point p )
  {
    m_geoBuilder.removeLastPoint();
  }

  @Override
  public void keyReleased( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    if( KeyEvent.VK_ESCAPE == keyCode )
    {
      super.finish();

      reset();
    }

    super.keyPressed( e );
  }
}