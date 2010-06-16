package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.grid.RichCoverageCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.gml.coverages.CoverageProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.wizards.WizardAddProfileFromDEM;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
final class CreateNewProfileJob extends UIJob implements ICreateProfileStrategy
{
  private final CreateProfileFromDEMWidget m_widget;

  private final CommandableWorkspace m_commandableWorkspace;

  private final IMapPanel m_mapPanel;

  private final WspmWaterBody m_waterBody;

  private final TuhhReach m_reach;

  private final ICoverageCollection m_coverages;

  private final double m_simplifyDistance;

  private LineGeometryBuilder m_geoBuilder;

  public CreateNewProfileJob( final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final WspmWaterBody waterBody, final TuhhReach reach, final ICoverageCollection coverages, final double simplifyDistance )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ) );
    m_widget = widget;

    m_commandableWorkspace = commandableWorkspace;
    m_mapPanel = mapPanel;
    m_waterBody = waterBody;
    m_reach = reach;
    m_coverages = coverages;
    m_simplifyDistance = simplifyDistance;

    m_geoBuilder = new LineGeometryBuilder( 0, mapPanel.getMapModell().getCoordinatesSystem() );
  }

  @Override
  public void dispose( )
  {
    if( m_geoBuilder != null )
    {
      m_geoBuilder.reset();
      m_geoBuilder = null;
    }
  }

  @Override
  public IStatus runInUIThread( final IProgressMonitor arg0 )
  {
    try
    {
      // remove last point: as we are using leftPressed, we always get two point on double clicks
      m_geoBuilder.removeLastPoint();
      final GM_Curve curve = (GM_Curve) m_geoBuilder.finish();

      final IProfil profile = createProfile( curve, TuhhProfil.PROFIL_TYPE );

      ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
      ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

      final IWizard wizard = new WizardAddProfileFromDEM( profile );
      final WizardDialog dialog = new WizardDialog( PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard );
      if( dialog.open() != Window.OK )
        return Status.OK_STATUS;

      final IProfileFeature profileFeature = ProfileUiUtils.addNewProfileAndFireEvents( profile, m_waterBody, m_reach );
      m_commandableWorkspace.postCommand( new EmptyCommand( "", false ) );

      final IFeatureSelectionManager selectionManager = m_mapPanel.getSelectionManager();
      selectionManager.setSelection( new EasyFeatureWrapper[] { new EasyFeatureWrapper( m_commandableWorkspace, profileFeature ) } );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), "Failed to create profile from terrain model." );
    }
  }

  private IProfil createProfile( final GM_Curve curve, final String profileType ) throws Exception
  {
    /* The builder for a profile from a DEM. */
    final RichCoverageCollection richCoverages = new RichCoverageCollection( m_coverages );
    final Coordinate[] gridCrds = richCoverages.extractPoints( curve );
    richCoverages.dispose();
    if( gridCrds == null )
      return CoverageProfile.convertLinestringToEmptyProfile( curve, profileType );

    // TODO: would be nice to simplify the coords, but not the profile afterwards. We need Douglas-Peucker for
    // coordinates (with distance by z!).

    final IProfil profile = CoverageProfile.createProfile( profileType, gridCrds, curve.getCoordinateSystem(), m_simplifyDistance );
    if( profile.getPoints().length == 0 )
      return CoverageProfile.convertLinestringToEmptyProfile( curve, profileType );

    return profile;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "New profile";
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#run()
   */
  @Override
  public void run( )
  {
    final CreateProfileFromDEMWidget widget = m_widget;
    final IMapPanel mapPanel = m_mapPanel;

    final JobChangeAdapter listener = new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event )
      {
        widget.activate( null, mapPanel );
        CreateNewProfileJob.this.removeJobChangeListener( this );
      }
    };

    addJobChangeListener( listener );

    schedule();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#paint(java.awt.Graphics,
   *      org.kalypso.ogc.gml.map.IMapPanel, java.awt.Point)
   */
  @Override
  public void paint( final Graphics g, final IMapPanel mapPanel, final Point currentPoint )
  {
    final GeoTransform projection = mapPanel.getProjection();
    m_geoBuilder.paint( g, projection, currentPoint );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#addPoint(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public void addPoint( final GM_Point pos ) throws Exception
  {
    m_geoBuilder.addPoint( pos );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#removeLastPoint()
   */
  @Override
  public void removeLastPoint( )
  {
    m_geoBuilder.removeLastPoint();
  }
}