package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import java.awt.Graphics;

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
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

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

  public CreateNewProfileJob( final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final WspmWaterBody waterBody, final TuhhReach reach, final ICoverageCollection coverages )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ) );
    m_widget = widget;

    m_commandableWorkspace = commandableWorkspace;
    m_mapPanel = mapPanel;
    m_waterBody = waterBody;
    m_reach = reach;
    m_coverages = coverages;
  }

  @Override
  public IStatus runInUIThread( final IProgressMonitor arg0 )
  {
    try
    {
      final GM_Curve curve = m_widget.createNewProfileCurve();

      /* The builder for a profile from a DEM. */
      final CoverageProfile cProfile = new CoverageProfile( m_coverages, TuhhProfil.PROFIL_TYPE );
      final IProfil profile = cProfile.createProfile( curve );
      cProfile.dispose();

      ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
      ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

      final IWizard wizard = new WizardAddProfileFromDEM( profile );
      final WizardDialog dialog = new WizardDialog( PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard );
      if( dialog.open() != Window.OK )
        return Status.OK_STATUS;

      final IProfileFeature profileFeature = ProfileUiUtils.addNewProfileAndFireEvents( profile, m_waterBody, m_reach );

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

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "New profile";
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#adjustPoint(org.kalypsodeegree.model.geometry.GM_Point,
   *      int)
   */
  @Override
  public GM_Point adjustPoint( final GM_Point pos, final int pointCount )
  {
    return pos;
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
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection, final GM_Point currentPos )
  {
    // nothing to paint
  }
}