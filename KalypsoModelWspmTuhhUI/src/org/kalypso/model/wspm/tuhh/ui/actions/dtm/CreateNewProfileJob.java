package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
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
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
final class CreateNewProfileJob extends AbstractDemProfileJob
{
  private final WspmWaterBody m_waterBody;

  public CreateNewProfileJob( final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final WspmWaterBody waterBody, final TuhhReach reach, final ICoverageCollection coverages, final double simplifyDistance )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ), widget, commandableWorkspace, mapPanel, reach, coverages, simplifyDistance ); //$NON-NLS-1$

    m_waterBody = waterBody;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.AbstractDemProfileJob#runJob(org.kalypsodeegree.model.geometry.GM_Curve,
   *      org.kalypso.grid.RichCoverageCollection)
   */
  @Override
  protected IStatus runJob( final GM_Curve curve, final RichCoverageCollection richCoverages ) throws Exception
  {
    final IProfil profile = createProfile( richCoverages, curve, TuhhProfil.PROFIL_TYPE );

    ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    final WizardAddProfileFromDEM wizard = new WizardAddProfileFromDEM( profile );
    wizard.setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() ) );
    final WizardDialog2 dialog = new WizardDialog2( PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard );
    dialog.setRememberSize( true );
    if( dialog.open() != Window.OK )
      return Status.OK_STATUS;

    final IProfileFeature profileFeature = ProfileUiUtils.addNewProfileAndFireEvents( profile, m_waterBody, getReach() );
    final CommandableWorkspace workspace = getWorkspace();
    workspace.postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$

    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.setSelection( new EasyFeatureWrapper[] { new EasyFeatureWrapper( workspace, profileFeature ) } );

    return Status.OK_STATUS;
  }

  private IProfil createProfile( final RichCoverageCollection richCoverages, final GM_Curve curve, final String profileType ) throws Exception
  {
    final Coordinate[] gridCrds = richCoverages.extractPoints( curve );
    richCoverages.dispose();
    if( gridCrds == null )
      return CoverageProfile.convertLinestringToEmptyProfile( curve, profileType );

    // TODO: would be nice to simplify the coords, but not the profile afterwards. We need Douglas-Peucker for
    // coordinates (with distance by z!).

    final IProfil profile = CoverageProfile.createProfile( profileType, gridCrds, curve.getCoordinateSystem(), getSimplifyDistance() );
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
    return Messages.getString("CreateNewProfileJob.2"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#addPoint(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public void addPoint( final GM_Point pos ) throws Exception
  {
    getGeoBuilder().addPoint( pos );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#removeLastPoint()
   */
  @Override
  public void removeLastPoint( )
  {
    getGeoBuilder().removeLastPoint();
  }
}