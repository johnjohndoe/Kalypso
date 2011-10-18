package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
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
import org.kalypso.model.wspm.tuhh.ui.wizards.ProfileFromDEMData;
import org.kalypso.model.wspm.tuhh.ui.wizards.WizardAddProfileFromDEM;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
final class CreateNewProfileJob extends AbstractDemProfileJob
{
  public CreateNewProfileJob( final CreateProfileFromDEMWidget widget, final IMapPanel mapPanel, final ICoverageCollection coverages, final double simplifyDistance, final IKalypsoFeatureTheme[] profileThemes )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ), widget, mapPanel, coverages, simplifyDistance, profileThemes ); //$NON-NLS-1$
  }

  @Override
  protected IStatus runJob( final GM_Curve curve, final RichCoverageCollection richCoverages ) throws Exception
  {
    final IProfil profile = createProfile( richCoverages, curve, TuhhProfil.PROFIL_TYPE );
    if( profile == null )
      return openNoPointsWarning();

    ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    ProfileUiUtils.addDefaultMarkers( profile, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    final IKalypsoFeatureTheme[] profileThemes = getProfileThemes();

    final ProfileFromDEMData data = new ProfileFromDEMData( profile, profileThemes );

    final WizardAddProfileFromDEM wizard = new WizardAddProfileFromDEM( data );
    wizard.setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() ) );
    final WizardDialog2 dialog = new WizardDialog2( PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard );
    dialog.setRememberSize( true );
    if( dialog.open() != Window.OK )
      return Status.OK_STATUS;

    addNewProfile( data );

    return Status.OK_STATUS;
  }

  private void addNewProfile( final ProfileFromDEMData data ) throws Exception
  {
    final IKalypsoFeatureTheme theme = data.getTheme();
    final IProfil profile = data.getProfile();

    final CommandableWorkspace workspace = theme.getWorkspace();
    final FeatureList profileFeatures = theme.getFeatureList();
    final Feature parentFeature = profileFeatures.getParentFeature();
    final WspmWaterBody water = ProfileUiUtils.findWaterbody( parentFeature );
    final TuhhReach reach = ProfileUiUtils.findReach( parentFeature );

    final IProfileFeature profileFeature = ProfileUiUtils.addNewProfileAndFireEvents( profile, water, reach );
    workspace.postCommand( new EmptyCommand( StringUtils.EMPTY, false ) );

    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.setSelection( new EasyFeatureWrapper[] { new EasyFeatureWrapper( workspace, profileFeature ) } );
  }

  private IProfil createProfile( final RichCoverageCollection richCoverages, final GM_Curve curve, final String profileType ) throws Exception
  {
    final Coordinate[] gridCrds = richCoverages.extractPoints( curve );
    richCoverages.dispose();
    if( ArrayUtils.isEmpty( gridCrds ) )
      return null;

    // TODO: would be nice to simplify the coords, but not the profile afterwards. We need Douglas-Peucker for
    // coordinates (with distance by z!).

    final IProfil profile = CoverageProfile.createProfile( profileType, gridCrds, curve.getCoordinateSystem(), getSimplifyDistance() );
    if( profile.getPoints().length == 0 )
      return CoverageProfile.convertLinestringToEmptyProfile( curve, profileType );

    return profile;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "CreateNewProfileJob.2" ); //$NON-NLS-1$
  }

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