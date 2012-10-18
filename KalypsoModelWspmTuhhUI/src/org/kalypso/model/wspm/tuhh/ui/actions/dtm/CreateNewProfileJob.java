package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.util.CreateProfileFromCrdsTransaction;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
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
import org.kalypsodeegree.model.coverage.RichCoverageCollection;
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
  private final double m_digitalizeDistance;

  public CreateNewProfileJob( final CreateProfileFromDEMWidget widget, final IMapPanel mapPanel, final ICoverageCollection coverages, final double simplifyDistance, final double digitalizeDistance, final IKalypsoFeatureTheme[] profileThemes )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.3" ), widget, mapPanel, coverages, simplifyDistance, profileThemes ); //$NON-NLS-1$

    m_digitalizeDistance = digitalizeDistance;
  }

  @Override
  protected IStatus runJob( final GM_Curve curve, final RichCoverageCollection richCoverages ) throws Exception
  {
    final IProfile profile = createProfile( richCoverages, curve, TuhhProfil.PROFIL_TYPE );
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
    final IProfile profile = data.getProfile();

    final CommandableWorkspace workspace = theme.getWorkspace();
    final FeatureList profileFeatures = theme.getFeatureList();
    final Feature parentFeature = profileFeatures.getOwner();
    final WspmWaterBody water = ProfileUiUtils.findWaterbody( parentFeature );
    final TuhhReach reach = ProfileUiUtils.findReach( parentFeature );

    final Feature featureForSelection = ProfileUiUtils.addNewProfileAndFireEvents( profile, water, reach, null );
    workspace.postCommand( new EmptyCommand( StringUtils.EMPTY, false ) );

    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.setSelection( new EasyFeatureWrapper[] { new EasyFeatureWrapper( workspace, featureForSelection ) } );
  }

  private IProfile createProfile( final RichCoverageCollection richCoverages, final GM_Curve curve, final String profileType ) throws Exception
  {
    final Coordinate[] gridCrds = richCoverages.extractPoints( curve, m_digitalizeDistance );
    richCoverages.dispose();
    if( ArrayUtils.isEmpty( gridCrds ) )
      return null;

    // TODO: Would be nice to simplify the coords, but not the profile afterwards.
    // TODO: We need Douglas-Peucker for coordinates (with distance by z!).

    final String coordinateSystem = curve.getCoordinateSystem();
    final IProfile profile = CreateProfileFromCrdsTransaction.createProfileFromCoordinates( profileType, gridCrds, coordinateSystem, getSimplifyDistance() );
    if( profile.getPoints().length == 0 )
      return WspmProfileHelper.convertLinestringToEmptyProfile( curve, profileType );

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

  @Override
  public void removeLastPoint( )
  {
    getGeoBuilder().removeLastPoint();
  }
}