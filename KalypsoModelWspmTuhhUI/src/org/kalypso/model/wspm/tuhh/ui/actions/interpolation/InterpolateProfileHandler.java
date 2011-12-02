package org.kalypso.model.wspm.tuhh.ui.actions.interpolation;

import java.math.BigDecimal;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.util.ProfileInterpolation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileHandlerUtils;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

public class InterpolateProfileHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final ProfileSelection profileSelection = ProfileHandlerUtils.getProfileSelectionChecked( event );

    final Feature container = profileSelection.getContainer();

    final WspmWaterBody waterBody = ProfileUiUtils.findWaterbody( container );
    if( waterBody == null )
      throw new ExecutionException( Messages.getString("InterpolateProfileHandler_0") ); //$NON-NLS-1$

    final TuhhReach reach = ProfileUiUtils.findReach( container );

    final IProfileFeature[] profiles = profileSelection.getProfiles();
    final InterpolationWizard wizard = new InterpolationWizard( profiles );
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() != Window.OK )
      return null;

    try
    {
      doInterpolation( waterBody, reach, wizard );
      final CommandableWorkspace workspace = profileSelection.getWorkspace();
      workspace.postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      final String commandName = HandlerUtils.getCommandName( event );
      new StatusDialog( shell, e.getStatus(), commandName ).open();
    }
    catch( final Exception e )
    {
      // will never happen
      e.printStackTrace();
    }

    return null;
  }

  private void doInterpolation( final WspmWaterBody waterBody, final TuhhReach reach, final InterpolationWizard wizard ) throws CoreException
  {
    try
    {
      final IProfil previousProfile = wizard.getPreviousProfile().getProfil();
      final IProfil nextProfile = wizard.getNextProfile().getProfil();
      final BigDecimal newStation = wizard.getNewStation();
      final boolean onlyRiverChannel = wizard.getOnlyRiverChannel();

      final ProfileInterpolation interpolation = new ProfileInterpolation( previousProfile, nextProfile, onlyRiverChannel );
      final IProfil newProfile = interpolation.interpolate( newStation, previousProfile.getType() );

      ProfileUiUtils.addNewProfileAndFireEvents( newProfile, waterBody, reach );

    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString("InterpolateProfileHandler_2"), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }
}
