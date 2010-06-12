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
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.results.ProfileInterpolation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileHandlerUtils;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.util.swt.StatusDialog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public class InterpolateProfileHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final ProfileSelection profileSelection = ProfileHandlerUtils.getProfileSelectionChecked( event );

    final Feature container = profileSelection.getContainer();

    final WspmWaterBody waterBody = findWaterbody( container );
    final TuhhReach reach = findReach( container );

    final IProfileFeature[] profiles = profileSelection.getProfiles();
    final InterpolationWizard wizard = new InterpolationWizard( profiles );
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() != Window.OK )
      return null;

    try
    {
      doInterpolation( waterBody, reach, wizard );
    }
    catch( final CoreException e )
    {
      final String commandName = HandlerUtils.getCommandName( event );
      new StatusDialog( shell, e.getStatus(), commandName ).open();
    }

    return null;
  }

  private void doInterpolation( final WspmWaterBody waterBody, final TuhhReach reach, final InterpolationWizard wizard ) throws CoreException
  {
    try
    {
      final IProfileFeature previousProfile = wizard.getPreviousProfile();
      final IProfileFeature nextProfile = wizard.getNextProfile();
      final BigDecimal newStation = wizard.getNewStation();

      final ProfileInterpolation interpolation = new ProfileInterpolation( previousProfile, nextProfile );
      final IProfileFeature newProfile = waterBody.createNewProfile();
      interpolation.interpolate( newStation, newProfile );

      if( reach != null )
      {
        final TuhhReachProfileSegment segment = reach.createProfileSegment( newProfile, newStation.doubleValue() );
        final GMLWorkspace workspace = reach.getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, reach, new Feature[] { segment }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), "Profile interpolation failed.", e );
      throw new CoreException( status );
    }
  }

  private TuhhReach findReach( final Feature container )
  {
    if( container instanceof TuhhReach )
      return (TuhhReach) container;

    return null;
  }

  private WspmWaterBody findWaterbody( final Feature container ) throws ExecutionException
  {
    if( container instanceof WspmWaterBody )
      return (WspmWaterBody) container;

    if( container instanceof TuhhReach )
      return ((TuhhReach) container).getWaterBody();

    throw new ExecutionException( "Unable to insert profile into selection. Please select either a water body or a reach." );
  }
}
