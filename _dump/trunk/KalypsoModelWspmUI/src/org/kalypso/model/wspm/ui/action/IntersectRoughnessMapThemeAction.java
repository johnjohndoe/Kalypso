package org.kalypso.model.wspm.ui.action;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.wspm.core.gml.ProfileFeatureProvider;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class IntersectRoughnessMapThemeAction extends ActionDelegate
{
  private ISelection m_selection;

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    /* retrieve selected profiles, abort if none */
    final List<Feature> selectedProfiles = new ArrayList<Feature>();
    IKalypsoFeatureTheme theme = null;
    if( m_selection instanceof IStructuredSelection )
    {
      for( final Object selectedObject : ((IStructuredSelection) m_selection).toList() )
      {
        if( selectedObject instanceof IKalypsoFeatureTheme )
        {
          theme = (IKalypsoFeatureTheme) selectedObject;
          final FeatureList featureList = theme.getFeatureList();
          for( final Object object : featureList )
          {
            final Feature feature = FeatureHelper.getFeature( theme.getWorkspace(), object );
            addFeature( selectedProfiles, feature );
          }
        }
      }
    }

    final Shell shell = event.display.getActiveShell();

    if( selectedProfiles.size() == 0 || theme == null )
    {
      MessageDialog.openWarning( shell, "Rauheiten zuweisen", "Es wurden keine Profile in der Selektion gefunden." );
      return;
    }

    
    final IWizard intersectWizard = new IntersectRoughnessWizard( selectedProfiles.toArray( new Feature[selectedProfiles.size()] ), theme.getMapModell() );

    /* show intersection wizard */
    final WizardDialog2 dialog = new WizardDialog2( shell, intersectWizard );
    dialog.setRememberSize( true );
    dialog.open();
  }

  private void addFeature( final List<Feature> selectedProfiles, final Feature feature )
  {
    final WspmProfile profile = ProfileFeatureProvider.findProfile( feature );
    if( profile == null )
      return;

    final Feature profileFeature = profile.getFeature();
    selectedProfiles.add( profileFeature );
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection;
  }
}
