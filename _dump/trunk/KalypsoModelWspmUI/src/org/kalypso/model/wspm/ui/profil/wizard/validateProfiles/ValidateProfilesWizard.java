/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.profil.wizard.validateProfiles;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.profil.validator.IValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.ValidatorRuleSet;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.validation.ResourceValidatorMarkerCollector;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypso.ui.editor.gmleditor.ui.GmlEditor;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kimwerner
 */
public class ValidateProfilesWizard extends Wizard
{

  private String m_profiletype = ""; //$NON-NLS-1$

  final private ValidatorRuleSet m_validatorRuleSet;

  final private IProfilMarkerResolution[] m_reparatorRules;

  final private ArrayChooserPage m_profileChooserPage;

  private ArrayChooserPage m_validatorChooserPage;

  private ArrayChooserPage m_quickFixChoosePage;

  final private List<Feature> m_profiles;

  final private List<Feature> m_selectedProfiles;

  final protected CommandableWorkspace m_workspace;

  public ValidateProfilesWizard( final CommandableWorkspace workspace, final List<Feature> profiles, final List<Feature> selection )
  {
    m_workspace = workspace;
    m_profiles = profiles;
    m_selectedProfiles = selection;
    m_profiletype = (String) profiles.get( 0 ).getProperty( ProfileFeatureFactory.QNAME_TYPE );
    m_validatorRuleSet = KalypsoModelWspmCorePlugin.getValidatorSet( m_profiletype );

    m_reparatorRules = KalypsoModelWspmCoreExtensions.createReparatorRules();
    setWindowTitle( Messages.ValidateProfilesWizard_0 );
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
    m_profileChooserPage = new ArrayChooserPage( m_profiles, new Object[0], m_selectedProfiles.toArray(), 1, "profilesChooserPage", Messages.PropertyEditWizard_3, null ); //$NON-NLS-1$
    m_profileChooserPage.setLabelProvider( new GMLLabelProvider() );
    m_profileChooserPage.setMessage( Messages.PropertyEditWizard_4 );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    addPage( m_profileChooserPage );

    final IValidatorRule[] rules = m_validatorRuleSet.getRules();

    m_validatorChooserPage = new ArrayChooserPage( rules, new IValidatorRule[0], new IValidatorRule[0], 1, "validatorChooserPage", Messages.ValidateProfilesWizard_1, null ); //$NON-NLS-1$
    m_validatorChooserPage.setLabelProvider( new LabelProvider() );
    m_validatorChooserPage.setMessage( Messages.ValidateProfilesWizard_2 );
    m_quickFixChoosePage = new ArrayChooserPage( m_reparatorRules, new IProfilMarkerResolution[0], new IProfilMarkerResolution[0], 0, "quickFixChoosePage", Messages.ValidateProfilesWizard_3, null ); //$NON-NLS-1$
    m_quickFixChoosePage.setLabelProvider( new LabelProvider()
    {

      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        return ((IProfilMarkerResolution) element).getLabel();
      }
    } );
    m_quickFixChoosePage.setMessage( Messages.ValidateProfilesWizard_4 );

    addPage( m_validatorChooserPage );
    addPage( m_quickFixChoosePage );

  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_validatorChooserPage.isPageComplete();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();
    final Object[] choosenRules = m_validatorChooserPage.getChoosen();
    final Object[] quickFixes = m_quickFixChoosePage.getChoosen();
    final int size = (profilFeatures.length);
    final IProfil[] profiles = new IProfil[size];
    final String[] featureIDs = new String[size];
    final ArrayList<FeatureChange> featureChanges = new ArrayList<FeatureChange>();

    final URL workspaceContext = m_workspace.getContext();
    final IFile resource = workspaceContext == null ? null : ResourceUtilities.findFileFromURL( workspaceContext );

    final ICoreRunnableWithProgress m_validateJob = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( Messages.ValidateProfilesWizard_5, size + size );
        for( int i = 0; i < size; i++ )
        {
          if( profilFeatures[i] instanceof Feature )
          {
            final WspmProfile wspmProfil = new WspmProfile( (Feature) profilFeatures[i] );
            profiles[i] = wspmProfil == null ? null : wspmProfil.getProfil();
            featureIDs[i] = ((Feature) profilFeatures[i]).getId();
          }

          if( profiles[i] != null )
          {
            try
            {
              final IMarker[] markers = resource.findMarkers( KalypsoModelWspmUIPlugin.MARKER_ID, true, IResource.DEPTH_ZERO );
              for( final IMarker marker : markers )
              {
                if( marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_PROFILE_ID ).equals( featureIDs[i] ) )
                  marker.delete();
              }
            }
            catch( CoreException e )
            {
              monitor.done();
              return new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getID(), e.getLocalizedMessage() );
            }
          }
          monitor.worked( i );
        }

        for( int i = 0; i < size; i++ )
        {
          if( profiles[i] != null )
          {
            final IValidatorMarkerCollector collector = new ResourceValidatorMarkerCollector( resource, GmlEditor.ID, "" + profiles[i].getStation(), featureIDs[i] ); //$NON-NLS-1$
            try
            {
              for( final Object rule : choosenRules )
              {
                ((IValidatorRule) rule).validate( profiles[i], collector );
              }
              final IMarker[] markers = collector.getMarkers();
              for( IMarker marker : markers )
              {
                final String quickFixRes = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS, (String) null );
                if( quickFixRes != null && quickFixes.length > 0 )
                {
                  final IProfilMarkerResolution mr = KalypsoModelWspmCoreExtensions.getReparatorRule( quickFixRes );
                  boolean resolved = false;
                  for( final Object quickFix : quickFixes )
                  {
                    if( mr.getClass().getName().equals( quickFix.getClass().getName() ) )
                    {
                      resolved = mr.resolve( profiles[i] );
                    }
                  }
                  if( resolved )
                  {
                    marker.delete();
                    for( final FeatureChange change : ProfileFeatureFactory.toFeatureAsChanges( profiles[i], (Feature) profilFeatures[i] ) )
                      featureChanges.add( change );
                  }
                }
              }
            }
            catch( final CoreException e )
            {
              monitor.done();
              return new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getID(), e.getLocalizedMessage() );
            }
          }
          monitor.worked( size + i );
        }
        monitor.done();
        return Status.OK_STATUS;
      }
    };
    Display.getDefault().asyncExec( new Runnable()
    {
      public void run( )
      {
        RunnableContextHelper.execute( new ProgressMonitorDialog( getShell() ), true, true, m_validateJob );

        try
        {
          m_workspace.postCommand( new ChangeFeaturesCommand( m_workspace, featureChanges.toArray( new FeatureChange[] {} ) ) );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    } );
    return true;
  }
}
