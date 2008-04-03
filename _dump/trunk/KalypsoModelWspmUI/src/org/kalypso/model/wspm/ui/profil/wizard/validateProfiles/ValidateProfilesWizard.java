/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IMarkerResolution2;
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
import org.kalypso.model.wspm.core.profil.MarkerIndex;
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
    setWindowTitle( "Profile validieren" );
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

    m_validatorChooserPage = new ArrayChooserPage( rules, new IValidatorRule[0], new IValidatorRule[0], 1, "validatorChooserPage", "Regeln auswählen", null ); //$NON-NLS-1$
    m_validatorChooserPage.setLabelProvider( new LabelProvider() );
    m_validatorChooserPage.setMessage( "Bitte wählen Sie aus, welche Regeln angewand werden sollen" );
    m_quickFixChoosePage = new ArrayChooserPage( m_reparatorRules, new IProfilMarkerResolution[0], new IProfilMarkerResolution[0], 1, "quickFixChoosePage", "QuickFix auswählen", null ); //$NON-NLS-1$
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
    m_quickFixChoosePage.setMessage( "Bitte wählen Sie aus, welcher QuickFix ausgelöst werden sollen" );

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
    final URL workspaceContext = m_workspace.getContext();
    final IFile resource = workspaceContext == null ? null : ResourceUtilities.findFileFromURL( workspaceContext );

    final ICoreRunnableWithProgress m_validateJob = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Profilvalidierung", profilFeatures.length );
        for( int i = 0; i < profilFeatures.length; i++ )
        {
          if( profilFeatures[i] instanceof Feature )
          {
            final Feature profilFeature = (Feature) profilFeatures[i];
            final WspmProfile wspmProfile = new WspmProfile( profilFeature );
            final IProfil profil = wspmProfile.getProfil();
            final IValidatorMarkerCollector collector = new ResourceValidatorMarkerCollector( resource, GmlEditor.ID, profil, profilFeature.getId() );
            try
            {
              collector.reset( profilFeature.getId() );
              for( final Object rule : choosenRules )
              {
                ((IValidatorRule) rule).validate( profil, collector );
              }
              final IMarker[] markers = collector.getMarkers();

              for( IMarker marker : markers )
              {
                final String resolutions = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS, (String) null );
                if( resolutions != null && quickFixes.length > 0 )
                {
                  final IMarkerResolution2 mr = KalypsoModelWspmCoreExtensions.getReparatorRule( resolutions );
                  for( final Object quickFix : quickFixes )
                  {
                    if( mr.getClass().getName().equals( quickFix.getClass().getName() ) )
                      mr.run( marker );
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
          monitor.worked( i );
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
          m_workspace.postCommand( new ChangeFeaturesCommand( m_workspace, new FeatureChange[] {} ) );
        }
        catch( Exception e )
        {
          // do nothing
        }
      }
    } );

    return true;
  }
}
