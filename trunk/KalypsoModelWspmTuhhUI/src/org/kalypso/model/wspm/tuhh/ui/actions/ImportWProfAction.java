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
package org.kalypso.model.wspm.tuhh.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class ImportWProfAction extends ActionDelegate implements IObjectActionDelegate
{
  private IWorkbenchPart m_targetPart;

  private TuhhWspmProject m_project;

  private CommandableWorkspace m_workspace;

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  @Override
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_targetPart = targetPart;
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    setSelection( selection );
    action.setEnabled( m_project != null );
  }

  private void setSelection( final ISelection selection )
  {
    m_project = null;
    m_workspace = null;

    if( !(selection instanceof IFeatureSelection) )
      return;

    final IFeatureSelection featureSelection = (IFeatureSelection) selection;
    final Object firstElement = featureSelection.getFirstElement();
    if( !(firstElement instanceof FeatureAssociationTypeElement) )
      return;

    final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) firstElement;
    final Feature parentFeature = fate.getParentFeature();
    if( parentFeature == null )
      return;

    m_workspace = featureSelection.getWorkspace( parentFeature );

    m_project = findProject( parentFeature );
  }

  private TuhhWspmProject findProject( final Feature parentFeature )
  {
    if( GMLSchemaUtilities.substitutes( parentFeature.getFeatureType(), TuhhWspmProject.QNAME ) )
      return new TuhhWspmProject( parentFeature );

    if( GMLSchemaUtilities.substitutes( parentFeature.getFeatureType(), WspmWaterBody.QNAME ) )
    {
      final Feature grandDad = parentFeature.getParent();
      if( GMLSchemaUtilities.substitutes( grandDad.getFeatureType(), TuhhWspmProject.QNAME ) )
        return new TuhhWspmProject( grandDad );
    }

    return null;
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  @Override
  public void run( final IAction action )
  {
    final Shell shell = m_targetPart.getSite().getShell();

    final WProfImportWizard importWizard = new WProfImportWizard( m_workspace, m_project );
    final IDialogSettings settings = PluginUtilities.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), "tuhhWProfImport" ); //$NON-NLS-1$
    importWizard.setDialogSettings( settings );

    final WizardDialog wizardDialog = new WizardDialog( shell, importWizard );
    wizardDialog.open();
  }
}
