/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

package org.kalypso.ui.rrm.wizards.importGeologyData;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.convert.namodel.hydrotope.GeologyImportOperation;
import org.kalypso.convert.namodel.hydrotope.GeologyImportOperation.InputDescriptor;
import org.kalypso.convert.namodel.hydrotope.GeologyShapeInputDescriptor;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportGeologyWizard extends Wizard implements INewWizard
{
  private IStructuredSelection m_initialSelection;

  protected ImportGeologyWizardPage m_wizardPage;

  private final FeatureList m_featureList;

  public ImportGeologyWizard( final FeatureList featureList )
  {
    m_featureList = featureList;
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyDataImportGeologyWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_wizardPage = new ImportGeologyWizardPage();
    m_wizardPage.init( m_initialSelection );
    addPage( m_wizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_wizardPage.isPageComplete();
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String maxPerculationsRateProperty = m_wizardPage.getMaxPerculationsRateProperty();
    final String gwFactorProperty = m_wizardPage.getGWFactorProperty();
    final String sourceShapeFilePath = m_wizardPage.getSourceLocation().removeFileExtension().toPortableString();

    final File shapeFile = new File( sourceShapeFilePath );
    final InputDescriptor inputDescriptor = new GeologyShapeInputDescriptor( shapeFile, maxPerculationsRateProperty, gwFactorProperty );

    final Feature parentFeature = m_featureList.getParentFeature();
    final GeologyCollection output = (GeologyCollection) parentFeature;

    final GMLWorkspace workspace = output.getWorkspace();
    final IFile geologyFile = ResourceUtilities.findFileFromURL( workspace.getContext() );
    try
    {
      // call importer
      final GeologyImportOperation op = new GeologyImportOperation( inputDescriptor, output, ImportType.CLEAR_OUTPUT );
      final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, op );
      ErrorDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyDataImportGeologyWizard.1" ), execute.getMessage(), execute ); //$NON-NLS-1$

      final File outputFile = geologyFile.getLocation().toFile();
      GmlSerializer.serializeWorkspace( outputFile, workspace, "UTF-8" ); //$NON-NLS-1$
      geologyFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      MessageDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyDataImportGeologyWizard.1" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      return false;
    }
    return true;

  }

}
