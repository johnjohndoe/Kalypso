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

package org.kalypso.ui.rrm.wizards.importLanduse;

import java.io.File;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.convert.namodel.hydrotope.LanduseClassHelper;
import org.kalypso.convert.namodel.hydrotope.LanduseImportOperation;
import org.kalypso.convert.namodel.hydrotope.LanduseShapeInputDescriptor;
import org.kalypso.convert.namodel.hydrotope.LanduseImportOperation.InputDescriptor;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
import org.kalypso.convert.namodel.schema.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportLanduseWizard extends Wizard implements INewWizard
{
  private IStructuredSelection m_initialSelection;

  protected ImportLanduseWizardPage m_wizardPage;

  private final FeatureList m_featureList;

  public ImportLanduseWizard( final FeatureList featureList )
  {
    m_featureList = featureList;
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "ImportLanduseWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_wizardPage = new ImportLanduseWizardPage();
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
    final String landuseProperty = m_wizardPage.getLanduseProperty();
    final String sealingFactorProperty = m_wizardPage.getSealingFactorProperty();
    final String drainageTypeProperty = m_wizardPage.getDrainageTypeProperty();
    final String sourceShapeFilePath = m_wizardPage.getSourceLocation().removeFileExtension().toPortableString();

    final File shapeFile = new File( sourceShapeFilePath );
    final InputDescriptor inputDescriptor = new LanduseShapeInputDescriptor( shapeFile, landuseProperty, sealingFactorProperty, drainageTypeProperty );

    final Feature parentFeature = m_featureList.getParentFeature();
    final LanduseCollection lc = (LanduseCollection) parentFeature;

    final GMLWorkspace landuseWorkspace = lc.getWorkspace();

    final IFile landuseFile = ResourceUtilities.findFileFromURL( landuseWorkspace.getContext() );
    final IFile parameterFile = landuseFile.getParent().getFile( new Path( "parameter.gml" ) ); //$NON-NLS-1$
    if( parameterFile.exists() )
    {
      try
      {
        final GMLWorkspace landuseClassesWorkspace = GmlSerializer.createGMLWorkspace( parameterFile.getContents(), null, null );

        final LanduseCollection output = (LanduseCollection) landuseWorkspace.getRootFeature();
        final Map<String, String> landuseClasses = LanduseClassHelper.resolve( landuseClassesWorkspace );

        // call importer
        final LanduseImportOperation op = new LanduseImportOperation( inputDescriptor, output, landuseClasses, ImportType.CLEAR_OUTPUT );
        final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, op );
        ErrorDialog.openError( getShell(), Messages.getString( "ImportLanduseWizard.1" ), execute.getMessage(), execute ); //$NON-NLS-1$

        final File outputFile = landuseFile.getLocation().toFile();
        GmlSerializer.serializeWorkspace( outputFile, landuseWorkspace, "UTF-8" );
        landuseFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
      }
      catch( final Exception e )
      {
        MessageDialog.openError( getShell(), Messages.getString( "ImportLanduseWizard.1" ), e.getLocalizedMessage() );
        return false;
      }
    }
    return true;

  }

}
