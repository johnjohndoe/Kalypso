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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.operation.hydrotope.DefaultLanduseClassDelegate;
import org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.LanduseShapeInputDescriptor;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypso.ui.rrm.wizards.ImportShapeWizardPage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportLanduseWizard extends Wizard
{
  private final static String PROPERTY_LANDUSE = Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.12" ); //$NON-NLS-1$

  private final static String PROPERTY_SEALING_FACTOR = Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.14" ); //$NON-NLS-1$

  private final static String PROPERTY_DRAINAGE_TYPE = Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.15" ); //$NON-NLS-1$

  protected ImportShapeWizardPage m_wizardPage;

  private final FeatureList m_featureList;

  public ImportLanduseWizard( final FeatureList featureList )
  {
    m_featureList = featureList;

    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduseImportLanduseWizard.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    final String[] properties = new String[] { PROPERTY_LANDUSE, PROPERTY_SEALING_FACTOR, PROPERTY_DRAINAGE_TYPE };
    m_wizardPage = new ImportShapeWizardPage( "shapePage", properties ); //$NON-NLS-1$

    m_wizardPage.setDescription( Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.3" ) ); //$NON-NLS-1$

    addPage( m_wizardPage );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String landuseProperty = m_wizardPage.getProperty( PROPERTY_LANDUSE );
    final String sealingFactorProperty = m_wizardPage.getProperty( PROPERTY_SEALING_FACTOR );
    final String drainageTypeProperty = m_wizardPage.getProperty( PROPERTY_DRAINAGE_TYPE );

    final File shapeFile = m_wizardPage.getShapeFile();
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
        final DefaultLanduseClassDelegate delegate = new DefaultLanduseClassDelegate( landuseClassesWorkspace );

        // call importer
        final LanduseImportOperation op = new LanduseImportOperation( inputDescriptor, output, delegate, ImportType.CLEAR_OUTPUT );
        final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, op );
        new StatusDialog( getShell(), execute, getWindowTitle() ).open();
        if( execute.matches( IStatus.ERROR ) )
          return false;

        final File outputFile = landuseFile.getLocation().toFile();
        GmlSerializer.serializeWorkspace( outputFile, landuseWorkspace, "UTF-8" ); //$NON-NLS-1$
        landuseFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );

        return true;
      }
      catch( final Exception e )
      {
        MessageDialog.openError( getShell(), getWindowTitle(), e.getLocalizedMessage() ); //$NON-NLS-1$
        return false;
      }
    }
    return true;

  }

}
