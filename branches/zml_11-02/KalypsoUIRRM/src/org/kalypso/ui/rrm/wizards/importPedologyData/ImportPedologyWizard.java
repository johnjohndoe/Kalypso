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

package org.kalypso.ui.rrm.wizards.importPedologyData;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.operation.hydrotope.PedologyImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.PedologyImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.PedologyShapeInputDescriptor;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypso.ui.rrm.wizards.ImportShapeWizardPage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportPedologyWizard extends Wizard
{
  private final static String PROPERTY_SOIL_TYPE = Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizardPage.12" ); //$NON-NLS-1$

  protected ImportShapeWizardPage m_wizardPage;

  private final FeatureList m_featureList;

  public ImportPedologyWizard( final FeatureList featureList )
  {
    m_featureList = featureList;

    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizard.0" ) ); //$NON-NLS-1$

    final String[] properties = new String[] { PROPERTY_SOIL_TYPE };
    m_wizardPage = new ImportShapeWizardPage( "shapePage", properties ); //$NON-NLS-1$

    m_wizardPage.setDescription( Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizardPage.3" ) ); //$NON-NLS-1$

    addPage( m_wizardPage );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String soilTypeProperty = m_wizardPage.getProperty( PROPERTY_SOIL_TYPE );
    final File shapeFile = m_wizardPage.getShapeFile();

    final InputDescriptor inputDescriptor = new PedologyShapeInputDescriptor( shapeFile, soilTypeProperty );
    final Feature parentFeature = m_featureList.getParentFeature();
    final SoilTypeCollection lc = (SoilTypeCollection) parentFeature;

    final GMLWorkspace pedologyWorkspace = lc.getWorkspace();

    final IFile pedologyFile = ResourceUtilities.findFileFromURL( pedologyWorkspace.getContext() );

    // FIXME: should be checked beforehand
    final IFile parameterFile = pedologyFile.getParent().getFile( new Path( "parameter.gml" ) ); //$NON-NLS-1$
    if( !parameterFile.exists() )
    {
      MessageDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizard.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return true;
    }

    try
    {
      final SoilTypeCollection output = (SoilTypeCollection) pedologyWorkspace.getRootFeature();

      final Map<String, String> pedologyClasses = hashSoiltypeClasses( parameterFile );
      if( pedologyClasses.size() == 0 )
      {
        MessageDialog.openWarning( getShell(), getWindowTitle(), Messages.getString("ImportPedologyWizard.0") ); //$NON-NLS-1$
        return false;
      }

      // call importer
      final PedologyImportOperation op = new PedologyImportOperation( inputDescriptor, output, pedologyClasses, ImportType.CLEAR_OUTPUT );
      final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, op );
      new StatusDialog( getShell(), execute, getWindowTitle() ).open();
      if( execute.matches( IStatus.ERROR ) )
        return false;

      final File outputFile = pedologyFile.getLocation().toFile();
      GmlSerializer.serializeWorkspace( outputFile, pedologyWorkspace, "UTF-8" ); //$NON-NLS-1$
      pedologyFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      MessageDialog.openError( getShell(), getWindowTitle(), e.getLocalizedMessage() ); //$NON-NLS-1$
      return false;
    }

    return true;

  }

  private Map<String, String> hashSoiltypeClasses( final IFile parameterFile ) throws Exception, CoreException
  {
    final GMLWorkspace pedologyClassesWorkspace = GmlSerializer.createGMLWorkspace( parameterFile.getContents(), null, null );
    final Map<String, String> pedologyClasses = new HashMap<String, String>();
    final List< ? > pedologyClassesFeatures = (List< ? >) pedologyClassesWorkspace.getRootFeature().getProperty( new QName( NaModelConstants.NS_NAPARAMETER, "soiltypeMember" ) ); //$NON-NLS-1$
    for( final Object object : pedologyClassesFeatures )
    {
      final Feature f = (Feature) object;
      final String name = f.getName();
      final String id = f.getId();

      pedologyClasses.put( name, id );
    }

    pedologyClassesWorkspace.dispose();

    return pedologyClasses;
  }

}
