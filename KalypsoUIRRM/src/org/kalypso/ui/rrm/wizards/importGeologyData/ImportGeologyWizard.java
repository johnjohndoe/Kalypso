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

import org.apache.commons.lang.CharEncoding;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyShapeInputDescriptor;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportGeologyWizard extends Wizard implements IWorkbenchWizard
{
  private final static String PROPERTY_MAX_PERC_RATE = Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.12" ); //$NON-NLS-1$

  private final static String PROPERTY_GW_FACTOR = Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.14" ); //$NON-NLS-1$

  private ImportShapeWizardPage m_wizardPage;

  private FeatureList m_featureList;

  public ImportGeologyWizard( )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyDataImportGeologyWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IKalypsoTheme[] themes = MapHandlerUtils.getSelectedThemes( selection );
    if( themes.length != 1 )
      throw new IllegalArgumentException();

    m_featureList = ((IKalypsoFeatureTheme) themes[0]).getFeatureList();

    final String[] properties = new String[] { PROPERTY_MAX_PERC_RATE, PROPERTY_GW_FACTOR };
    m_wizardPage = new ImportShapeWizardPage( "shapePage", properties ); //$NON-NLS-1$

    m_wizardPage.setDescription( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.3" ) ); //$NON-NLS-1$

    addPage( m_wizardPage );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String maxPerculationsRateProperty = m_wizardPage.getProperty( PROPERTY_MAX_PERC_RATE );
    final String gwFactorProperty = m_wizardPage.getProperty( PROPERTY_GW_FACTOR );

    final File shapeFile = m_wizardPage.getShapeFile();
    final InputDescriptor inputDescriptor = new GeologyShapeInputDescriptor( shapeFile, maxPerculationsRateProperty, gwFactorProperty );

    final Feature parentFeature = m_featureList.getParentFeature();
    final GeologyCollection output = (GeologyCollection) parentFeature;

    final GMLWorkspace workspace = output.getWorkspace();
    final IFile geologyFile = ResourceUtilities.findFileFromURL( workspace.getContext() );
    final IFile parameterFile = geologyFile.getParent().getFile( new Path( "parameter.gml" ) ); //$NON-NLS-1$
    if( parameterFile.exists() )
    {
      try
      {
        // call importer
        final GeologyImportOperation op = new GeologyImportOperation( inputDescriptor, output, ImportType.CLEAR_OUTPUT );
        final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, op );
        new StatusDialog( getShell(), execute, getWindowTitle() ).open();
        if( execute.matches( IStatus.ERROR ) )
          return false;

        final File outputFile = geologyFile.getLocation().toFile();
        GmlSerializer.serializeWorkspace( outputFile, workspace, CharEncoding.UTF_8 );
        geologyFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
      }
      catch( final Exception e )
      {
        Display.getDefault().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            MessageDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyDataImportGeologyWizard.1" ), e.getLocalizedMessage() ); //$NON-NLS-1$
          }
        } );
        return false;
      }
    }
    else
    {
      {
        Display.getDefault().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            MessageDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizard.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          }
        } );
      }
    }
    return true;
  }

}
