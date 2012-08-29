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
package org.kalypso.kalypsomodel1d2d.ui.wizard.windmodel;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsomodel1d2d.conv.wind.IWindDataCollectionReader;
import org.kalypso.kalypsomodel1d2d.conv.wind.IWindDataReader;
import org.kalypso.kalypsomodel1d2d.conv.wind.WindDataConverterFactory;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.kalypsosimulationmodel.core.wind.NativeWindDataModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.wind.WindDataModelSystem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 *
 *
 */
public class ImportWindDataWizard extends Wizard implements INewWizard/* INewWizardKalypsoImport */
{
  private IStructuredSelection initialSelection;

  private WindDataWizardMainPage m_wizardPage;

  IFolder m_modelFolder;

  IWindModel m_windModel;

  IFolder m_destFolder;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportWindDataWizard( )
  {
  }

  /**
   * The required selection structure is:
   * <ul>
   * <li/>Length=2
   * <li/>First element an instance of {@link IWindModel}
   * <li/>Second element an instance of {@link IFolder}
   * <li/>third element an instance of {@link CommandableWorkspace}
   * </ul>
   *
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    initialSelection = selection;
    final Iterator selIterator = selection.iterator();

    m_windModel = (IWindModel) selIterator.next();
    m_modelFolder = (IFolder) selIterator.next();
    m_destFolder = (IFolder) selIterator.next();
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.0" ) ); //$NON-NLS-1$
    m_wizardPage = new WindDataWizardMainPage();
    addPage( m_wizardPage );
    m_wizardPage.init( initialSelection );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final IPath sourcePath = m_wizardPage.getSourceLocation();
      final String strFileName = m_wizardPage.getModelName().trim();
      final String strFileDescription = m_wizardPage.getDescriptionForFileArea();
      final String selectedCoordinateSystem = m_wizardPage.getCoordinateSystem();

      getContainer().run( true, true, new IRunnableWithProgress()
      {
        @Override
        public void run( final IProgressMonitor monitor ) throws InvocationTargetException, IllegalArgumentException
        {
          try
          {
            final GMLWorkspace workspace = m_windModel.getWorkspace();

            final File modelFolderFile = getUTF_DecodedFile( new File( FileLocator.toFileURL( workspace.getContext() ).getFile() ).getParentFile() );
            final File srcFile = getUTF_DecodedFile( sourcePath.toFile() );

            final IWindDataCollectionReader lWindDataConverter = (IWindDataCollectionReader) WindDataConverterFactory.getWindDataConverterForFileType( srcFile.toURI().toURL(), m_destFolder.getLocationURI().toURL(), strFileName.replace( ' ', '_' ), selectedCoordinateSystem );
            final boolean lBoolConvertResult = convertAndCopy( lWindDataConverter );
            if( !lBoolConvertResult )
            {
              throw new Exception( Messages.getString("ImportWindDataWizard.0") ); //$NON-NLS-1$
            }

            final IWindDataModelSystem lWindDataModelSystem = WindDataModelSystem.createWindSystemForWindModel( ImportWindDataWizard.this.m_windModel, lWindDataConverter.getGridDescriptor(), strFileName, strFileDescription );

            final List<IWindDataWrapper> lListImportedDataFiles = lWindDataConverter.getCollectionWindDataProviders();
            m_modelFolder.getProject().refreshLocal( IResource.DEPTH_INFINITE, null/* new NullProgressMonitor() */);
            for( int i = 0; i < lListImportedDataFiles.size(); ++i )
            {
              final IWindDataWrapper lActWindDataProvider = lListImportedDataFiles.get( i );
              final URL lUrlNewInternalWindDataFile = lActWindDataProvider.getDataFileURL();
              final String nativeTEMRelPath = modelFolderFile.toURI().relativize( lUrlNewInternalWindDataFile.toURI() ).toString(); //$NON-NLS-1$

              final IWindDataModel lWindDataModel = NativeWindDataModelWrapper.createFeatureForTEMSystem( lWindDataModelSystem, nativeTEMRelPath, lActWindDataProvider.getDateStep() );

              final String name = lUrlNewInternalWindDataFile.getFile();
              if( !"".equals( strFileName ) )//$NON-NLS-1$
              {
                lWindDataModel.setName( strFileName );
              }
              else
              {
                lWindDataModel.setName( name );
              }

              if( selectedCoordinateSystem.compareTo( "" ) != 0 ) //$NON-NLS-1$
              {
                lWindDataModel.setCoordinateSystem( selectedCoordinateSystem );
              }
              final Feature temFeature = lWindDataModel;
              workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, lWindDataModelSystem, temFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
              workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, temFeature.getOwner(), temFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            }
            // TODO check why saving thow pool does not work
            final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

            caseDataProvider.postCommand( IWindModel.class.getName(), new AddWindDataModelCmd() );

            caseDataProvider.saveModel( IWindModel.class.getName(), null );

          }

          catch( final Exception e )
          {
            e.printStackTrace();
            throw new InvocationTargetException( e );
          }
        }
      } );

    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return false;
    }

    return true;
  }

  public static String getExtension( final File f )
  {
    String ext = null;
    final String s = f.getName();
    final int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( i + 1 ).toLowerCase();
    }
    return ext;
  }

  public static String getFileNameNoExtension( final File f )
  {
    String ext = null;
    final String s = f.getName();
    final int i = s.lastIndexOf( '.' );

    if( i > 0 && i < s.length() - 1 )
    {
      ext = s.substring( 0, i );
    }
    return ext;
  }

  // private List<String> convertAndCopy( final File src, final URL url, final IWindDataReader pWindDataConverter, final
  // IWindDataWriter pWindDataWriter )
  boolean convertAndCopy( final IWindDataReader pWindDataConverter )
  {
    try
    {
      if( !pWindDataConverter.read() )
        return false;

      return true;

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return false;
    }
  }

  /**
   * Answer the selected source location
   */
  public IPath getSourceLocationsss( )
  {
    return m_wizardPage.getSourceLocation();
  }

  public String getFileDescription( )
  {
    return m_wizardPage.getDescriptionForFileArea();
  }

  public String getFileUserName( )
  {
    return m_wizardPage.getModelName();
  }

  public File getUTF_DecodedFile( final File file )
  {
    try
    {
      return new File( URLDecoder.decode( file.toString(), "UTF-8" ) ); //$NON-NLS-1$
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
    return file;
  }

}
