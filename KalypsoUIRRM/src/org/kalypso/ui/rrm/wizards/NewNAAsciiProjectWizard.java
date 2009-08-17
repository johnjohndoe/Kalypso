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

/*
 * Created on 31.01.2005
 *
 */
package org.kalypso.ui.rrm.wizards;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.util.HashMap;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NAModellConverter;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * @author huebsch
 */
public class NewNAAsciiProjectWizard extends Wizard implements INewWizard, INewProjectWizard
{
  // Constants
  static final String PROJECT_PAGE = "page_type:createNewProject"; //$NON-NLS-1$

  private final String m_resourceBase = "resource/.projecttemplate.zip"; //$NON-NLS-1$

  final HashMap<String, Feature> m_IDMap = new HashMap<String, Feature>();

  private WizardNewProjectCreationPage m_createProjectPage;

  private IPath m_workspacePath;

  private IProject m_projectHandel;

  private IPath m_parameterPath;

  private GMLSchema m_parameterSchema;

  final File m_asciiBaseDir = new File( "C:\\TMP\\na" ); // TODO:change by wizard //$NON-NLS-1$

  public NewNAAsciiProjectWizard( )
  {
    try
    {
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      m_parameterSchema = schemaCatalog.getSchema( "http://www.tuhh.de/parameter", (String) null ); //$NON-NLS-1$
      setNeedsProgressMonitor( true );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
    }
  }

  @Override
  public void addPages( )
  {
    try
    {
      m_createProjectPage = new WizardNewProjectCreationPage( PROJECT_PAGE );
      m_createProjectPage.setDescription( Messages.get("org.kalypso.ui.rrm.wizards.NewNAAsciiProjectWizard.2") ); //$NON-NLS-1$
      m_createProjectPage.setTitle( Messages.get("org.kalypso.ui.rrm.wizards.NewNAAsciiProjectWizard.3") ); //$NON-NLS-1$
      // m_createProjectPage.setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
      addPage( m_createProjectPage );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * We will accept the selection in the workbench to see if we can initialize from it.
   *
   * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
  }

  /**
   * This method creates the new Project and all the necessary , performs the mapping and writes the new modell.gml file
   * .
   *
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {

    m_workspacePath = m_createProjectPage.getLocationPath();
    m_projectHandel = m_createProjectPage.getProjectHandle();

    try
    {
      m_projectHandel.create( new NullProgressMonitor() );
      m_projectHandel.open( new NullProgressMonitor() );
      final IProjectDescription description = m_projectHandel.getDescription();
      final String[] nanature = { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
      description.setNatureIds( nanature );
      m_projectHandel.setDescription( description, new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      ErrorDialog.openError( getShell(), "Create Project", "Failed to create project", e.getStatus() );
      return false;
    }

    // copy all the resources to the workspace into the new created project
    copyResourcesToProject( m_workspacePath.append( m_projectHandel.getFullPath() ) );
    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
      // open modell.gml and hydrotop.gml file to write imported feature
      m_parameterPath = new Path( m_projectHandel.getFullPath().append( "/parameter.gml" ).toString() ); //$NON-NLS-1$
      importParameter( m_parameterPath, m_asciiBaseDir );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, e1.getLocalizedMessage(), e1 );
      ErrorDialog.openError( getShell(), "Create Project", "Failed to import ASCII-Data", status );
      return false;
    }

    try
    {
      ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( final CoreException e2 )
    {
      e2.printStackTrace();
      return false;
    }
    return true;
  }

  private void copyResourcesToProject( final IPath path )
  {
    final String resource = m_resourceBase;
    System.out.print( Messages.get("org.kalypso.ui.rrm.wizards.NewNAAsciiProjectWizard.4") + resource + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final InputStream resourceAsStream = getClass().getResourceAsStream( resource );
    try
    {
      ZipUtilities.unzip( resourceAsStream, path.toFile() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( resourceAsStream );

    }
  }

  public IGMLSchema getParameterSchema( )
  {
    return m_parameterSchema;
  }

  public boolean performCancle( )
  {
    try
    {
      m_projectHandel.delete( true, false, null );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  public void importParameter( final IPath path, final File asciiBaseDir ) throws Exception
  {
    final IPath paraPath = m_workspacePath.append( path );
    final File parameterGmlFile = paraPath.toFile();
    final NAConfiguration ascii2GmlConfiguration = NAConfiguration.getAscii2GmlConfiguration( asciiBaseDir, parameterGmlFile );
    final Feature parameterRootFeature = NAModellConverter.parameterAsciiToFeature( ascii2GmlConfiguration );
    final GMLWorkspace paraWorkspace = new GMLWorkspace_Impl( m_parameterSchema, m_parameterSchema.getAllFeatureTypes(), parameterRootFeature, null, null, "http://www.tuhh.de/parameter", null ); //$NON-NLS-1$
    GmlSerializer.serializeWorkspace( new FileWriter( parameterGmlFile ), paraWorkspace );
    System.out.println( Messages.get("org.kalypso.ui.rrm.wizards.NewNAAsciiProjectWizard.7") + parameterGmlFile.getPath() ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.INewProjectWizard#getNewProject()
   */
  @Override
  public IProject getNewProject( )
  {
    return m_projectHandel;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.INewProjectWizard#setActivateScenarioOnPerformFinish(boolean)
   */
  @Override
  public void setActivateScenarioOnPerformFinish( final boolean b )
  {
    // nothing to do
  }

}