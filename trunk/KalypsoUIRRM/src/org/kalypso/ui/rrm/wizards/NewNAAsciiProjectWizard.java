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

package org.kalypso.ui.rrm.wizards;

import java.io.File;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.wizards.NewProjectWizard;
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
public class NewNAAsciiProjectWizard extends NewProjectWizard
{
  static final String PROJECT_PAGE = "page_type:createNewProject"; //$NON-NLS-1$

  final HashMap<String, Feature> m_IDMap = new HashMap<String, Feature>();

  private GMLSchema m_parameterSchema;

  final File m_asciiBaseDir = new File( "C:\\TMP\\na" ); // TODO:change by wizard //$NON-NLS-1$

  public NewNAAsciiProjectWizard( )
  {
    super( KalypsoNAProjectWizard.CATEGORY_TEMPLATE, true );

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

  /**
   * @see org.kalypso.afgui.wizards.NewProjectWizard#postCreateProject(org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void postCreateProject( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      // open modell.gml and hydrotop.gml file to write imported feature
      final IPath parameterPath = project.getLocation().append( "/parameter.gml" ); //$NON-NLS-1$
      importParameter( parameterPath, m_asciiBaseDir );
    }
    catch( final Exception e1 )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, e1.getLocalizedMessage(), e1 );
      throw new CoreException( status );
    }

    // TODO: only refresh changed file!
    ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, null );
  }

  public IGMLSchema getParameterSchema( )
  {
    return m_parameterSchema;
  }

  private void importParameter( final IPath paraPath, final File asciiBaseDir ) throws Exception
  {
    final File parameterGmlFile = paraPath.toFile();
    final NAConfiguration ascii2GmlConfiguration = new NAConfiguration( asciiBaseDir, parameterGmlFile );
    final Logger anonymousLogger = Logger.getAnonymousLogger();
    final Feature parameterRootFeature = NAModellConverter.parameterAsciiToFeature( ascii2GmlConfiguration, anonymousLogger );
    final GMLWorkspace paraWorkspace = new GMLWorkspace_Impl( m_parameterSchema, m_parameterSchema.getAllFeatureTypes(), parameterRootFeature, null, null, "http://www.tuhh.de/parameter", null ); //$NON-NLS-1$
    GmlSerializer.serializeWorkspace( new FileWriter( parameterGmlFile ), paraWorkspace );
    System.out.println( Messages.getString( "org.kalypso.ui.rrm.wizards.NewNAAsciiProjectWizard.7" ) + parameterGmlFile.getPath() ); //$NON-NLS-1$
  }
}