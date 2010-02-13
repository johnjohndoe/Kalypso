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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.wprof.BCEShapeWPRofContentProviderFactory;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPointFactory;
import org.kalypso.model.wspm.tuhh.core.wprof.WProfImporter;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 */
public class WProfImportOperation implements ICoreRunnableWithProgress
{
  private final File m_wprofShape;

  private String m_shapeDefaultSrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private final TuhhWspmProject m_targetProject;

  private URL m_photoContext;

  private final CommandableWorkspace m_workspace;

  public WProfImportOperation( final File wprofShape, final CommandableWorkspace workspace, final TuhhWspmProject targetProject )
  {
    m_wprofShape = wprofShape;
    m_workspace = workspace;
    m_targetProject = targetProject;
  }

  public void setShapeDefaultSrs( final String shapeDefaultSrs )
  {
    m_shapeDefaultSrs = shapeDefaultSrs;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    try
    {
      doImport( monitor );
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final GmlSerializeException e )
    {
      throw new InvocationTargetException( e );
    }

    return Status.OK_STATUS;
  }

  private void doImport( final IProgressMonitor monitor ) throws IOException, GmlSerializeException, CoreException
  {
    final String shapeFilePath = m_wprofShape.getAbsolutePath();
    final String shapeFileBase = FilenameUtils.removeExtension( shapeFilePath );

    final IWProfPointFactory pointFactory = new BCEShapeWPRofContentProviderFactory();

    final WProfImporter wProfImporter = new WProfImporter( shapeFileBase, pointFactory );
    wProfImporter.setShapeDefaultSrs( m_shapeDefaultSrs );

    /* Create Empty WSPM-Workspace */
    final String targetSrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final TuhhProfileWProfContentHandler handler = new TuhhProfileWProfContentHandler( m_targetProject, targetSrs, m_photoContext );

    wProfImporter.addHandler( handler );

    wProfImporter.importW80Shape( monitor );

    fireChangeEvents();
  }

  private void fireChangeEvents( )
  {
    final WspmWaterBody[] waterBodies = m_targetProject.getWaterBodies();
    final Feature[] changedFeatures = new Feature[waterBodies.length];
    for( int i = 0; i < waterBodies.length; i++ )
      changedFeatures[i] = waterBodies[i].getFeature();

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_targetProject.getFeature(), changedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    try
    {
      m_workspace.postCommand( new EmptyCommand( "", false ) );
    }
    catch( final Exception e )
    {
      // does not happen
      e.printStackTrace();
    }
  }

  public void setPhotoContext( final URL photoContext )
  {
    m_photoContext = photoContext;
  }

}
