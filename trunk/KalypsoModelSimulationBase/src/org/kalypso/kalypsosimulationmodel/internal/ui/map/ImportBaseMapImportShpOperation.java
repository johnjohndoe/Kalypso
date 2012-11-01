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
package org.kalypso.kalypsosimulationmodel.internal.ui.map;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.wizard.shape.ImportShapeFileData;
import org.kalypso.ui.wizard.shape.ImportShapeFileData.StyleImport;
import org.kalypso.ui.wizard.shape.ImportShapeOperation;

/**
 * @author Gernot Belger
 */
public class ImportBaseMapImportShpOperation implements IImportBaseMapOperation
{
  private final IPath m_sourceLocation;

  private final String m_sourceSrs;

  private final ICommandTarget m_cmdTarget;

  private final IKalypsoLayerModell m_mapModell;

  private final IFolder m_scenarioFolder;

  private final File m_srcFileShape;

  private final IFile m_dstFileShape;

  private final File m_srcFileIndex;

  private final IFile m_dstFileIndex;

  private final File m_srcFileDBase;

  private final IFile m_dstFileDBase;

  private final ImportShapeOperation m_operation;

  public ImportBaseMapImportShpOperation( final IPath sourceLocation, final String sourceSrs, final ICommandTarget cmdTarget, final IKalypsoLayerModell mapModell, final IFolder scenarioFolder )
  {
    m_sourceLocation = sourceLocation;
    m_sourceSrs = sourceSrs;
    m_cmdTarget = cmdTarget;
    m_mapModell = mapModell;
    m_scenarioFolder = scenarioFolder;

    final IPath destinationPath = new Path( "imports" ).append( "basemap" ); //$NON-NLS-1$ //$NON-NLS-1$ //$NON-NLS-2$
    final IFolder destinationFolder = m_scenarioFolder.getProject().getFolder( destinationPath );

    final IPath sourceBase = m_sourceLocation.removeFileExtension();

    m_srcFileShape = new File( m_sourceLocation.toOSString() );
    m_srcFileIndex = new File( sourceBase.addFileExtension( "shx" ).toOSString() ); //$NON-NLS-1$
    m_srcFileDBase = new File( sourceBase.addFileExtension( "dbf" ).toOSString() ); //$NON-NLS-1$

    m_dstFileShape = destinationFolder.getFile( m_sourceLocation.lastSegment() );
    m_dstFileIndex = destinationFolder.getFile( sourceBase.addFileExtension( "shx" ).lastSegment() ); //$NON-NLS-1$
    m_dstFileDBase = destinationFolder.getFile( sourceBase.addFileExtension( "dbf" ).lastSegment() ); //$NON-NLS-1$

    /* Add theme to map */
    final ImportShapeFileData importShapeData = new ImportShapeFileData();
    importShapeData.setInsertionIndex( 0 );
    importShapeData.setSrs( m_sourceSrs );

    // REMARK: setting the shape path after the import type has the effect, that, if an sld file already exists,
    // it will always be reused. This is intended.
    importShapeData.setStyleImportType( StyleImport.generateDefault );
    importShapeData.getShapeFile().setPath( m_dstFileShape.getFullPath() );

    m_operation = new ImportShapeOperation( importShapeData );
  }

  @Override
  public boolean checkPreconditions( final Shell shell, final String windowTitle )
  {
    // TODO: check if destination files exists and ask if we can overwrite

    // TODO: check, if a layer with the same data already exists; if yes ask if we should update or
    // if we should add a new layer

    return m_operation.checkPrecondition( shell, windowTitle );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IContainer destinationFolder = m_dstFileShape.getParent();

    FolderUtilities.mkdirs( destinationFolder );

    ImportBaseMapUtils.copy( m_srcFileShape, m_dstFileShape, monitor );
    ImportBaseMapUtils.copy( m_srcFileIndex, m_dstFileIndex, monitor );
    ImportBaseMapUtils.copy( m_srcFileDBase, m_dstFileDBase, monitor );

    final ICommand command = m_operation.createCommand( m_mapModell );
    m_cmdTarget.postCommand( command, null );
    return Status.OK_STATUS;
  }
}