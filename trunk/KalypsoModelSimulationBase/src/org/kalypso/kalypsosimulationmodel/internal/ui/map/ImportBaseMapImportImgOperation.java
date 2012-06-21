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
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.grid.WorldFileReader;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.action.AddThemeCommand;

/**
 * @author Gernot Belger
 */
public class ImportBaseMapImportImgOperation implements IImportBaseMapOperation
{
  private final ICommandTarget m_cmdTarget;

  private final IKalypsoLayerModell m_mapModell;

  private final IFolder m_scenarioFolder;

  private final IPath m_sourceLocation;

  private final String m_srs;

  public ImportBaseMapImportImgOperation( final IPath sourceLocation, final String srs, final ICommandTarget cmdTarget, final IKalypsoLayerModell mapModell, final IFolder scenarioFolder )
  {
    m_sourceLocation = sourceLocation;
    m_srs = srs;
    m_cmdTarget = cmdTarget;
    m_mapModell = mapModell;
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public boolean checkPreconditions( final Shell shell, final String windowTitle )
  {
    // TODO: check if file exists and ask user if overwrite is ok
    return true;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IFolder importsFolder = m_scenarioFolder.getProject().getFolder( "imports" ); //$NON-NLS-1$
    final IFolder dstFileFolder = importsFolder.getFolder( "basemap" ); //$NON-NLS-1$

    FolderUtilities.mkdirs( dstFileFolder );

    final File srcFileImage = new File( m_sourceLocation.toString() );
    final IFile dstFileImage = dstFileFolder.getFile( srcFileImage.getName() );

    final File srcFileGeoreference = WorldFileReader.findWorldFile( srcFileImage );
    if( srcFileGeoreference == null )
      throw new UnsupportedOperationException( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapWizard.0" ) + ": " + m_sourceLocation.getFileExtension() ); //$NON-NLS-1$ //$NON-NLS-2$

    final IFile dstFileGeoreference = dstFileFolder.getFile( srcFileGeoreference.getName() );

    if( !dstFileFolder.exists() )
      dstFileFolder.create( true, true, monitor );

    ImportBaseMapUtils.copy( srcFileImage, dstFileImage, monitor );
    ImportBaseMapUtils.copy( srcFileGeoreference, dstFileGeoreference, monitor );

    final String type = m_sourceLocation.getFileExtension();
    final String layerName = m_sourceLocation.removeFileExtension().lastSegment();

    final URL context = m_mapModell.getContext();
    final IFile mapFile = ResourceUtilities.findFileFromURL( context );
    final IPath relativeDstPath = ResourceUtilities.makeRelativ( mapFile, dstFileImage );

    //    final String imgHref = "project:" + File.separator + "imports" + File.separator + "basemap" + File.separator + sourceLocation.lastSegment() + "#" + coordinateSystem; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    final String imgHref = String.format( "%s#%s", relativeDstPath.toString(), m_srs ); //$NON-NLS-1$
    final AddThemeCommand command = new AddThemeCommand( m_mapModell, layerName, type, imgHref );
    m_cmdTarget.postCommand( command, null );

    return Status.OK_STATUS;
  }
}