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

package KalypsoPluginRasterExport;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;

/**
 * 
 * 
 * Selection with information needed to export a raster from gml to another format (e.g. ascii)
 * 
 * @author Nadja Peiler
 */
public class RasterExportSelection implements ISelection
{

  private final File m_fileTarget;

  private final IPath m_pathSource;

  private final String m_format;

  private IProject m_selectedProject;

  public RasterExportSelection( IPath pathSource, IProject selectedProject, File fileTarget, String format )
  {
    m_pathSource = pathSource;
    m_selectedProject = selectedProject;
    m_fileTarget = fileTarget;
    m_format = format;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelection#isEmpty()
   */
  public boolean isEmpty()
  {
    return m_pathSource == null || m_fileTarget == null;
  }

  public IPath getPathSource()
  {
    return m_pathSource;
  }

  public File getSourceFile()
  {
    return new File( m_selectedProject.getLocation() + "/" + m_pathSource.removeFirstSegments( 1 ).toString() );
  }

  public IProject getProject()
  {
    return m_selectedProject;
  }

  public File getFileTarget()
  {
    return m_fileTarget;
  }

  public String getTargetFormat()
  {
    return m_format;
  }
}