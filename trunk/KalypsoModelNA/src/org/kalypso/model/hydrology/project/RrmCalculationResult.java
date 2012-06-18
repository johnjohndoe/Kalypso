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
package org.kalypso.model.hydrology.project;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;

/**
 * Helper class which encapsulates an rrm calculation result folder
 * 
 * @author Dirk Kuch
 */
public class RrmCalculationResult
{
  public static final String CURRENT_FOLDRER_NAME = "Aktuell"; //$NON-NLS-1$

  public static final String FILE_STATISTICS_CSV = "statistics.csv"; //$NON-NLS-1$

  public static final String FOLDER_BILANZ = "Bilanz"; //$NON-NLS-1$

  public static final String FILE_CALCULATION_LOG = "calculation.log"; //$NON-NLS-1$

  public static final String FILE_BILANZ_TXT = "bilanz.txt"; //$NON-NLS-1$

  private static final String FOLDER_REPORT = "Report"; //$NON-NLS-1$

  private static final String FOLDER_LOG = "Log"; //$NON-NLS-1$

  static final String FILE_OUTPUT_ZIP = "output.zip"; //$NON-NLS-1$

  private static final String FILE_CALCULATION_STATUS_GML = "calculationStatus.gml"; //$NON-NLS-1$

  private static final String FILE_ERROR_GML = "error.gml"; //$NON-NLS-1$

  private final IFolder m_folder;

  public RrmCalculationResult( final IFolder folder )
  {
    m_folder = folder;
  }

  public IFolder getFolder( )
  {
    return m_folder;
  }

  public String getName( )
  {
    return m_folder.getName();
  }

  public IFile getCalculationLog( )
  {
    return getLogFolder().getFile( FILE_CALCULATION_LOG );
  }

  public IFile getOutputZip( )
  {
    return getLogFolder().getFile( FILE_OUTPUT_ZIP );
  }

  public IFile getBilanzTxt( )
  {
    return getBilanzFolder().getFile( FILE_BILANZ_TXT );
  }

  private IFolder getBilanzFolder( )
  {
    return getFolder().getFolder( FOLDER_BILANZ );
  }

  public IFolder getLogFolder( )
  {
    return getFolder().getFolder( FOLDER_LOG );
  }

  public IFile getCalculationStatusGml( )
  {
    return getLogFolder().getFile( FILE_CALCULATION_STATUS_GML );
  }

  public IFile getErrorGml( )
  {
    return getLogFolder().getFile( FILE_ERROR_GML );
  }

  public IFolder getReportFolder( )
  {
    return getFolder().getFolder( FOLDER_REPORT );
  }

  public IFile getStatisticsCsv( )
  {
    return getReportFolder().getFile( FILE_STATISTICS_CSV );
  }

  public IFolder getLzimResultFolder( )
  {
    return getFolder().getFolder( RrmSimulation.FOLDER_ANFANGSWERTE );
  }

  public boolean isCurrent( )
  {
    return StringUtils.equalsIgnoreCase( CURRENT_FOLDRER_NAME, m_folder.getName() );
  }
}
