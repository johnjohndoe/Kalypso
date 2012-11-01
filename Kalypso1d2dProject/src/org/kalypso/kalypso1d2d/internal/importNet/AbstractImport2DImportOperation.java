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
package org.kalypso.kalypso1d2d.internal.importNet;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.IPolygonWithName;

/**
 * @author Gernot Belger
 */
public abstract class AbstractImport2DImportOperation implements IImport2dImportOperation
{
  private final Import2dImportData m_importData;

  private final Import2dElementsData m_data;

  public AbstractImport2DImportOperation( final Import2dElementsData data, final Import2dImportData importData )
  {
    m_data = data;
    m_importData = importData;
  }

  @Override
  public final IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( Messages.getString("AbstractImport2DImportOperation_0"), 100 ); //$NON-NLS-1$

    /* Read the file */
    final String srs = m_importData.getSrs();
    final int sourceSrid = JTSAdapter.toSrid( srs );
    final File importFile = m_importData.getFile();

    monitor.subTask( String.format( Messages.getString("AbstractImport2DImportOperation_1"), importFile.getName() ) ); //$NON-NLS-1$

    final Pair<IStatus, IPolygonWithName[]> readData = readFileData( importFile, sourceSrid, new SubProgressMonitor( monitor, 33 ) );

    final IStatus readStatus = readData.getKey();
    final IPolygonWithName[] value = readData.getValue();

    if( readStatus.matches( IStatus.ERROR ) )
    {
      /* Stop here, old data remains unchanged */
      return readStatus;
    }

    /* Check for cancel */
    ProgressUtilities.worked( monitor, 0 );

    monitor.subTask( String.format( Messages.getString("AbstractImport2DImportOperation_2"), importFile.getName() ) ); //$NON-NLS-1$

    // TODO: analyse data

    /* Check for cancel */
    ProgressUtilities.worked( monitor, 0 );

    m_data.setElements( value );
    m_data.setLastReadStatus( readStatus );

    /* always only return read status, the analyse status is shown in the details panel */
    return readStatus;
  }

  protected abstract Pair<IStatus, IPolygonWithName[]> readFileData( File importFile, int sourceSrid, IProgressMonitor monitor ) throws InvocationTargetException, CoreException;
}