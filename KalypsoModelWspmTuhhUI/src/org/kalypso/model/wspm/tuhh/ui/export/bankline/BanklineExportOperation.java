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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.commands.exportshape.ExportShapeOperation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.shape.deegree.IShapeDataFactory;

/**
 * @author Gernot Belger
 */
public class BanklineExportOperation implements ICoreRunnableWithProgress
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

  private final BanklineExportData m_data;

  public BanklineExportOperation( final BanklineExportData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString("BanklineExportOperation_0"), 100 ); //$NON-NLS-1$

    final IShapeDataFactory shapeDataFactory = createShapeData( new SubProgressMonitor( monitor, 90 ) );

    exportShape( new SubProgressMonitor( monitor, 10 ), shapeDataFactory );

    return m_log.asMultiStatusOrOK( null, Messages.getString("BanklineExportOperation_1") ); //$NON-NLS-1$
  }

  private IShapeDataFactory createShapeData( final IProgressMonitor monitor )
  {
    final BanklineExportShapeWorker worker = new BanklineExportShapeWorker( m_data );
    final IStatus status = worker.execute( monitor );
    m_log.add( status );
    return worker.getShapeData();
  }

  private void exportShape( final IProgressMonitor monitor, final IShapeDataFactory shapeDataFactory )
  {
    try
    {
      final ExportShapeOperation operation = new ExportShapeOperation( m_data.getExportShapeBase(), shapeDataFactory, m_data.getWritePrj() );
      final IStatus status = operation.execute( monitor );
      if( status.isOK() )
        m_log.add( IStatus.OK, Messages.getString("BanklineExportOperation_2") ); //$NON-NLS-1$
      else
        m_log.add( status );
    }
    catch( final CoreException e )
    {
      m_log.add( e.getStatus() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      m_log.add( IStatus.ERROR, Messages.getString("BanklineExportOperation_3"), e ); //$NON-NLS-1$
    }
  }
}