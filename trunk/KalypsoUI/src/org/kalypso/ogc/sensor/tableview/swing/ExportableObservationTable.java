/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview.swing;

import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.ogc.sensor.ExportUtilities;
import org.kalypso.ogc.sensor.tableview.swing.tablemodel.ObservationTableModel;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * ExportableObservationTable
 * 
 * @author schlienger
 */
public class ExportableObservationTable implements IExportableObject
{
  private final ObservationTable m_table;

  private final String m_identifierPrefix;

  private final String m_category;

  private final String m_stationIDs;

  public ExportableObservationTable( final ObservationTable table, final String identifierPrefix, final String category )
  {
    m_table = table;
    m_identifierPrefix = identifierPrefix;
    m_category = category;
    m_stationIDs = ExportUtilities.extractStationIDs( m_table.getTemplate().getItems() );
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getPreferredDocumentName()
   */
  public String getPreferredDocumentName( )
  {
    return FileUtilities.validateName( "Tabelle.csv", "_" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#exportObject(java.io.OutputStream,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus exportObject( final OutputStream output, final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "ExportableObservationTable.1" ), 2 ); //$NON-NLS-1$

    final BufferedWriter writer = new BufferedWriter( new OutputStreamWriter( output ) );
    try
    {
      // scenario name header
      if( !m_table.getCurrentScenarioName().equals( "" ) ) //$NON-NLS-1$
      {
        writer.write( m_table.getCurrentScenarioName() );
        int columnCount = m_table.getObservationTableModel().getColumnCount() - 1;
        for( int i = 0; i < columnCount; i++ )
          writer.write( ";" ); //$NON-NLS-1$
        writer.newLine();
      }

      monitor.worked( 1 );

      // normal table dump
      final ObservationTableModel model = m_table.getObservationTableModel();
      model.dump( ";", writer ); //$NON-NLS-1$

      monitor.worked( 1 );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, Messages.getString( "ExportableObservationTable.5" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( writer );
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_identifierPrefix + getPreferredDocumentName();
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getCategory()
   */
  public String getCategory( )
  {
    return m_category;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getStationIDs()
   */
  @Override
  public String getStationIDs( )
  {
    return m_stationIDs;
  }
}