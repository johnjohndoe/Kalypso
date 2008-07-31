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
package org.kalypso.ogc.gml.table.wizard;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.io.CSV;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.ogc.gml.table.LayerTableViewer;

/**
 * ExportableTableDocument
 * 
 * @author schlienger
 */
public class ExportableLayerTable implements IExportableObject
{
  private final LayerTableViewer m_layerTable;

  private boolean m_onlyRows = false;

  public ExportableLayerTable( final LayerTableViewer layerTable )
  {
    m_layerTable = layerTable;
  }

  public void setOnlySelectedRows( final boolean flag )
  {
    m_onlyRows = flag;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getPreferredDocumentName()
   */
  public String getPreferredDocumentName( )
  {
    // TODO besserer Name zurückgeben
    return FileUtilities.validateName( "GisTabelle.csv", "_" ); //$NON-NLS-1$ $NON-NLS-2$
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#exportObject(java.io.OutputStream,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus exportObject( final OutputStream output, final IProgressMonitor monitor )
  {
    final LayerTableViewer layerTable = m_layerTable;
    final boolean onlyRows = m_onlyRows;

    final Runnable runnable = new Runnable()
    {
      public void run( )
      {
        final String[][] csv = layerTable.exportTable( onlyRows );
        final PrintWriter pw = new PrintWriter( new OutputStreamWriter( output ) );
        CSV.writeCSV( csv, pw );
        pw.close();
      }
    };

    layerTable.getTable().getDisplay().syncExec( runnable );

    return Status.OK_STATUS;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getIdentifier()
   */
  public String getIdentifier( )
  {
    // TODO bessere Id zurückgeben
    return getPreferredDocumentName();
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getCategory()
   */
  public String getCategory( )
  {
    // TODO bessere Category zurückgeben
    return Messages.getString( "org.kalypso.ogc.gml.table.command.ExportableLayerTable.1" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getStationIDs()
   */
  @Override
  public String getStationIDs( )
  {
    return "";
  }
}