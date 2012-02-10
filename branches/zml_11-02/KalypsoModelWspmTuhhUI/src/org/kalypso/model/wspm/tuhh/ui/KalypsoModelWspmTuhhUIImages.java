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
package org.kalypso.model.wspm.tuhh.ui;

import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Utility class for handling images in this plugin.
 * 
 * @author Gernot Belger
 */
public enum KalypsoModelWspmTuhhUIImages implements ImageKey
{
  NEWPROJECT_PROJECT_PAGE_WIZBAN("icons/wizban/kalypso32.gif"), //$NON-NLS-1$
  WSP_TIN("icons/obj16/wspTin.gif"), //$NON-NLS-1$
  ADD_CSV_EXPORT_COLUMN("icons/obj16/addCsvExportColumn.gif"), //$NON-NLS-1$
  ADD_CSV_EXPORT_RESULT_COLUMNS("icons/obj16/addCsvExportResultColumns.gif"), //$NON-NLS-1$
  REMOVE_CSV_EXPORT_COLUMN("icons/obj16/removeCsvExportColumn.gif"), //$NON-NLS-1$
  REMOVE_CSV_CONFIGURATION("icons/obj16/removeCsvConfiguration.gif"), //$NON-NLS-1$
  ROUGHNESS_PANEL_ADD("icons/obj16/roughnessPanelAdd.gif"), //$NON-NLS-1$
  ROUGHNESS_PANEL_REMOVE("icons/obj16/roughnessPanelRemove.gif"); //$NON-NLS-1$

  private final String m_imagePath;

  private KalypsoModelWspmTuhhUIImages( final String imagePath )
  {
    m_imagePath = imagePath;
  }

  /**
   * @see org.kalypso.informdss.KalypsoInformDSSImages.ImageKey#getImagePath()
   */
  @Override
  public String getImagePath( )
  {
    return m_imagePath;
  }
}
