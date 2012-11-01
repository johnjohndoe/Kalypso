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

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Gernot Belger
 */
public class Import2dImportData extends FileAndHistoryData
{
  public static final String PROPERTY_SRS = "srs"; //$NON-NLS-1$

  private String m_srs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  public Import2dImportData( )
  {
    super( "importData" ); //$NON-NLS-1$
  }

  @Override
  public void init( final IDialogSettings settings )
  {
    super.init( settings );

    m_srs = DialogSettingsUtils.getString( settings, PROPERTY_SRS, m_srs );
  }

  @Override
  public void storeSettings( final IDialogSettings settings )
  {
    super.storeSettings( settings );

    settings.put( PROPERTY_SRS, m_srs );
  }

  public String getSrs( )
  {
    return m_srs;
  }

  public void setSrs( final String srs )
  {
    final Object oldValue = m_srs;

    m_srs = srs;

    firePropertyChange( PROPERTY_SRS, oldValue, srs );
  }
}