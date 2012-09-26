/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.ui.internal.gaf;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;

/**
 * Data class for options for gaf export.
 *
 * @author Gernot Belger
 */
class GafOptionsData extends AbstractModelObject
{
  static String PROPERTY_HYK_EXPORT_MODE = "hykExportMode"; //$NON-NLS-1$

  private final FileAndHistoryData m_gafFile = new FileAndHistoryData( "gaf" ); //$NON-NLS-1$

  private HykExportMode m_hykExportMode = HykExportMode.MARKER;

  public void init( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    final String hykExportMode = settings.get( PROPERTY_HYK_EXPORT_MODE );
    try
    {
      m_hykExportMode = HykExportMode.valueOf( hykExportMode );
    }
    catch( final Exception e )
    {
      // ignore, unknown ornot set
    }

    m_gafFile.init( settings );
  }

  public void store( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    settings.put( PROPERTY_HYK_EXPORT_MODE, m_hykExportMode.name() );

    m_gafFile.storeSettings( settings );
  }

  public HykExportMode getHykExportMode( )
  {
    return m_hykExportMode;
  }

  public void setHykExportMode( final HykExportMode hykExportMode )
  {
    final HykExportMode oldValue = hykExportMode;

    m_hykExportMode = hykExportMode;

    firePropertyChange( PROPERTY_HYK_EXPORT_MODE, oldValue, hykExportMode );
  }

  public FileAndHistoryData getGafFile( )
  {
    return m_gafFile;
  }
}