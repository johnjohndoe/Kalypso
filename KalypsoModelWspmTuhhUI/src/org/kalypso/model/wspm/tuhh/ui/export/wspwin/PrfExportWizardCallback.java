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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.IProfilePatternData;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternData;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.IWaterlevel;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.Waterlevel;

public abstract class PrfExportWizardCallback implements IPrfExporterCallback
{
  private final Set<String> m_filenames = new HashSet<String>();

  private final File m_exportDirectory;

  private final String m_filenamePattern;

  private final WspmResultLengthSection[] m_results;

  public PrfExportWizardCallback( final File exportDir, final String filenamePattern, final WspmResultLengthSection[] results )
  {
    m_exportDirectory = exportDir;
    m_filenamePattern = filenamePattern;
    m_results = results;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.wspwin.IPrfExporterCallback#getExportFile(org.kalypso.model.wspm.core.gml.IProfileFeature,
   *      org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public File getExportFile( final IProfileFeature feature, final IProfil profil )
  {
    final IProfilePatternData data = new ProfilePatternData( feature, profil, null );
    final String fileName = ProfilePatternInputReplacer.getINSTANCE().replaceTokens( m_filenamePattern, data );
    final String uniqueFileName = createUniqueFilename( fileName );
    final String cleanFileName = cleanupFilename( uniqueFileName );
    return new File( m_exportDirectory, cleanFileName + ".prf" ); //$NON-NLS-1$
  }

  private String createUniqueFilename( final String fileName )
  {
    String uniqueName = fileName;

    for( int i = 0; i < Integer.MAX_VALUE && m_filenames.contains( uniqueName ); i++ )
      uniqueName = fileName + i;

    m_filenames.add( uniqueName );
    return uniqueName;
  }

  private String cleanupFilename( final String fileName )
  {
    String result = fileName;
    result = fileName.replace( '#', '_' );
    result = fileName.replace( ':', '_' );
    result = fileName.replace( ' ', '_' );
    result = fileName.replace( ' ', '_' );
    return result;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.wspwin.IPrfExporterCallback#getWaterlevels(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public IWaterlevel[] getWaterlevels( final IProfil profil )
  {
    final double station = profil.getStation();
    final BigDecimal bigStation = ProfilUtil.stationToBigDecimal( station );

    final Collection<IWaterlevel> waterlevels = new ArrayList<IWaterlevel>( m_results.length );

    for( final WspmResultLengthSection result : m_results )
    {
      final Object waterlevel = result.getValue( bigStation, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL );
      final Object discharge = result.getValue( bigStation, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_RUNOFF );
      final String label = result.getLabel();

      if( waterlevel instanceof Number && discharge instanceof Number )
        waterlevels.add( new Waterlevel( ((Number) waterlevel).doubleValue(), ((Number) discharge).doubleValue(), label ) );
    }

    return waterlevels.toArray( new IWaterlevel[waterlevels.size()] );
  }

}
