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
package org.kalypso.model.wspm.pdb.internal.wspm;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class PPPartBuilder implements IPartBuilder
{
  private final IProfilPointMarker[] m_tfMarkers;

  private final IProfilPointMarker[] m_bvMarkers;

  private final IProfilPointMarker[] m_dbMarkers;

  public PPPartBuilder( final IProfil profil )
  {
    m_tfMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    m_bvMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    m_dbMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
  }

  @Override
  public String getCategory( )
  {
    return IGafConstants.KZ_CATEGORY_PROFILE;
  }

  @Override
  public String getHeightComponent( )
  {
    return IWspmConstants.POINT_PROPERTY_HOEHE;
  }

  @Override
  public String guessCode( final IRecord[] records, final int i )
  {
    final String markerCode = guessMarkerCode( records[i] );
    if( markerCode != null )
      return markerCode;

    return IGafConstants.CODE_PP;
  }

  private String guessMarkerCode( final IRecord record )
  {
    final String codeTF = checkMarker( record, m_tfMarkers, IGafConstants.CODE_LU, IGafConstants.CODE_RU );
    if( codeTF != null )
      return codeTF;

    final String codeBV = checkMarker( record, m_bvMarkers, IGafConstants.CODE_LBOK, IGafConstants.CODE_RBOK );
    if( codeBV != null )
      return codeBV;

    final String codeBD = checkMarker( record, m_dbMarkers, IGafConstants.CODE_PA, IGafConstants.CODE_PE );
    if( codeBD != null )
      return codeBD;

    return null;
  }

  private static String checkMarker( final IRecord record, final IProfilPointMarker[] markers, final String codeStart, final String codeEnd )
  {
    for( final IProfilPointMarker marker : markers )
    {
      if( marker.getPoint() == record )
      {
        if( marker == markers[0] )
          return codeStart;
        else
          return codeEnd;
      }
    }

    return null;
  }
}