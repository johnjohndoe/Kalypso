/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author Gernot Belger
 */
public class PPPartBuilder implements IPartBuilder
{
  private final IProfilePointMarker[] m_tfMarkers;

  private final IProfilePointMarker[] m_bvMarkers;

  private final IProfilePointMarker[] m_dbMarkers;

  public PPPartBuilder( final IProfile profil )
  {
    m_tfMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    m_bvMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    m_dbMarkers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
  }

  @Override
  public GafKind getKind( )
  {
    return GafKind.P;
  }

  @Override
  public String getHeightComponent( )
  {
    return IWspmPointProperties.POINT_PROPERTY_HOEHE;
  }

  @Override
  public String guessCode( final IProfileRecord[] records, final int i )
  {
    return IGafConstants.CODE_PP;
  }

  @Override
  public String getHykCode( final IProfileRecord record )
  {
    final Collection<String> codes = new ArrayList<>( 3 );

    final String codeTF = checkMarker( record, m_tfMarkers, IGafConstants.CODE_LU, IGafConstants.CODE_RU );
    if( codeTF != null )
      codes.add( codeTF );

    final String codeBV = checkMarker( record, m_bvMarkers, IGafConstants.CODE_LBOK, IGafConstants.CODE_RBOK );
    if( codeBV != null )
      codes.add( codeBV );

    final String codeBD = checkMarker( record, m_dbMarkers, IGafConstants.CODE_PA, IGafConstants.CODE_PE );
    if( codeBD != null )
      codes.add( codeBD );

    if( codes.isEmpty() )
      return null;

    return StringUtils.join( codes, ',' );
  }

  private static String checkMarker( final IProfileRecord record, final IProfilePointMarker[] markers, final String codeStart, final String codeEnd )
  {
    for( final IProfilePointMarker marker : markers )
    {
      if( marker.getPoint().getRecord() == record.getRecord() )
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