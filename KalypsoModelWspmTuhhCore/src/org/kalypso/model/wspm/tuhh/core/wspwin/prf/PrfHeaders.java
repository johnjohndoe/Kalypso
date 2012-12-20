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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

/**
 * @author Dirk Kuch
 */
public final class PrfHeaders
{
  private PrfHeaders( )
  {
  }

  /**
   * @deprecated: inline! Each case is used at exactly one place.
   */
  // TODO: this is nonsense, because the given key is 'random' -> probably we should just inline
  @Deprecated
  public static DataBlockHeader createHeader( final String key )
  {
    if( IWspmPointProperties.POINT_PROPERTY_HOEHE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "GELAENDE-", "HOEHE" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "TRENNFLAECHEN" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "DURCHSTROEMTE", "BEREICHE" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "BORDVOLL" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "TRENNLINIE", "WEHR" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "RAUHIGKEIT", "kst   m" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( key ) )
      return new DataBlockHeader( "RAUHIGKEIT", "k-s   m" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmPointProperties.POINT_PROPERTY_RECHTSWERT.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "RECHTSWERT" ); //$NON-NLS-1$

    if( IWspmPointProperties.POINT_PROPERTY_HOCHWERT.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "HOCHWERT" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "UK-BRUECKE" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "OK-BRUECKE" ); //$NON-NLS-1$

    if( IWspmPointProperties.POINT_PROPERTY_COMMENT.equals( key ) )
      // REMARK: Important: Kommmentar MUST be written with lower case letters, else WspWin will not read it...
      return new DataBlockHeader( "Kommentar:" ); //$NON-NLS-1$

    if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "AX   m" ); //$NON-NLS-1$

    if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "AY   m" ); //$NON-NLS-1$

    if( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "DP   m" ); //$NON-NLS-1$

    if( BuildingEi.ID.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "EI", "", IWspWinConstants.SPEZIALPROFIL_EI ); //$NON-NLS-1$ //$NON-NLS-2$

    if( BuildingKreis.ID.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "KREIS", "", IWspWinConstants.SPEZIALPROFIL_KREIS ); //$NON-NLS-1$ //$NON-NLS-2$

    if( BuildingTrapez.ID.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "TRAPEZ", "", IWspWinConstants.SPEZIALPROFIL_TRAPEZ ); //$NON-NLS-1$ //$NON-NLS-2$

    if( BuildingMaul.ID.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "MAULPROFIL", "", IWspWinConstants.SPEZIALPROFIL_MAUL ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "OK-WEHR" ); //$NON-NLS-1$

    // FIXME: we should probably throw an exception here: finding the error afterwards is very difficult
    return new DataBlockHeader( key );
  }
}