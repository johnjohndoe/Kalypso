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
package org.kalypso.model.wspm.pdb.gaf;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;

/**
 * A mapping between GafKind's and part types of WSPM.<br/>
 * Intended to be generalized later for other formats.
 *
 * @author Gernot Belger
 */
public class GafPartsMapping
{
  public String partType2kindName( final String partType )
  {
    switch( partType )
    {
      case BuildingBruecke.ID:
        return GafKind.UK.toString();

      case BuildingWehr.ID:
        return GafKind.OK.toString();

      case BuildingKreis.ID:
        return GafKind.K.name();

      case BuildingEi.ID:
        return GafKind.EI.name();

      case BuildingMaul.ID:
        return GafKind.MA.name();

      case BuildingBruecke.ID_OK:
        return GafKind.OK.name();

      case BuildingTrapez.ID:
        return IGafConstants.KIND_TR;

      case SinuositaetProfileObject.ID:
        return IGafConstants.KIND_SINUOSITAET;

      case EnergylossProfileObject.ID:
        return IGafConstants.KIND_ENERGYLOSS;

      case IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS:
        return GafKind.W.toString();

      case IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT:
        return IGafConstants.KIND_W2D;

      default:
        // Must be a generic type, just return
        return partType;
    }
  }

  /**
   * Maps names of 'gaf kinds' to wspm part types.<br/>
   * We do not use {@link GafKind} directly, because it really maps pdb part types (which are a super set of GafKind's) to wspm part types.
   */
  public String kind2partType( final String gafKindName )
  {
    if( gafKindName == null )
      return null;

    final GafKind kind = asKind( gafKindName );
    if( kind != null )
      return kind2SpecialPart( kind );

    switch( gafKindName )
    {
      case IGafConstants.KIND_TR:
        return BuildingTrapez.ID;

      case IGafConstants.KIND_SINUOSITAET:
        return SinuositaetProfileObject.ID;

      case IGafConstants.KIND_ENERGYLOSS:
        return EnergylossProfileObject.ID;

      case IGafConstants.KIND_W2D:
        return IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT;

      default:
        return gafKindName;
    }
  }

  private String kind2SpecialPart( final GafKind kind )
  {
    switch( kind )
    {
    /* Ignore p horizont, because this one should be already imported. */
      case P:
        return null;

      case UK:
        return BuildingBruecke.ID;

      case K:
        return BuildingKreis.ID;

      case EI:
        return BuildingEi.ID;

      case MA:
        return BuildingMaul.ID;

      case OK:
        return BuildingBruecke.ID_OK;

      case W:
        return IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS;

      default:
        /* 'generic' using kind as id */
        return kind.name();
    }
  }

  private GafKind asKind( final String gafKindeName )
  {
    try
    {
      return GafKind.valueOf( gafKindeName );
    }
    catch( final IllegalArgumentException e )
    {
      return null;
    }
  }
}