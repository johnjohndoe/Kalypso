/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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
package org.kalypso.model.wspm.tuhh.ui.rules;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelDeviderResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;
import org.kalypso.observation.result.IComponent;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author kimwerner
 */
public class BrueckeRule extends AbstractValidatorRule
{

  private boolean validateParams( final IProfileBuilding building, final ProfileAltitudeValidator pav ) throws CoreException
  {
    for( final IComponent property : building.getObjectProperties() )
    {
      final Object oValue = building.getValue( property );
      if( oValue == null || (oValue instanceof Double && ((Double) oValue).isNaN()) )
      {
        pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.0", property.getName() ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
        return false;
      }
    }
    return true;
  }

  private boolean validateBankfullPoints( final ProfileAltitudeValidator pav, final IProfil profil ) throws CoreException
  {
    final IProfilPointMarker[] brdvp = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    if( brdvp.length > 0 )
    {
      final IProfilMarkerResolution delRes = new DelDeviderResolution( -1, IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.19" ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, delRes );//$NON-NLS-1$
      return false;
    }
    return true;
  }

  @Override
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final IProfileBuilding building = WspmProfileHelper.getBuilding( profil, IProfileBuilding.class );
    if( building == null )
      return;
    if( !IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( building.getId() ) )
      return;

    final ProfileAltitudeValidator pav = new ProfileAltitudeValidator( profil, collector );
    final int pointsCount = profil.getPoints().length;

    // validierung ohne Brückengeometrie möglich
    if( !validateParams( building, pav ) || !validateBankfullPoints( pav, profil ) )
      return;

    // Brückengeometrie ermitteln

    final IProfilPointMarker[] trenner = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    final int markerLeft = trenner.length > 1 ? profil.indexOfPoint( trenner[0].getPoint() ) : -1;
    final int markerRight = trenner.length > 1 ? profil.indexOfPoint( trenner[trenner.length - 1].getPoint() ) : -1;
    // Trennflächen vorhanden
    if( markerLeft == -1 || markerRight == -1 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.1" ), 0, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ); //$NON-NLS-1$
      return;
    }
    // ersten BrückenOberkantenpunkt von Links
    final int pLOK = pav.whileNaN( 0, pointsCount, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    // ersten BrückenOberkantenpunkt von Rechts
    final int pROK = pav.whileNaN( pointsCount - 1, pLOK, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    // ersten BrückenUnterkantenpunkt von Links
    final int pLUK = pav.whileNaN( 0, pointsCount, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    // ersten BrückenUnterkantenpunkt von Rechts
    final int pRUK = pav.whileNaN( pointsCount - 1, pLUK, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );

    // Flußschlauch
    final int iL = pav.whileEqual( pLUK, pRUK, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    final int innerLeft = iL < 0 ? pLUK : iL;
    final int iR = pav.whileEqual( pRUK, pLUK, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    final int innerRight = iR < 0 ? pRUK : iR;
    if( innerLeft == -1 || innerRight == -1 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.2" ), 0, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ); //$NON-NLS-1$
      return;
    }

    // Trennflächen position

    if( innerRight != markerRight )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.9" ), markerRight, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, new MoveDeviderResolution( 1, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, innerRight ) );//$NON-NLS-1$
      return;
    }
    if( innerLeft != markerLeft )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.9" ), markerLeft, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, new MoveDeviderResolution( 0, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, innerLeft ) );//$NON-NLS-1$
      return;
    }

    // BrückenOberkante > Brückenunterkante
    // sicher den ersten okB < minOK
    // sicher den ersten ukB > maxUK
    Double minOK = Double.MAX_VALUE;
    Double maxUK = -Double.MAX_VALUE;
    for( int i = innerLeft; i < innerRight; i++ )
    {
      final double okB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pav.getPoints()[i] );
      final double ukB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, pav.getPoints()[i] );
      if( !Double.isNaN( okB ) )
        minOK = Math.min( minOK, okB );
      if( !Double.isNaN( ukB ) )
        maxUK = Math.max( maxUK, ukB );
      // min Oberkante > max Unterkante
      if( maxUK > minOK )
      {
        pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.25" ), i, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
        return;
      }
    }
    final int outL = pav.whileUpper( pLOK, innerLeft, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, true );
    final int outerLeft = outL < 0 ? pLOK : outL;
    final int outR = pav.whileUpper( pROK, innerRight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, true );
    final int outerRight = outR < 0 ? pROK : outR;

    // Wiederlager
    if( innerRight == outerRight )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.5" ), innerRight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
      return;
    }
    if( innerLeft == outerLeft )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.5" ), innerLeft, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
      return;
    }

    // Schnittkanten
    final int duh = pav.whileUpper( outerLeft, outerRight, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, true );
    if( duh > -1 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.23" ), duh, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );//$NON-NLS-1$
      return;
    }
    // Schnittkanten
    final int doh = pav.whileUpper( outerLeft, outerRight < pointsCount - 1 ? outerRight + 1 : outerRight, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, true );
    if( doh > -1 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.23" ), doh, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
      return;
    }

  }
}
