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

import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.reparator.IProfileMarkerResolution;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelDeviderResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen. Oberkante darf nicht unter Unterkante.
 *
 * @author Kim Werner
 */
public class BrueckeRule extends AbstractValidatorRule
{
  private boolean validateParams( final BuildingBruecke building, final ProfileAltitudeValidator pav ) throws CoreException
  {
    /* brückenbreite */
    if( !validateParam( building.getBreite(), BuildingBruecke.KEY_BREITE, pav ) )
      return false;

    /* unterwasser */
    if( !validateParam( building.getUnterwasser(), BuildingBruecke.KEY_UNTERWASSER, pav ) )
      return false;

    /* formbeiwert */
    if( !validateIsSet( building.getFormbeiwert(), BuildingBruecke.KEY_FORMBEIWERT, pav ) )
      return false;
    if( !validateIsGreaterEqualZero( building.getFormbeiwert(), BuildingBruecke.KEY_FORMBEIWERT, pav ) )
      return false;

    /* roguhness */
    if( !validateParam( building.getRauheit(), BuildingBruecke.KEY_RAUHEIT, pav ) )
      return false;

    return true;
  }

  private boolean validateParam( final Double oValue, final String propertyName, final ProfileAltitudeValidator pav ) throws CoreException
  {
    if( !validateIsSet( oValue, propertyName, pav ) )
      return false;

    return validateIsGreaterZero( oValue, propertyName, pav );
  }

  private boolean validateIsSet( final Double oValue, final String propertyName, final ProfileAltitudeValidator pav ) throws CoreException
  {
    if( oValue != null && !oValue.isNaN() )
      return true;

    pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.0", propertyName ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
    return false;
  }

  private boolean validateIsGreaterZero( final Double oValue, final String propertyName, final ProfileAltitudeValidator pav ) throws CoreException
  {
    if( oValue.doubleValue() <= 0.0 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.1", propertyName ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
      return false;
    }

    return true;
  }

  private boolean validateIsGreaterEqualZero( final Double oValue, final String propertyName, final ProfileAltitudeValidator pav ) throws CoreException
  {
    if( oValue.doubleValue() < 0.0 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.3", propertyName ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
      return false;
    }

    return true;
  }

  private boolean validateBankfullPoints( final ProfileAltitudeValidator pav, final IProfile profil ) throws CoreException
  {
    final IProfilePointMarker[] brdvp = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    if( brdvp.length > 0 )
    {
      final IProfileMarkerResolution delRes = new DelDeviderResolution( -1, IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.19" ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, delRes );//$NON-NLS-1$
      return false;
    }
    return true;
  }

  @Override
  public void validate( final IProfile profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final BuildingBruecke building = WspmSohlpunkte.getBuilding( profil, BuildingBruecke.class );
    if( building == null )
      return;

    final ProfileAltitudeValidator pav = new ProfileAltitudeValidator( profil, collector );
    final int pointsCount = profil.getPoints().length;

    // validierung ohne Brückengeometrie möglich
    if( !validateParams( building, pav ) || !validateBankfullPoints( pav, profil ) )
      return;

    // Brückengeometrie ermitteln
    final IProfilePointMarker[] trenner = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    final int markerLeft = trenner.length > 1 ? trenner[0].getPoint().getIndex() : -1;
    final int markerRight = trenner.length > 1 ? trenner[trenner.length - 1].getPoint().getIndex() : -1;

    // Trennflächen vorhanden
    if( markerLeft == -1 || markerRight == -1 )
    {
      // wird schon bei der Trennflächenregel geprüft
      //pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.1" ), 0, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ); //$NON-NLS-1$
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

    final Map<Integer, Double> UKB = pav.getInterpolatedValues( pLUK, pRUK, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    final Map<Integer, Double> OKB = pav.getInterpolatedValues( pLOK, pROK, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );

    // Flußschlauch
    final int iL = pav.whileEqual( pLUK, pRUK, UKB );
    final int innerLeft = iL;// < 0 ? pLUK : iL;
    final int iR = pav.whileEqual( pRUK, pLUK, UKB );
    final int innerRight = iR;// < 0 ? pRUK : iR;
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

    final int outL = pav.whileUpper( pLOK, innerLeft, OKB, true );
    final int outerLeft = outL < 0 ? pLOK : outL;
    final int outR = pav.whileUpper( pROK, innerRight, OKB, true );
    final int outerRight = outR < 0 ? pROK : outR;

    // BrückenOberkante > Brückenunterkante
    // sicher den ersten okB < minOK
    // sicher den ersten ukB > maxUK
    Double minOK = Double.MAX_VALUE;
    Double maxUK = -Double.MAX_VALUE;
    for( int i = outerLeft; i <= outerRight; i++ )
    {
      final Double okB = OKB.get( i );// ProfilUtil.getDoubleValueFor(
      // IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pav.getPoints()[i] );
      final Double ukB = UKB.get( i );// ProfilUtil.getDoubleValueFor(
      // IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, pav.getPoints()[i] );
      if( okB != null && !okB.isNaN() )
      {
        minOK = Math.min( minOK, okB );
      }
      if( ukB != null && !ukB.isNaN() )
      {
        maxUK = Math.max( maxUK, ukB );
      }
      if( ukB != null && okB != null && !okB.isNaN() && !ukB.isNaN() && ukB > okB )
      {
        pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.4" ), i, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
        return;
      }
      // min Oberkante > max Unterkante
      if( maxUK > minOK )
      {
        collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.25" ), String.format( "km %.4f", profil.getStation() ), i, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$ //$NON-NLS-2$
        return;
      }
    }

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
    final int duh = pav.whileUpper( outerLeft, outerRight, UKB, true );
    if( duh > -1 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.23" ), duh, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );//$NON-NLS-1$
      return;
    }

    // Schnittkanten
    final int doh = pav.whileUpper( outerLeft, outerRight < pointsCount - 1 ? outerRight + 1 : outerRight, OKB, true );
    if( doh > -1 )
    {
      pav.createMarker( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.23" ), doh, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );//$NON-NLS-1$
      return;
    }
  }
}