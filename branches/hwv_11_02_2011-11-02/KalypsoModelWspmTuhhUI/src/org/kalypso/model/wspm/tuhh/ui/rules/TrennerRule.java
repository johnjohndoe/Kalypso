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

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AddDeviderResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Trennflächen und Bordvollpunkte dürfen nur innerhalb der durchströmten Bereiche liegen
 * 
 * @author kimwerner
 */
public class TrennerRule extends AbstractValidatorRule
{
  @Override
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final IProfileObject building = WspmProfileHelper.getBuilding( profil, IProfileBuilding.class );
    final boolean isDurchlass = isDurchlass( building );

    if( !validateSize( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, collector, false ) )
      return;
    // Bei Durchlässen dürfen TF fehlen
    if( !validateSize( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, collector, isDurchlass ) )
      return;
    if( !validateSize( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL, collector, true ) )
      return;

    final IProfilPointMarker db[] = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final IProfilPointMarker tf[] = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker bv[] = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

    validatePosition( db, tf, profil, collector );
    validatePosition( db, bv, profil, collector );
  }

  private boolean validateSize( final IProfil profile, final String markerId, final IValidatorMarkerCollector collector, final boolean allowEmpty ) throws CoreException
  {
    final IProfilPointMarker[] markers = profile.getPointMarkerFor( markerId );

    // REMARK: only used to get the label, so we use the generic component. Do not use this
    // component to do anything with the markers.
    final IComponent component = profile.getPointPropertyFor( markerId );
    final String label = ComponentUtilities.getComponentLabel( component );

    switch( markers.length )
    {
      case 0:
      {
        if( allowEmpty )
          return true;

        final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.TrennerRule.0", label ); //$NON-NLS-1$ 
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, msg, profile, new AddDeviderResolution( markerId ) );
        return false;
      }

      case 1:
      {
        // FIXME: resolution
        final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.TrennerRule.1", label ); //$NON-NLS-1$ 
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, msg, profile );
        return false;
      }

      case 2:
        return true;

        // > 2
      default:
      {
        // FIXME: resolution
        final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.TrennerRule.2", label ); //$NON-NLS-1$ 
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, msg, profile );
        return false;
      }
    }
  }

  private boolean isDurchlass( final IProfileObject building )
  {
    if( !(building instanceof IProfileBuilding) )
      return false;

    if( building instanceof BuildingBruecke )
      return false;

    if( building instanceof BuildingWehr )
      return false;

    return true;
  }

  private void validatePosition( final IProfilPointMarker[] db, final IProfilPointMarker[] toValidate, final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( db == null || db.length != 2 )
      return;
    if( toValidate == null || toValidate.length != 2 )
      return;

    final IRecord leftP = db[0].getPoint();
    final IRecord rightP = db[1].getPoint();

    final Double left = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, leftP );
    final Double right = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, rightP );
    final Double xleft = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, toValidate[0].getPoint() );
    final Double xright = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, toValidate[1].getPoint() );
    if( xright.isNaN() || xleft.isNaN() || left.isNaN() || right.isNaN() )
      return;

    final String type = toValidate[0].getId().getId();
    if( xleft < left || xleft > right )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.TrennerRule.4", toValidate[0].getId().getName() ), String.format( "km %.4f", profil.getStation() ), profil.indexOfPoint( toValidate[0].getPoint() ), null, new MoveDeviderResolution( 0, type, ArrayUtils.indexOf( profil.getPoints(), leftP ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    if( xright < left || xright > right )
    {
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.TrennerRule.6", toValidate[0].getId().getName() ), String.format( "km %.4f", profil.getStation() ), profil.indexOfPoint( toValidate[toValidate.length - 1].getPoint() ), null, new MoveDeviderResolution( toValidate.length - 1, type, ArrayUtils.indexOf( profil.getPoints(), rightP ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
