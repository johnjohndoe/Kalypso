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
package org.kalypso.model.wspm.tuhh.ui.rules;

import java.util.LinkedList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IMarkerResolution2;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class BrueckeRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    // TODO IProfileObjects now returned as list from IProfile
    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    if( (profil == null) || (building == null) || (!IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( building.getId() )) )
      return;

    try
    {
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
      final LinkedList<IRecord> points = profil.getPoints();
      final double delta = ProfilObsHelper.getPrecision( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );
      IRecord ok_h_l = null;
      IRecord uk_h_l = null;
      IRecord uk_h_r = null;
      IRecord ok_h_r = null;
      IRecord lastPoint = null;

      // Brückengeometrie
      for( final IRecord point : points )
      {
        final double h = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );
        final double uk = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ) );
        final double ok = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) );
        final Double bewuchs = point.getOwner().hasComponent( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) ) ? (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) )
            : 0.0;

        if( uk - ok > delta )
          collector.createProfilMarker( true, "Brückenkanten schneiden sich", "", profil.getPoints().indexOf( point ), IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, null );
        if( (ok_h_l == null) && (ok - h > delta) )
          ok_h_l = lastPoint;
        if( ok_h_l != null && uk_h_l == null && (uk - h > delta) )
          uk_h_l = lastPoint;
        if( ok_h_l != null && uk_h_l != null && uk_h_r == null && (Math.abs( uk - h ) < delta) )
          uk_h_r = point;
        if( ok_h_l != null && uk_h_l != null && uk_h_r != null && ok_h_r == null && (Math.abs( ok - h ) < delta) )
          ok_h_r = point;
        if( (h - ok > delta) || (h - uk > delta) )
          collector.createProfilMarker( true, "Brückenkanten unter Geländehöhe", "", profil.getPoints().indexOf( point ), IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, null );
        // Bewuchs únter der Brücke
        if( ok_h_l != null && ok_h_r == null && bewuchs != 0.0 )
          collector.createProfilMarker( true, "Bewuchsparameter im Brückenbereich", "", profil.getPoints().indexOf( point ), IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, null );

        lastPoint = point;
      }

      for( final IComponent property : building.getObjectProperties() )
      {
        if( ((Double) building.getValue( property )).isNaN() )
        {
          collector.createProfilMarker( true, "Parameter <" + property.getName() + "> fehlt", "", 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
          break;
        }
      }
      if( ok_h_l == null || uk_h_l == null || ok_h_r == null || uk_h_r == null )
      {
        collector.createProfilMarker( true, "Brücke unvollständig", "", 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
      }
      else
      {
        // Durchströmte Bereiche
        final IProfilPointMarker[] marker = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
        if( marker.length > 1 )
        {
          if( marker[0].getPoint() != points.getFirst() )
          {
            collector.createProfilMarker( true, "Der durchströmte Bereich muß das gesamte Profil einschließen", "", 0, IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( 0, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ), 0 ) } );
          }
          if( marker[marker.length - 1].getPoint() != points.getLast() )
          {
            collector.createProfilMarker( true, "Der durchströmte Bereich muß das gesamte Profil einschließen", "", points.size() - 1, IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( marker.length - 1, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ), points.size() - 1 ) } );
          }
        }

        // Trennflächen
        final IProfilPointMarker[] trenner = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
        if( trenner.length > 1 )
        {
          if( trenner[0].getPoint() != uk_h_l )

            collector.createProfilMarker( true, "Trennfläche nicht auf Schnittpunkt Gelände-UK-Brücke", "", profil.getPoints().indexOf( trenner[0].getPoint() ), IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( 0, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ), points.indexOf( uk_h_l ) ) } );
          if( trenner[trenner.length - 1].getPoint() != uk_h_r )

            collector.createProfilMarker( true, "Trennfläche nicht auf Schnittpunkt Gelände-UK-Brücke", "", profil.getPoints().indexOf( trenner[trenner.length - 1].getPoint() ), IWspmTuhhConstants.POINT_PROPERTY_BREITE, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( trenner.length - 1, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ), points.indexOf( uk_h_r ) ) } );
        }

        // Brückenlager
        if( points.indexOf( ok_h_l ) >= points.indexOf( uk_h_l ) )
        {
          collector.createProfilMarker( true, "Schnittpunkt Oberkante-Gelände falsch", "", points.indexOf( uk_h_l ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
        }
        if( points.indexOf( ok_h_r ) <= points.indexOf( uk_h_r ) )
        {
          collector.createProfilMarker( true, "Schnittpunkt Oberkante-Gelände falsch", "", points.indexOf( uk_h_r ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }

  final boolean paramCheck( final IProfileObject profileObject )
  {
    for( final IComponent property : profileObject.getObjectProperties() )
    {
      if( ((Double) profileObject.getValue( property )).isNaN() )
        return false;
    }
    return true;
  }
}
