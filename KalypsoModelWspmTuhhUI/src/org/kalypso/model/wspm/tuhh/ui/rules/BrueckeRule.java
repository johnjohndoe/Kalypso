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

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelDeviderResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 *
 * @author kimwerner
 */
public class BrueckeRule extends AbstractValidatorRule
{

  private boolean validateParams( final IProfileObject building, final IValidatorMarkerCollector collector, final String pluginId, final IProfil profil ) throws CoreException
  {
    for( final IComponent property : building.getObjectProperties() )
    {
      final Object oValue = building.getValue( property );
      if( oValue == null || (oValue instanceof Double && ((Double) oValue).isNaN()) )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.0" ) + property.getName() + "> fehlt", String.format( "km %.4f", profil.getStation() ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        return false;
      }
    }
    return true;
  }

  private boolean validateBankfullPoints( final IValidatorMarkerCollector collector, final String pluginId, final IProfil profil ) throws CoreException
  {
    final IProfilPointMarker[] brdvp = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    if( brdvp.length > 0 )
    {
      final IProfilMarkerResolution[] delRes = new IProfilMarkerResolution[] { new DelDeviderResolution( -1, IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) };
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.19" ), String.format( "km %.4f", profil.getStation() ), profil.indexOfPoint( brdvp[0].getPoint() ), IWspmConstants.POINT_PROPERTY_BREITE, pluginId, delRes ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    return true;
  }

  private final int getBridgeLimits( final IRecord[] points, final double delta, final int start, final int end, final int step )
  {
    int i = start < 0 ? end : start;
    while( i != end )
    {
      final Double d = compare( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );

      if( d.isNaN() )
        return -1;
      if( Math.abs( d ) < delta )
        return i;
      i = i + step;
    }
    return -1;
  }

  private final Double compare( final String compId1, final String compId2, final IRecord point )
  {
    final Double h1 = ProfilUtil.getDoubleValueFor( compId1, point );
    final Double h2 = ProfilUtil.getDoubleValueFor( compId2, point );
    if( h1.isNaN() || h2.isNaN() )
      return Double.NaN;
    else
      return h1 - h2;
  }

  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {

    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    if( profil == null || building == null || !IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( building.getId() ) )
      return;

    try
    {
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

      // validierung ohne Brückengeometrie möglich
      if( !validateParams( building, collector, pluginId, profil ) || !validateBankfullPoints( collector, pluginId, profil ) )
        return;

      // Brückengeometrie ermitteln

      final IRecord[] points = profil.getPoints();
      final IComponent cBreite = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      final double delta = cBreite == null ? 0.0001 : cBreite.getPrecision();

      final IProfilPointMarker[] trenner = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
      final int innerLeft = trenner.length > 1 ? profil.indexOfPoint( trenner[0].getPoint() ) : -1;
      final int innerRight = trenner.length > 1 ? profil.indexOfPoint( trenner[trenner.length - 1].getPoint() ) : -1;
      //Schnittp. OK-Brücke Boden
      final int outerLeft = getBridgeLimits( points, delta, innerLeft, -1, -1 );
      final int outerRight = getBridgeLimits( points, delta, innerRight, points.length, 1 );

      if( innerLeft == -1 || innerRight == -1 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Trennflächen fehlen", String.format( "km %.4f", profil.getStation() ), 0, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
        return;
      }
      if( outerLeft == -1 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brücke schließt nicht mit Geländehöhe ab", String.format( "km %.4f", profil.getStation() ), 0, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
        return;
      }
      if( outerRight == -1 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brücke schließt nicht mit Geländehöhe ab", String.format( "km %.4f", profil.getStation() ), points.length - 1, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
        return;
      }
      // Wiederlager
      if( innerRight == outerRight )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.5" ), String.format( "km %.4f", profil.getStation() ), outerRight, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      else if( innerLeft == outerLeft )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.7" ), String.format( "km %.4f", profil.getStation() ), outerLeft, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      // Trennflächen
      else if( Math.abs( compare( IWspmConstants.POINT_PROPERTY_HOEHE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[innerLeft] ) ) > delta )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.9" ), String.format( "km %.4f", profil.getStation() ), innerLeft, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      else if( Math.abs( compare( IWspmConstants.POINT_PROPERTY_HOEHE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[innerRight] ) ) > delta )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.11" ), String.format( "km %.4f", profil.getStation() ), innerRight, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      else
      {
        // Brückemkanten und Bewuchs
        // sicher den ersten okB < minOK
        // sicher den ersten ukB > maxUK
        Double minOK = Double.MAX_VALUE;
        Double maxUK = -Double.MAX_VALUE;
        for( int i = innerLeft; i < innerRight; i++ )
        {
 //         final Double deltaH = compare( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
          final Double deltaB = compare( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[i] );
          minOK = Math.min( minOK, ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, points[i] ).doubleValue() );
          maxUK = Math.max( maxUK, ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[i] ).doubleValue() );
          // Schnittkanten
          if( deltaB < delta )
          {
            collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.21" ), String.format( "km %.4f", profil.getStation() ), i - 1, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
            break;
          }
          // min Oberkante > max Unterkante
          if( maxUK > minOK )
          {
            collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.25" ), String.format( "km %.4f", profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
            break;
          }
          // Mehrfeldbrücke (Brücke mit mehreren Öffnungen erlaubt
//          if( deltaH < delta && i > innerLeft )
//          {
//            collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Die Brückenunterkante berührt den Boden innerhalb des Flußschlauchs", String.format( "km %.4f", profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
//            break;
//          }
        }

      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BrueckeRule.29" ), e ) ); //$NON-NLS-1$
    }

  }
}
