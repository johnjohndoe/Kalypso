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
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelDeviderResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author kimwerner
 */
public class BrueckeRule extends AbstractValidatorRule
{
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

      for( final IComponent property : building.getObjectProperties() )
      {
        final Object oValue = building.getValue( property );
        if( oValue == null || (oValue instanceof Double && ((Double) oValue).isNaN()) )
        {
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Parameter <" + property.getName() + "> fehlt", "km " + Double.toString( profil.getStation() ), 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId );
          break;
        }
      }
      final IRecord[] points = profil.getPoints();
      final IComponent cBreite = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      final double delta = cBreite == null ? 0.0001 : cBreite.getPrecision();

      // Brückengeometrie
      // ermitteln der linken Grenzen
      int outerLeft = -1;
      int innerLeft = -1;
      for( int i = 0; i < points.length; i++ )
      {
        final Double h = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
        final Double okB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, points[i] );
        final Double ukB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[i] );
        if( okB.isNaN() || ukB.isNaN() )
          continue;
        if( (outerLeft == -1) && (Math.abs( h - okB ) > delta) )
          outerLeft = i == 0 ? 0 : i - 1;
        if( (outerLeft > -1) && (Math.abs( h - ukB ) > delta) )
        {
          innerLeft = i == 0 ? 0 : i - 1;
          break;
        }
      }
      if( innerLeft == -1 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückengeometrie unvollständig", "km " + Double.toString( profil.getStation() ), 0, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );
        return;
      }
      if( innerLeft == outerLeft )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten treffen in einem Punkt auf die Geländeoberfläche", "km " + Double.toString( profil.getStation() ), outerLeft, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );
      }

      // ermitteln der rechten Grenzen
      int outerRight = points.length - 1;
      int innerRight = points.length - 1;
      for( int i = points.length - 1; i > innerLeft; i-- )
      {
        final Double h = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
        final Double okB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, points[i] );
        final Double ukB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[i] );
        if( okB.isNaN() || ukB.isNaN() )
          continue;
        if( (outerRight == points.length - 1) && (Math.abs( h - okB ) > delta) )
          outerRight = i == points.length - 1 ? points.length - 1 : i + 1;
        if( (outerRight < points.length - 1) && (Math.abs( h - ukB ) > delta) )
        {
          innerRight = i == points.length - 1 ? points.length - 1 : i + 1;
          break;
        }
      }
      if( innerRight == outerRight )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten treffen in einem Punkt auf die Geländeoberfläche", "km " + Double.toString( profil.getStation() ), outerRight, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );
      }

      // Trennflächen
      final IProfilPointMarker[] trenner = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
      if( trenner.length > 1 )
      {
        final int left = profil.indexOfPoint( trenner[0].getPoint() );
        final Double h_0 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[left] );
        final Double ukB_0 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[left] );
        final Double h_1 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[left + 1] );
        final Double ukB_1 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[left + 1] );
        if( !h_0.isNaN() && !ukB_0.isNaN() && Math.abs( h_0 - ukB_0 ) > delta )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Trennfläche nicht auf Schnittpunkt Gelände-UK-Brücke", "km " + Double.toString( profil.getStation() ), left, IWspmConstants.POINT_PROPERTY_BREITE, pluginId , new MoveDeviderResolution(0, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE,innerLeft));
        if( !h_1.isNaN() && !ukB_1.isNaN() && ukB_1 - h_1 < delta )
          collector.createProfilMarker( IMarker.SEVERITY_WARNING, "Trennfläche innerhalb der Brückengeometrie", "km " + Double.toString( profil.getStation() ), left, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, new MoveDeviderResolution(0, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE,innerLeft));

        final int right = profil.indexOfPoint( trenner[trenner.length - 1].getPoint() );
        final Double h_2 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[right] );
        final Double ukB_2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[right] );
        final Double h_3 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[right - 1] );
        final Double ukB_3 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[right - 1] );
        if( !h_2.isNaN() && !ukB_2.isNaN() && Math.abs( h_2 - ukB_2 ) > delta )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Trennfläche nicht auf Schnittpunkt Gelände-UK-Brücke", "km " + Double.toString( profil.getStation() ), right, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ,new MoveDeviderResolution(trenner.length - 1, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE,innerRight));
        if( !h_3.isNaN() && !ukB_3.isNaN() && ukB_3 - h_3 < delta )
          collector.createProfilMarker( IMarker.SEVERITY_WARNING, "Trennfläche innerhalb der Brückengeometrie", "km " + Double.toString( profil.getStation() ), right, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ,new MoveDeviderResolution(trenner.length - 1, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE,innerRight));
      }

      // mehr als eine Öffnung

      for( int i = innerLeft + 1; i < innerRight; i++ )
      {
        final Double h = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
        final Double ukB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[i] );
        if( !h.isNaN() && !ukB.isNaN() && (Math.abs( h - ukB ) < delta) )
        {
          collector.createProfilMarker( IMarker.SEVERITY_INFO, "Mehrfeldbrücke oder Öffnung außerhalb des Flussschlauchs", "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );
          break;
        }
      }

      // Bordvollpunkte
      final IProfilPointMarker[] brdvp = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
      if( brdvp.length > 0 )
      {
        final IProfilMarkerResolution[] delRes = new IProfilMarkerResolution[brdvp.length];
        // the last devider must be deleted first
        for( int i = 0; i < brdvp.length; i++ )
        {
          delRes[i] = new DelDeviderResolution( brdvp.length - 1 - i, IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
        }

        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Bordvollpunkte sind zu entfernen", "km " + Double.toString( profil.getStation() ), profil.indexOfPoint( brdvp[0].getPoint() ), IWspmConstants.POINT_PROPERTY_BREITE, pluginId, delRes );
      }
      Double minOK = Double.MAX_VALUE;
      Double maxUK = Double.MIN_VALUE;
      int minmax = 0;
      for( int i = outerLeft; i < outerRight; i++ )
      {
        final Double h = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
        final Double okB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, points[i] );
        final Double ukB = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, points[i] );
        if( (minmax == 0) && (i > innerLeft) && (i < innerRight) )
        {
          minOK = Math.min( minOK, okB );
          maxUK = Math.max( maxUK, ukB );
          if( maxUK > minOK )
            minmax = i;
        }
        // Schnittkanten
        if( ukB - okB > delta )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten schneiden sich", "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );
        if( h - okB > delta || h - ukB > delta )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten unter Geländehöhe", "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );

        // Bewuchs unter der Brücke
        if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) == null )
          continue;
        final Double bewuchs = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, points[i] );
        if( bewuchs != 0.0 )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Bewuchsparameter im Brückenbereich", "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, new DelBewuchsResolution() );
      }
      if( minmax > 0 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "kleinste Höhe der Oberkante liegt unter größter Höhe der Unterkante", "km " + Double.toString( profil.getStation() ), minmax, IWspmConstants.POINT_PROPERTY_BREITE, pluginId );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
