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
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Brï¿½ckenkanten dï¿½rfen nicht unterhalb des Gelï¿½ndeniveaus liegen Oberkante darf nicht unter Unterkante
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
        if( ((Double) building.getValue( property )).isNaN() )
        {
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Parameter <" + property.getName() + "> fehlt", "", 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
          break;
        }
      }
      final IRecord[] points = profil.getPoints();

      final double delta = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ).getPrecision();
      final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
      final int iOK = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
      final int iUK = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
      final int iAX = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );

      // Brückengeometrie
      // ermitteln der linken Grenzen
      int outerLeft = 0;
      int innerLeft = 0;
      for( int i = 1; i < points.length; i++ )
      {
        final Double h = (Double) points[i].getValue( iHoehe );
        final Double okB = (Double) points[i].getValue( iOK );
        final Double ukB = (Double) points[i].getValue( iUK );
        if( (outerLeft == 0) && (Math.abs( h - okB ) > delta) )
          outerLeft = i - 1;
        if( (outerLeft > 0) && (Math.abs( h - ukB ) > delta) )
        {
          innerLeft = i - 1;
          break;
        }
      }
      if( innerLeft == 0 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückengeometrie unvollständig", "", 0, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
        return;
      }
      if( innerLeft == outerLeft )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten treffen in einem Punkt auf die Geländeoberfläche", "", outerLeft, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
      }

      // ermitteln der rechten Grenzen
      int outerRight = points.length - 1;
      int innerRight = points.length - 1;
      for( int i = points.length - 2; i > innerLeft; i-- )
      {
        final Double h = (Double) points[i].getValue( iHoehe );
        final Double okB = (Double) points[i].getValue( iOK );
        final Double ukB = (Double) points[i].getValue( iUK );
        if( (outerRight == points.length - 1) && (Math.abs( h - okB ) > delta) )
          outerRight = i + 1;
        if( (outerRight < points.length - 1) && (Math.abs( h - ukB ) > delta) )
        {
          innerRight = i +1;
          break;
        }
      }
      if( innerRight == outerRight )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten treffen in einem Punkt auf die Geländeoberfläche", "", outerRight, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
      }

      // Trennflächen
      final IProfilPointMarker[] trenner = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
      if( trenner.length > 1 )
      {
        if( profil.indexOfPoint( trenner[0].getPoint() ) != innerLeft )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Trennfläche nicht auf Schnittpunkt Gelände-UK-Brücke", "", innerLeft, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );// new
        if( profil.indexOfPoint( trenner[trenner.length - 1].getPoint() ) != innerRight )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Trennfläche nicht auf Schnittpunkt Gelände-UK-Brücke", "", innerRight, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );// new
      }
      // Bordvollpunkte
      final IProfilPointMarker[] brdvp = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
      if( brdvp.length > 0 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Bordvollpunkte sind zu entfernen", "", profil.indexOfPoint( brdvp[0].getPoint() ), IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );// new
      }
      Double minOK = Double.MAX_VALUE;
      Double maxUK = Double.MIN_VALUE;
      int minmax = 0;
      for( int i = outerLeft; i < outerRight; i++ )
      {
        final Double h = (Double) points[i].getValue( iHoehe );
        final Double uk = (Double) points[i].getValue( iUK );
        final Double ok = (Double) points[i].getValue( iOK );
        if( (minmax == 0) && (i > innerLeft) && (i < innerRight) )
        {
          minOK = Math.min( minOK, ok );
          maxUK = Math.max( maxUK, uk );
          if( maxUK > minOK )
            minmax = i;
        }
        // Schnittkanten
        if( uk - ok > delta )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten schneiden sich", "", i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
        if( h - ok > delta || h - uk > delta )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Brückenkanten unter Geländehöhe", "", i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );

        // Bewuchs unter der Brücke
        if( iAX < 0 )
          continue;
        final Double bewuchs = (Double) points[i].getValue( iAX );
        if( bewuchs != 0.0 )
          collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Bewuchsparameter im Brückenbereich", "", i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
      }
      if( minmax > 0 )
      {
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, "kleinste Höhe der Oberkante liegt unter größter Höhe der Unterkante", "", minmax, IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
