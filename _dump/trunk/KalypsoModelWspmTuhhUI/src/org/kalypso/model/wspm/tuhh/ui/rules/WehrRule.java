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

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;


/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class WehrRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( (profil == null) || (profil.getBuilding() == null) || (profil.getBuilding().getTyp() != IProfilBuilding.BUILDING_TYP.WEHR) )
      return;

    try
    {
      validateLimits( profil, collector );
      validateProfilLines( profil, collector );
      validateDevider( profil, collector );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final IProfilDevider[] deviders = profil.getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.TRENNFLAECHE, DEVIDER_TYP.WEHR } );
    if (deviders.length < 3) return;
    if( deviders[0].getTyp()==DEVIDER_TYP.WEHR  )
    {
      final IProfilPoint point =  deviders[0].getPoint();
      collector.createProfilMarker( true, "Wehrfeldtrenner [" + String.format( IProfilConstants.FMT_STATION, point.getValueFor(POINT_PROPERTY.BREITE ) ) + "]außerhalb der Trennflächen", "", profil.getPoints().indexOf(point), POINT_PROPERTY.BREITE.toString(), null ); 
    }
    if( deviders[deviders.length -1].getTyp()==DEVIDER_TYP.WEHR  )
    {
      final IProfilPoint point =  deviders[deviders.length -1].getPoint();
      collector.createProfilMarker( true, "Wehrfeldtrenner [" + String.format( IProfilConstants.FMT_STATION, point.getValueFor(POINT_PROPERTY.BREITE ) ) + "]außerhalb der Trennflächen", "", profil.getPoints().indexOf(point), POINT_PROPERTY.BREITE.toString(), null ); 
    }
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final List<IProfilPoint> points = profil.getPoints();
    for( final IProfilPoint point : points )
    {

      final double h = point.getValueFor( POINT_PROPERTY.HOEHE );
      final double b = point.getValueFor( POINT_PROPERTY.BREITE );
      final double wk = point.getValueFor( POINT_PROPERTY.OBERKANTEWEHR );

      if( wk < h )
      {
        collector.createProfilMarker( true, "Oberkante Wehr [" + String.format( IProfilConstants.FMT_STATION, b ) + "]unter Geländehöhe", "", profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE.toString(), null );
      }

    }
  }

  private void validateLimits( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final List<IProfilPoint> points = ProfilUtil.getInnerPoints( profil, DEVIDER_TYP.TRENNFLAECHE );

    if( points.size() < 2 )
      return;
    final IProfilPoint firstPoint = points.get( 0 );
    final IProfilPoint lastPoint = points.get( points.size() - 1 );

    if( Math.abs( firstPoint.getValueFor( POINT_PROPERTY.HOEHE ) - firstPoint.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) ) > 0.001 )
    {
      collector.createProfilMarker( true, "Der erste Punkt[" + String.format( IProfilConstants.FMT_STATION, firstPoint.getValueFor( POINT_PROPERTY.BREITE ) )
          + "]der OK-Wehr muss auf Geländehöhe liegen", "", profil.getPoints().indexOf( firstPoint ), POINT_PROPERTY.BREITE.toString(), null );
    }
    if( Math.abs( lastPoint.getValueFor( POINT_PROPERTY.HOEHE ) - lastPoint.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) ) > 0.001 )
    {
      collector.createProfilMarker( true, "Der letzte Punkt[" + String.format( IProfilConstants.FMT_STATION, lastPoint.getValueFor( POINT_PROPERTY.BREITE ) )
          + "]der OK-Wehr muss auf Geländehöhe liegen", "", profil.getPoints().indexOf( lastPoint ), POINT_PROPERTY.BREITE.toString(), null );
    }
  }
}
