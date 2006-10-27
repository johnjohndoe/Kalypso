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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;

public class WehrRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( (profil == null) || (profil.getBuilding() == null) || (profil.getBuilding().getTyp() != IProfilBuilding.BUILDING_TYP.WEHR) )
      return;
    try
    {
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
      validateLimits( profil, collector, pluginId );
      validateProfilLines( profil, collector, pluginId );
      validateDevider( profil, collector, pluginId );
      validateParams( profil, collector, pluginId );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {

    final IProfilDevider[] deviders = profil.getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.TRENNFLAECHE, DEVIDER_TYP.WEHR } );
    if( deviders.length < 3 )
      return;
    int index = -1;
    if( deviders[0].getTyp() == DEVIDER_TYP.WEHR )
      index = profil.getPoints().indexOf( deviders[0].getPoint() );
    if( deviders[deviders.length - 1].getTyp() == DEVIDER_TYP.WEHR )
      index = profil.getPoints().indexOf( deviders[deviders.length - 1].getPoint() );
    if( index > -1 )
      collector.createProfilMarker( true, "Wehrfeldtrenner: ungültige Position", "", index, POINT_PROPERTY.OBERKANTEWEHR.toString(), pluginId, null );
  }

  private void validateParams( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {

    final IProfilDevider[] deviders = profil.getDevider( DEVIDER_TYP.WEHR );
    final IProfilBuilding building = profil.getBuilding();
    IProfilPoint point = ((Double) building.getValueFor( BUILDING_PROPERTY.FORMBEIWERT ) == 0.0) ? profil.getPoints().getFirst() : null;
    if( deviders != null )
    {
      for( final IProfilDevider devider : deviders )
      {
        if( (Double) devider.getValueFor( DEVIDER_PROPERTY.BEIWERT ) == 0.0 )
        {
          point = devider.getPoint();
          break;
        }
      }
    }
    if( point != null )
      collector.createProfilMarker( true, "ungültiger Kronenparameter: 0.0", "", profil.getPoints().indexOf( point ), POINT_PROPERTY.OBERKANTEWEHR.toString(), pluginId, null );
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final List<IProfilPoint> points = profil.getPoints();
    for( final IProfilPoint point : points )
    {
      final double h = point.getValueFor( POINT_PROPERTY.HOEHE );
      final double b = point.getValueFor( POINT_PROPERTY.BREITE );
      final double wk = point.getValueFor( POINT_PROPERTY.OBERKANTEWEHR );
      if( wk < h )
      {
        collector.createProfilMarker( true, "ungültige Wehrgeometrie [" + String.format( IProfilConstants.FMT_STATION, b ) + "]", "", profil.getPoints().indexOf( point ), POINT_PROPERTY.OBERKANTEWEHR.toString(), pluginId, null );
      }
      break;
    }
  }

  private void validateLimits( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.TRENNFLAECHE );
    if( devider.length < 2 )
      return;
    final IProfilPoint firstPoint = devider[0].getPoint();
    final IProfilPoint lastPoint = devider[devider.length - 1].getPoint();
    IProfilPoint point = null;
    if( Math.abs( firstPoint.getValueFor( POINT_PROPERTY.HOEHE ) - firstPoint.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) ) > (Double) POINT_PROPERTY.OBERKANTEWEHR.getParameter( PARAMETER.PRECISION ) )
      point = firstPoint;
    if( Math.abs( lastPoint.getValueFor( POINT_PROPERTY.HOEHE ) - lastPoint.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) ) > 0.001 )
      point = lastPoint;
    if( point != null )
      collector.createProfilMarker( true, "ungültige Wehrgeometrie [" + String.format( IProfilConstants.FMT_STATION, point.getValueFor( POINT_PROPERTY.BREITE ) ) + "]", "", profil.getPoints().indexOf( point ), POINT_PROPERTY.OBERKANTEWEHR.toString(), pluginId, null );
  }
}
