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
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IMarkerResolution2;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class BrueckeRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( (profil == null) || (profil.getBuilding() == null) || (profil.getBuilding().getTyp() != IProfilBuilding.BUILDING_TYP.BRUECKE) )
      return;

    try
    {
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
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    if( devider.length < 2 )
      return;
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

    if( devider[0].getPoint() != points.getFirst()  )
    {
      collector.createProfilMarker( true, "ungültiger durchströmter Bereich", "", 0, POINT_PROPERTY.BREITE.toString(), pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( 0, DEVIDER_TYP.DURCHSTROEMTE, null ) }  );
    }
    if  (devider[devider.length -1].getPoint() != points.getLast())
    {
      collector.createProfilMarker( true, "ungültiger durchströmter Bereich", "", 0, POINT_PROPERTY.BREITE.toString(), pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( devider.length -1, DEVIDER_TYP.DURCHSTROEMTE, null ) }  );
    }
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
    final List<IProfilPoint> points = profil.getPoints();
    for( final IProfilPoint point : points )
    {
      final double h = point.getValueFor( POINT_PROPERTY.HOEHE );
      final double b = point.getValueFor( POINT_PROPERTY.BREITE );
      final double ok = point.getValueFor( POINT_PROPERTY.OBERKANTEBRUECKE );
      final double uk = point.getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE );
      if( (ok < h) || (uk < h) || (ok < uk) )
      {
        collector.createProfilMarker( true, "ungültige Brückengeometrie [" + String.format( IProfilConstants.FMT_STATION, b ) + "]", "", profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE.toString(), pluginId, null );
      }
    }
  }
}
