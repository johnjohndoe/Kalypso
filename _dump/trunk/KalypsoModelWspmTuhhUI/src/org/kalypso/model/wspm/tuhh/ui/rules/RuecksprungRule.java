/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;


public class RuecksprungRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

    try
    {
      final List<IProfilPoint> points = profil.getPoints();
      IProfilPoint prevPoint = null;
      for( final IProfilPoint point : points )
      {
        if( prevPoint != null )
        {
          final double x1 = prevPoint.getValueFor( POINT_PROPERTY.BREITE );
          final double x2 = point.getValueFor( POINT_PROPERTY.BREITE );
          final double y1 = prevPoint.getValueFor( POINT_PROPERTY.HOEHE );
          final double y2 = point.getValueFor( POINT_PROPERTY.HOEHE );
          if( x2 - x1 < 0.0 )
            collector.createProfilMarker( true,
                "Gauss-R¸cksprung bei Breite = "
                    + String.format( IProfilConstants.FMT_STATION, x2 ), "",
                profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE
                    .toString(), pluginId, null );
          else if( (x2 - x1 < (Double) POINT_PROPERTY.BREITE
              .getParameter( PARAMETER.PRECISION ))
              && (y1 != y2) )
            collector.createProfilMarker( false,
                "Senkrechte Wand bei Breite = "
                    + String.format( IProfilConstants.FMT_STATION, x2 ), "",
                profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE
                    .toString(), pluginId, null );
        }

        prevPoint = point;
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin
          .getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
