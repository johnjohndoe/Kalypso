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
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class RuecksprungRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

    try
    {
      final IRecord[] points = profil.getPoints();
      final IComponent cB = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      final IComponent cH = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
      if( cB == null || cH == null )
        return;
      final int iB = profil.indexOfProperty( cB );
      final int iH = profil.indexOfProperty( cH );
      IRecord prevPoint = null;
      for( final IRecord point : points )
      {
        if( prevPoint != null )
        {
          final Object x1 = prevPoint.getValue( iB );
          final Object x2 = point.getValue( iB );
          final Object y1 = prevPoint.getValue( iH );
          final Object y2 = point.getValue( iH );
          if( x1 == null || x2 == null || y1 == null || y2 == null )
            continue;

          final double deltaX = cB.getPrecision();
          final double deltaY = cH.getPrecision();
          if( (Double) x1 - (Double) x2 > deltaX )
            collector.createProfilMarker( IMarker.SEVERITY_ERROR, "Gauss-Rücksprung bei Breite = " + String.format( FMT_BREITE, (Double) x2 ), "", profil.indexOfPoint( point ), IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
          else if( Math.abs( (Double) x2 - (Double) x1 ) < deltaX && Math.abs( (Double) y2 - (Double) y1 ) > deltaY )
            collector.createProfilMarker( IMarker.SEVERITY_WARNING, "Senkrechte Wand bei Breite = " + String.format( FMT_BREITE, (Double) x2 ), "", profil.indexOfPoint( point ), IWspmConstants.POINT_PROPERTY_BREITE, pluginId, null );
        }

        prevPoint = point;
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }
}
