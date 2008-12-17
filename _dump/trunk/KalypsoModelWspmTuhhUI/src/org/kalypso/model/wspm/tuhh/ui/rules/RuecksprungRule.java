/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
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

    final IRecord[] points = profil.getPoints();
    final IComponent cB = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent cH = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    if( cB == null || cH == null || points.length < 1 )
      return;
    final double deltaX = cB.getPrecision();
    final double deltaY = cH.getPrecision();

    for( int i = 1; i < points.length; i++ )
    {
      final Double x1 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i - 1] );
      final Double x2 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[i] );
      final Double y1 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i - 1] );
      final Double y2 = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, points[i] );
      if( x1.isNaN() || x2.isNaN() || y1.isNaN() || y2.isNaN() )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getFormatString("org.kalypso.model.wspm.tuhh.ui.rules.RuecksprungRule.0", cB.getName()), "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
      else if(  x1 - x2  > deltaX )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getFormatString("org.kalypso.model.wspm.tuhh.ui.rules.RuecksprungRule.2",  x2 ), "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
      else if( Math.abs( x2 - x1 ) < deltaX && Math.abs( (Double) y2 - (Double) y1 ) > deltaY )
        collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getFormatString("org.kalypso.model.wspm.tuhh.ui.rules.RuecksprungRule.4",  x2 ), "km " + Double.toString( profil.getStation() ), i, IWspmConstants.POINT_PROPERTY_BREITE, pluginId ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
