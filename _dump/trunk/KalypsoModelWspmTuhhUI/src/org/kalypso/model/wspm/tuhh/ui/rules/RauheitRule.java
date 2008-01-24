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
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

public class RauheitRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;
    IComponent pointProp = null;
    if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ) ) )
      pointProp = ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS );
    if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST ) ) )
      pointProp = ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( pointProp == null )
      return;
    final List<IRecord> points = ProfilUtil.getInnerPoints( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    if( points == null )
      return;
    int i = 0;
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
    for( final IRecord point : points )
    {
      try
      {
        final double rh = (Double) point.getValue( pointProp );
        if( rh <= 0.0 )
        {
          collector.createProfilMarker( true, "unzul‰ssiger Rauheitswert [" + rh + "]", "", i, pointProp.getId(), pluginId, null );
          break;
        }
        i++;
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
      }
    }
  }
}
