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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IMarkerResolution2;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AddDeviderResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Trennflächen und Bordvollpunkte dürfen nur innerhalb der durchströmten Bereiche liegen
 * 
 * @author belger
 */
public class TrennerRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final IProfilPointMarker db[] = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    final IProfilPointMarker tf[] = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    final IProfilPointMarker bv[] = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );

    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

    if( db.length == 0 )
      collector.createProfilMarker( true, "keine durchströmten Bereiche vorhanden", "", 0, null, pluginId, new IMarkerResolution2[] { new AddDeviderResolution( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) } );

    // TODO IProfileObjects now returned as list from IProfile
    final IProfileObject[] profileObjects = profil.getProfileObject();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];

    if( tf.length == 0 && !(building != null && building instanceof IProfileObject) )
      collector.createProfilMarker( true, "keine Trennflächen vorhanden", "", 0, null, pluginId, new IMarkerResolution2[] { new AddDeviderResolution( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) } );
    validatePosition( db, tf, profil, collector );
    validatePosition( db, bv, profil, collector );
  }

  private void validatePosition( final IProfilPointMarker[] db, final IProfilPointMarker[] toValidate, final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( db.length < 2 || toValidate == null || toValidate.length < 2 )
      return;
    try
    {
      final IRecord leftP = db[0].getPoint();
      final double left = (Double) leftP.getValue( ProfilObsHelper.getPropertyFromId( leftP, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final IRecord rightP = db[db.length - 1].getPoint();
      final double right = (Double) rightP.getValue( ProfilObsHelper.getPropertyFromId( rightP, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final double xleft = (Double) toValidate[0].getPoint().getValue( ProfilObsHelper.getPropertyFromId( toValidate[0].getPoint(), IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final double xright = (Double) toValidate[toValidate.length - 1].getPoint().getValue( ProfilObsHelper.getPropertyFromId( toValidate[toValidate.length - 1].getPoint(), IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
      final IComponent deviderTyp = toValidate[0].getId();

      if( (xleft < left) || (xleft > right) )
      {
        collector.createProfilMarker( true, toValidate[0].getId().getName() + ": außerhalb des durchströmten Bereichs", "", profil.getPoints().indexOf( toValidate[0].getPoint() ), null, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( 0, deviderTyp, profil.getPoints().indexOf( leftP ) ) } );
      }
      if( (xright < left) || (xright > right) )
      {
        collector.createProfilMarker( true, toValidate[0].getId().getName() + ": außerhalb des durchströmten Bereichs", "", profil.getPoints().indexOf( toValidate[toValidate.length - 1].getPoint() ), null, pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( toValidate.length - 1, deviderTyp, profil.getPoints().indexOf( rightP ) ) } );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
