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

import org.apache.commons.lang.ArrayUtils;
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
 * Trennflï¿½chen und Bordvollpunkte dï¿½rfen nur innerhalb der durchstrï¿½mten Bereiche liegen
 * 
 * @author kimwerner
 */
public class TrennerRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final IComponent cTrennF = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IComponent cDurchS = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final IComponent cBordV = profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );

    final IProfilPointMarker db[] = profil.getPointMarkerFor( cDurchS );
    final IProfilPointMarker tf[] = profil.getPointMarkerFor( cTrennF );
    final IProfilPointMarker bv[] = profil.getPointMarkerFor( cBordV );

    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

    if( db.length == 0 )
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, "keine durchströmten Bereiche vorhanden", "", 0, null, pluginId, null );// new
                                                                                                                                  // IMarkerResolution2[]
                                                                                                                                  // {
                                                                                                                                  // new
                                                                                                                                  // AddDeviderResolution(
                                                                                                                                  // IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE
                                                                                                                                  // ) }
                                                                                                                                  // );

    final IProfileObject[] profileObjects = profil.getProfileObjects();
    IProfileObject building = null;
    if( profileObjects.length > 0 )
      building = profileObjects[0];
    // Regel für fehlende Trennflächen bei Durchlässen erlauben
    // TUHH-Hack
    if( tf.length == 0 && !isDurchlass( building ) )
      collector.createProfilMarker( IMarker.SEVERITY_ERROR, "keine Trennflächen vorhanden", "", 0, null, pluginId, null );
    validatePosition( db, tf, profil, collector );
    validatePosition( db, bv, profil, collector );
  }

  private boolean isDurchlass( final IProfileObject building )
  {
    return !(building == null || building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) || building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_WEHR ));
  }

  private void validatePosition( final IProfilPointMarker[] db, final IProfilPointMarker[] toValidate, final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( db.length < 2 || toValidate == null || toValidate.length < 2 )
      return;
    final int iB = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    try
    {
      final IRecord leftP = db[0].getPoint();
      final double left = (Double) leftP.getValue( iB );
      final IRecord rightP = db[db.length - 1].getPoint();
      final double right = (Double) rightP.getValue( iB );
      final double xleft = (Double) toValidate[0].getPoint().getValue( iB );
      final double xright = (Double) toValidate[toValidate.length - 1].getPoint().getValue( iB );
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

      if( xleft < left || xleft > right )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, toValidate[0].getId().getName() + ": außerhalb des durchströmten Bereichs", "", ArrayUtils.indexOf( profil.getPoints(), toValidate[0].getPoint() ), null, pluginId, null );// new
                                                                                                                                                                                                                      // IMarkerResolution2[]
                                                                                                                                                                                                                      // {
                                                                                                                                                                                                                      // new
                                                                                                                                                                                                                      // MoveDeviderResolution(
                                                                                                                                                                                                                      // 0,
                                                                                                                                                                                                                      // deviderTyp,
                                                                                                                                                                                                                      // ArrayUtils.indexOf(
                                                                                                                                                                                                                      // profil.getPoints(),
                                                                                                                                                                                                                      // leftP
                                                                                                                                                                                                                      // ) )
                                                                                                                                                                                                                      // } );
      if( xright < left || xright > right )
        collector.createProfilMarker( IMarker.SEVERITY_ERROR, toValidate[0].getId().getName() + ": außerhalb des durchströmten Bereichs", "", ArrayUtils.indexOf( profil.getPoints(), toValidate[toValidate.length - 1].getPoint() ), null, pluginId, null );// new
                                                                                                                                                                                                                                          // IMarkerResolution2[]
                                                                                                                                                                                                                                          // {
                                                                                                                                                                                                                                          // new
                                                                                                                                                                                                                                          // MoveDeviderResolution(
                                                                                                                                                                                                                                          // toValidate.length
                                                                                                                                                                                                                                          // - 1,
                                                                                                                                                                                                                                          // deviderTyp,
                                                                                                                                                                                                                                          // ArrayUtils.indexOf(
                                                                                                                                                                                                                                          // profil.getPoints(),
                                                                                                                                                                                                                                          // rightP
                                                                                                                                                                                                                                          // ) )
                                                                                                                                                                                                                                          // } );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
