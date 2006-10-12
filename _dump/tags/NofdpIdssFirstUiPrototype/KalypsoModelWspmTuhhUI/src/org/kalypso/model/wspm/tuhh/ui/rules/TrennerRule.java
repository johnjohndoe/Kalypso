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
import org.eclipse.ui.IMarkerResolution;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.ui.profil.validation.MessageMarkerResolution;

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

    final IProfilDevider db[] = profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    final IProfilDevider tf[] = profil.getDevider( DEVIDER_TYP.TRENNFLAECHE );
    final IProfilDevider bv[] = profil.getDevider( DEVIDER_TYP.BORDVOLL );

    if( db == null )
      collector.createProfilMarker( true, "keine Durchströmten Bereiche vorhanden", "", 0, "", null );
    if( tf != null )
      validatePosition( db, tf, profil, collector );
    else
      collector.createProfilMarker( true, "keine Trennflächen vorhanden", "", 0, "", new IMarkerResolution[] { new MessageMarkerResolution() } );
    if( bv != null )
      validatePosition( db, bv, profil, collector );

  }

  private void validatePosition( IProfilDevider[] db, IProfilDevider[] toValidate, final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if(db==null || db.length < 2 || toValidate==null || toValidate.length < 2)
      return;
    try
    {
      final double left = db[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double right = db[1].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double xleft = toValidate[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double xright = toValidate[1].getPoint().getValueFor( POINT_PROPERTY.BREITE );

      if( (xleft < left) || (xleft > right) )
      {
        collector.createProfilMarker( true, toValidate[0].getTyp().toString() + " [" + String.format( IProfilConstants.FMT_STATION, xleft ) + "] liegt außerhalb des Durchströmten Bereichs", "", profil.getPoints().indexOf( toValidate[0].getPoint() ), "", null );
      }
      if( (xright < left) || (xright > right) )
      {
        collector.createProfilMarker( true, toValidate[1].getTyp().toString() + " [" + String.format( IProfilConstants.FMT_STATION, xright ) + "] liegt außerhalb des Durchströmten Bereichs", "", profil.getPoints().indexOf( toValidate[1].getPoint() ), "", null );
      }
      if( toValidate[0].getPoint() == toValidate[1].getPoint() )
      {
        collector.createProfilMarker( true, "doppelte " + toValidate[1].getTyp().toString(), "", profil.getPoints().indexOf( toValidate[1].getPoint() ), "", null );
      }
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
