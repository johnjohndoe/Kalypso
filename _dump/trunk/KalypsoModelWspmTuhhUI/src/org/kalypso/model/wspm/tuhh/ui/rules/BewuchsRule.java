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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.AddBewuchsResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class BewuchsRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points == null )
      return;
    if( !profil.getProfilPoints().propertyExists( POINT_PROPERTY.BEWUCHS_AX ) )
      return;
    final IProfilDevider[] devider = profil.getDevider( IProfilDevider.DEVIDER_TYP.TRENNFLAECHE );
    final IProfilPoint leftP = (devider.length > 0) ? devider[0].getPoint() : null;
    final IProfilPoint rightP = (devider.length > 1) ? devider[devider.length - 1].getPoint() : null;
    if( (leftP == null) || (rightP == null) )
      return;
    final int leftIndex = points.indexOf( leftP );
    final int rightIndex = points.indexOf( rightP );
    final List<IProfilPoint> VorlandL = points.subList( 0, leftIndex );
    final List<IProfilPoint> VorlandR = points.subList( rightIndex, points.size() );
    final List<IProfilPoint> Flussschl = points.subList( leftIndex, rightIndex );
    final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );

    try
    {
      if( (validateArea( collector, VorlandL, 0, pluginId ) | validateArea( collector, VorlandR, rightIndex, pluginId )) && !Flussschl.isEmpty() )
      {
        int i = leftIndex;
        for( IProfilPoint point : Flussschl )
        {
          final double ax = point.getValueFor( POINT_PROPERTY.BEWUCHS_AX );
          final double ay = point.getValueFor( POINT_PROPERTY.BEWUCHS_AY );
          final double dp = point.getValueFor( POINT_PROPERTY.BEWUCHS_DP );
          if( ax + ay + dp != 0 )
            collector.createProfilMarker( false, "Bewuchsparameter im Flußschlauch", "", i, POINT_PROPERTY.BEWUCHS_AX.toString(), pluginId, new AbstractProfilMarkerResolution[] { new DelBewuchsResolution() } );
          i++;
        }
        final int lastIndex = (leftIndex > 0) ? leftIndex - 1 : leftIndex;
        if( points.get( lastIndex ).getValueFor( POINT_PROPERTY.BEWUCHS_AX ) == 0 )
          collector.createProfilMarker( true, "Bewuchsparameter erforderlich", "", lastIndex, POINT_PROPERTY.BEWUCHS_AX.toString(), pluginId, new AbstractProfilMarkerResolution[] { new AddBewuchsResolution( 0 ) } );
        if( rightP.getValueFor( POINT_PROPERTY.BEWUCHS_AX ) == 0 )
          collector.createProfilMarker( true, "Bewuchsparameter erforderlich", "", rightIndex, POINT_PROPERTY.BEWUCHS_AX.toString(), pluginId, new AbstractProfilMarkerResolution[] { new AddBewuchsResolution( devider.length - 1 ) } );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private boolean validateArea( final IValidatorMarkerCollector collector, final List<IProfilPoint> subList, final int fromIndex, final String pluginId ) throws ProfilDataException, CoreException
  {
    boolean hasValues = false;
    if( subList.isEmpty() )
      return false;
    int i = fromIndex;
    for( IProfilPoint point : subList )
    {
      final double ax = point.getValueFor( POINT_PROPERTY.BEWUCHS_AX );
      final double ay = point.getValueFor( POINT_PROPERTY.BEWUCHS_AY );
      final double dp = point.getValueFor( POINT_PROPERTY.BEWUCHS_DP );
      if( ax + ay + dp != 0.0 )
      {
        if( ax * ay * dp == 0.0 )
        {
          final StringBuffer stringBuffer = new StringBuffer("Bewuchsparameter(");
          if (ax==0.0)
          {
            stringBuffer.append( "aX, " );
          }
          if (ay==0.0)
          {
            stringBuffer.append( "aY, " );
          }
          if (dp==0.0)
          {
            stringBuffer.append( "dP" );
          }
          stringBuffer.append( ") fehlt");
          collector.createProfilMarker( true, stringBuffer.toString(), "", i, POINT_PROPERTY.BEWUCHS_AX.toString(), pluginId, null );
        }
        else
          hasValues = true;
      }
      i++;
    }
    return hasValues;
  }
}
