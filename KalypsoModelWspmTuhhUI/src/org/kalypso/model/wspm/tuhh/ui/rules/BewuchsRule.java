/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  DenickestraÔøΩe 22
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
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.resolutions.DelBewuchsResolution;
import org.kalypso.observation.result.IComponent;

/**
 * kein Bewuchs im Fluﬂschlauch wenn Bewuchs, dann (AX und AY und DP) != 0.0 wenn Bewuchs, dann muﬂ auch an Trennfl‰chen
 * Bewuchs definiert sein
 *
 * @author kimwerner
 * @author Dirk Kuch
 */
public class BewuchsRule extends AbstractValidatorRule
{
  @Override
  public void validate( final IProfile profile, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( Objects.isNull( profile ) )
      return;

    if( !WspmClassifications.hasVegetationProperties( profile ) && !WspmClassifications.hasVegetationClass( profile ) )
      return;

    final IProfileRecord[] points = profile.getPoints();

    /**
     * Bewuchs im Fluﬂschlauch ?
     */
    final IComponent cTrennF = profile.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilePointMarker[] devider = profile.getPointMarkerFor( cTrennF );
    if( devider.length < 2 )
      return;

    final IProfileRecord leftP = devider[0].getPoint();
    final IProfileRecord rightP = devider[devider.length - 1].getPoint();
    if( leftP == null || rightP == null )
      return;

    final int leftIndex = leftP.getIndex();
    final int rightIndex = rightP.getIndex();

    // FIXME: use durchstrˆmte Berichte to determine vorland -> check what the calculation core really needs

    final IProfileRecord[] leftForeland = profile.getPoints( 0, leftIndex - 1 );
    final IProfileRecord[] rightForeland = profile.getPoints( rightIndex, points.length - 1 );
    final IProfileRecord[] riverTube = profile.getPoints( leftIndex, rightIndex - 1 );

    if( !Arrays.isEmpty( riverTube ) && WspmSohlpunkte.getBuilding( profile, IProfileBuilding.class ) == null )
    {
      int i = leftIndex;
      for( final IProfileRecord point : riverTube )
      {
        final double ax = WspmClassifications.getAx( point );
        final double ay = WspmClassifications.getAy( point );
        final double dp = WspmClassifications.getDp( point );

        if( Doubles.isNaN( ax, ay, dp ) )
        {
          continue;
        }
        else
        {
          if( ax + ay + dp != 0 )
          {
            collector.createProfilMarker( IMarker.SEVERITY_WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BewuchsRule.0" ), String.format( "km %.4f", profile.getStation() ), i, getVegetationPointProperty( profile ), new DelBewuchsResolution() ); //$NON-NLS-1$ //$NON-NLS-2$
            break;
          }
        }
        i++;
      }
      final int lastIndex = leftIndex > 0 ? leftIndex - 1 : leftIndex;

      if( WspmSohlpunkte.getBuilding( profile, IProfileBuilding.class ) == null )
      {
        final boolean leftForelandHasValues = validateArea( profile, collector, leftForeland );
        final boolean rightForelandHasValues = validateArea( profile, collector, rightForeland );

        final Double ax1 = WspmClassifications.getAx( points[lastIndex] );

        if( leftForelandHasValues && !ax1.isNaN() && ax1 == 0.0 )
        {
          collector.createProfilMarker( IMarker.SEVERITY_INFO, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BewuchsRule.2" ), String.format( "km %.4f", profile.getStation() ), lastIndex, getVegetationPointProperty( profile ) );// , //$NON-NLS-1$ //$NON-NLS-2$
        }

        final Double ax2 = WspmClassifications.getAx( rightP );

        if( rightForelandHasValues && !ax2.isNaN() && rightForelandHasValues && ax2 == 0.0 )
        {
          collector.createProfilMarker( IMarker.SEVERITY_INFO, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BewuchsRule.2" ), String.format( "km %.4f", profile.getStation() ), rightIndex, getVegetationPointProperty( profile ) );// , //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    }
  }

  private boolean validateArea( final IProfile profil, final IValidatorMarkerCollector collector, final IProfileRecord[] subList ) throws CoreException
  {
    boolean hasValues = false;
    boolean hasErrors = false;
    if( subList.length < 1 )
      return false;

    for( final IProfileRecord point : subList )
    {
      final double ax = WspmClassifications.getAx( point );
      final double ay = WspmClassifications.getAy( point );
      final double dp = WspmClassifications.getDp( point );

      if( Doubles.isNaN( ax, ay, dp ) )
      {
        // displays only first error
        if( hasErrors )
        {
          continue;
        }

        hasErrors = true;
        final String stationFormatted = String.format( Messages.getString( "BewuchsRule.0" ), profil.getStation() ); //$NON-NLS-1$

        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BewuchsRule.7" ), stationFormatted, point.getIndex(), getVegetationPointProperty( profil ) ); //$NON-NLS-1$ //$NON-NLS-2$

      }
      else if( ax < 0 || ay < 0 || dp < 0 )
      {
        // displays only first error
        if( hasErrors )
        {
          continue;
        }

        hasErrors = true;
        final String stationFormatted = String.format( Messages.getString( "BewuchsRule.0" ), profil.getStation() ); //$NON-NLS-1$

        collector.createProfilMarker( IMarker.SEVERITY_ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BewuchsRule.8" ), stationFormatted, point.getIndex(), getVegetationPointProperty( profil ) ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      else
      {
        if( ax + ay + dp != 0.0 )
          if( ax * ay * dp == 0.0 )
          {
            if( hasErrors )
            {
              continue;
            }
            final StringBuffer stringBuffer = new StringBuffer();
            if( ax == 0.0 )
            {
              stringBuffer.append( "aX" ); //$NON-NLS-1$
            }
            if( ay == 0.0 )
            {
              stringBuffer.append( stringBuffer.length() == 0 ? "aY" : ", aY" ); //$NON-NLS-1$ //$NON-NLS-2$
            }
            if( dp == 0.0 )
            {
              stringBuffer.append( stringBuffer.length() == 0 ? "dP" : ", dp" ); //$NON-NLS-1$ //$NON-NLS-2$
            }

            final String text = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.rules.BewuchsRule.9", stringBuffer.toString() ); //$NON-NLS-1$
            hasErrors = true;

            collector.createProfilMarker( IMarker.SEVERITY_ERROR, text, String.format( "km %.4f", profil.getStation() ), point.getIndex(), getVegetationPointProperty( profil ) ); //$NON-NLS-1$
          }
          else
          {
            hasValues = true;
          }
      }
    }
    return hasValues;
  }

  private String getVegetationPointProperty( final IProfile profil )
  {
    if( WspmClassifications.hasVegetationProperties( profil ) )
      return IWspmConstants.POINT_PROPERTY_BEWUCHS_AX;
    else if( WspmClassifications.hasVegetationClass( profil ) )
      return IWspmConstants.POINT_PROPERTY_BEWUCHS_CLASS;

    // fallback
    return IWspmConstants.POINT_PROPERTY_BEWUCHS_AX;
  }
}
