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
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.resolutions.EditPointResolution;
import org.kalypso.model.wspm.tuhh.ui.resolutions.MoveDeviderResolution;

public class WehrRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( (profil == null) || (profil.getProfileObject() == null) || (!IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( profil.getProfileObject().getId() )) )
      return;
    try
    {
      final String pluginId = PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() );
      final IProfilPoint leftP = validateLimits( profil, collector, pluginId );
      validateProfilLines( profil, collector, pluginId );
      validateDevider( profil, collector, pluginId );
      validateParams( profil, collector, pluginId, leftP );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilPointMarker[] wehrDevider = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_WEHR );
    if( wehrDevider.length < 1 )
      return;
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final int index1 = points.indexOf( wehrDevider[0].getPoint() );
    final int index2 = points.indexOf( wehrDevider[wehrDevider.length - 1].getPoint() );
    final int index3 = points.indexOf( deviders[0].getPoint() );
    final int index4 = points.indexOf( deviders[deviders.length - 1].getPoint() );
    if( index1 < index3 )
    {
      collector.createProfilMarker( true, "Wehrfeldtrenner: ungültige Position", "", index1, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.toString(), pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( 0, IWspmTuhhConstants.MARKER_TYP_WEHR, index3 ) } );
    }
    if( index2 > index4 )
    {
      collector.createProfilMarker( true, "Wehrfeldtrenner: ungültige Position", "", index2, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.toString(), pluginId, new IMarkerResolution2[] { new MoveDeviderResolution( wehrDevider.length - 1, IWspmTuhhConstants.MARKER_TYP_WEHR, index4 ) } );
    }
  }

  private void validateParams( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId, final IProfilPoint p ) throws Exception
  {

    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_WEHR );
    final IProfileObject profileObject = profil.getProfileObject();

    final Double beiwert = (Double) profileObject.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT );
    IProfilPoint point = (beiwert == null || beiwert == 0.0) ? p : null;

    for( final IProfilPointMarker devider : deviders )
    {
      final double value = devider.getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BEIWERT ) == null ? 0.0 : (Double) devider.getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BEIWERT );
      if( value == 0.0 )
      {
        point = devider.getPoint();
        break;
      }
    }
    for( final String property : profileObject.getObjectProperties() )
    {
      final Object prop = profileObject.getValueFor( property );
      if( prop instanceof Double && ((Double) prop).isNaN() )
      {
        collector.createProfilMarker( true, "Parameter <" + profileObject.getLabelFor( property ) + "> fehlt", "", 0, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, pluginId, null );
        break;
      }
    }
    if( point != null )
      collector.createProfilMarker( true, "ungültiger Kronenparameter: 0.0", "", profil.getPoints().indexOf( point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.toString(), pluginId, null );
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final List<IProfilPoint> points = profil.getPoints();
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( deviders.length < 1 )
      return;
    final int left = points.indexOf( deviders[0].getPoint() );
    final int right = points.indexOf( deviders[deviders.length - 1].getPoint() );
    if( (left + 1) > right )
      return;
    final List<IProfilPoint> midPoints = points.subList( left + 1, right );
    for( final IProfilPoint point : midPoints )
    {
      final double h = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
      final double wk = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
      if( wk < h )
      {
        collector.createProfilMarker( true, "Wehrkante[" + String.format( FMT_BREITE, point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) + "] unterhalb Geländeniveau", "", points.indexOf( point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.toString(), pluginId, null );
      }
    }
  }

  private IProfilPoint validateLimits( final IProfil profil, final IValidatorMarkerCollector collector, final String pluginId ) throws Exception
  {
    final IProfilPointMarker[] devider = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( devider.length < 2 )
      return null;
    final IProfilPoint firstPoint = devider[0].getPoint();
    final IProfilPoint lastPoint = devider[devider.length - 1].getPoint();
    IProfilPoint point = null;
    final double p1H = firstPoint.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
    final double p1OkW = firstPoint.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final double p2H = lastPoint.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
    final double p2OkW = lastPoint.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    // final double dB = profil.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE ).getPrecision();
    final double dOkW = profil.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ).getPrecision();
    if( Math.abs( p1H - p1OkW ) > dOkW )
      point = firstPoint;
    if( Math.abs( p2H - p2OkW ) > dOkW )
      point = lastPoint;
    if( point != null )
    {
      final int index = profil.getPoints().indexOf( point );
      collector.createProfilMarker( true, "ungültige Randbedingung [" + String.format( FMT_BREITE, point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) + "]", "", profil.getPoints().indexOf( point ), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.toString(), pluginId, new IMarkerResolution2[] { new EditPointResolution( index, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) ) } );
    }
    return firstPoint;
  }
}
