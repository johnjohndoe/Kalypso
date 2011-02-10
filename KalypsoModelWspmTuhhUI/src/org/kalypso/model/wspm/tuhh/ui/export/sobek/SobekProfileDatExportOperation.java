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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class SobekProfileDatExportOperation extends AbstractSobekFileExportOperation
{
  public static final String PROFILE_DAT = "profile.dat"; //$NON-NLS-1$

  public SobekProfileDatExportOperation( final SobekExportInfo info )
  {
    super( info, PROFILE_DAT );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.ISobekProfileExportOperation#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return PROFILE_DAT;
  }

  @Override
  protected void writeProfile( final IProfileFeature profileFeature )
  {
    final SobekExportInfo info = getInfo();

    final String id = info.getID( profileFeature );

    final IRecord[] surfacePoints = getSurfaceIndices( profileFeature );
    if( surfacePoints == null )
      return;

    final int rl = 0;

    final double rs = getHeight( surfacePoints[0] );
    final double ls = getHeight( surfacePoints[1] );

    getFormatter().format( Locale.US, "CRSN id '%s' di '%s' rl %d  rs %.2f ls %.2f crsn%n", id, id, rl, rs, ls );
  }

  private double getHeight( final IRecord point )
  {
    final int widthindex = point.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE );
    return ProfilUtil.getDoubleValueFor( widthindex, point );
  }

  private IRecord[] getSurfaceIndices( final IProfileFeature profileFeature )
  {
    final IProfil profil = profileFeature.getProfil();
    final IRecord[] points = profil.getPoints();
    if( points.length < 2 )
    {
      // TODO: log?
      return null;
    }

    final SobekExportInfo info = getInfo();
    final String flowZone = info.getFlowZone();
    if( StringUtils.isBlank( flowZone ) )
      return new IRecord[] { points[0], points[points.length - 1] };

    final IComponent markerComponent = ComponentUtilities.getFeatureComponent( flowZone );
    final String unknownLabel = String.format( Messages.getString( "SobekDefExportOperation.1" ), flowZone ); //$NON-NLS-1$
    final String markerLabel = markerComponent == null ? unknownLabel : ComponentUtilities.getComponentLabel( markerComponent );

    final IProfilPointMarker[] markers = profil.getPointMarkerFor( flowZone );
    if( markers.length < 2 )
    {
      final String message = String.format( Messages.getString( "SobekDefExportOperation_2" ), markerLabel, profil.getStation(), profil.getName() ); //$NON-NLS-1$
      getLog().add( IStatus.WARNING, message );
      return null;
    }

    final IRecord startPoint = markers[0].getPoint();
    final IRecord endPoint = markers[1].getPoint();

    return new IRecord[] { startPoint, endPoint };
  }
}
