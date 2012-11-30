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

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.sobek.ISobekConstants;
import org.kalypso.model.wspm.core.profil.sobek.utils.hw.BridgeResult;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class SobekProfileDefExportOperation extends AbstractSobekFileExportOperation
{
  public SobekProfileDefExportOperation( final SobekExportInfo info )
  {
    super( info, ISobekConstants.PROFILE_DEF );
  }

  @Override
  public String getLabel( )
  {
    return ISobekConstants.PROFILE_DEF;
  }

  @Override
  protected void writeProfile( final IProfileFeature profileFeature )
  {
    final IProfile profil = profileFeature.getProfile();
    final IRecord[] points = getPointsToExport( profil );
    if( points == null )
      return;

    final SobekExportInfo info = getInfo();

    final String id = info.getID( profileFeature );
    final String profileName = info.getName( profileFeature );

    writeNormalProfile( id, profileName, points, profil );
    writeProfileBuilding( id, profileName, points, profil );
  }

  private void writeNormalProfile( final String id, final String profileName, final IRecord[] points, final IProfile profil )
  {
    final Formatter formatter = getFormatter();
    formatter.format( "CRDS id '%s' nm '%s' ty 10 st 0 lt sw 0 0 gl 0 gu 0 lt yz%n", id, profileName ); //$NON-NLS-1$
    formatter.format( "TBLE%n" ); //$NON-NLS-1$

    final int widhtIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heightIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

    for( final IRecord point : points )
    {
      final Number y = (Number) point.getValue( widhtIndex );
      final Number z = (Number) point.getValue( heightIndex );
      formatter.format( Locale.US, "%.4f %.4f <%n", y, z ); //$NON-NLS-1$
    }
    formatter.format( "tble%n" ); //$NON-NLS-1$
    formatter.format( "crds%n" ); //$NON-NLS-1$
  }

  private void writeProfileBuilding( final String id, final String profileName, final IRecord[] points, final IProfile profil )
  {
    final SobekExportInfo info = getInfo();
    if( !info.getExportBuildings() )
      return;

    // switch over building type
    final IProfileObject[] profileObjects = profil.getProfileObjects();
    int reallyExportedBuildings = 0;
    for( final IProfileObject profileObject : profileObjects )
    {
      final String countSuffix = reallyExportedBuildings == 0 ? StringUtils.EMPTY : "_" + reallyExportedBuildings; //$NON-NLS-1$
      final String idSuffix = info.getIdSuffix();
      final String buildingId = id + idSuffix + countSuffix;

      reallyExportedBuildings++;

      final Formatter formatter = getFormatter();
      if( profileObject instanceof BuildingBruecke )
      {
        writeBridge( formatter, buildingId, profileName, points, profil );
        // TODO: support other building types (tube, ...)
      }
      else
      {
        reallyExportedBuildings--;
      }
    }
  }

  private void writeBridge( final Formatter formatter, final String id, final String profileName, final IRecord[] points, final IProfile profil )
  {
    final Collection<Coordinate> lowerCrds = new ArrayList<>();
    final Collection<Coordinate> upperCrds = new ArrayList<>();

    final int widthIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int heigthIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int ukIndex = profil.indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );

    if( widthIndex == -1 || heigthIndex == -1 || ukIndex == -1 )
      // FIXME: add error status
      return;

    for( final IRecord point : points )
    {
      final Object widthValue = point.getValue( widthIndex );
      final Object heightValue = point.getValue( heigthIndex );
      final Object ukValue = point.getValue( ukIndex );

      if( widthValue instanceof Number )
      {
        final double width = ((Number) widthValue).doubleValue();
        if( heightValue instanceof Number )
        {
          final double heigth = ((Number) heightValue).doubleValue();
          lowerCrds.add( new Coordinate( width, heigth ) );
        }

        if( ukValue instanceof Number )
        {
          final double uk = ((Number) ukValue).doubleValue();
          upperCrds.add( new Coordinate( width, uk ) );
        }
      }
    }

    /* Only for testing purposes: write tube geometry into shape file */
    final File tempDir = new File( getInfo().getTargetDir(), "struct" ); //$NON-NLS-1$
    tempDir.mkdirs();
    final String shapeFileName = "struct_";//$NON-NLS-1$

    final BridgeResult bridgeHw = new BridgeResult( shapeFileName, id, id, profileName, lowerCrds, upperCrds, tempDir );
    // TODO: handle problems/errors (change formattErr to a IStatusCollector)
    bridgeHw.formatOut( formatter );
  }

  private IRecord[] getPointsToExport( final IProfile profil )
  {
    final String flowZone = getInfo().getFlowZone();
    if( StringUtils.isBlank( flowZone ) )
      return profil.getPoints();

    final IComponent markerComponent = ComponentUtilities.getFeatureComponent( flowZone );
    final String unknownLabel = String.format( Messages.getString( "SobekDefExportOperation.1" ), flowZone ); //$NON-NLS-1$
    final String markerLabel = markerComponent == null ? unknownLabel : ComponentUtilities.getComponentLabel( markerComponent );

    final IProfilePointMarker[] markers = profil.getPointMarkerFor( flowZone );
    if( markers.length < 2 )
    {
      final String message = String.format( Messages.getString( "SobekDefExportOperation_2" ), markerLabel, profil.getStation(), profil.getName() ); //$NON-NLS-1$
      getLog().add( IStatus.WARNING, message );
      return profil.getPoints();
    }

    final IProfileRecord startPoint = markers[0].getPoint();
    final IProfileRecord endPoint = markers[1].getPoint();

    final int startIndex = startPoint.getIndex();
    final int endIndex = endPoint.getIndex();

    return profil.getPoints( startIndex, endIndex );
  }
}