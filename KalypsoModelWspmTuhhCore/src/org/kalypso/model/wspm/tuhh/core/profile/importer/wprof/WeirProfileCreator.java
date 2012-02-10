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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class WeirProfileCreator extends GelaendeProfileCreator
{
  private final String m_weirPolygonID;

  public WeirProfileCreator( final String description, final ProfileData data, final String soilPolygonID, final String weirPolygonID )
  {
    super( description, data, soilPolygonID );
    m_weirPolygonID = weirPolygonID;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.GelaendeProfileCreator#configure(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  protected void configure( final IProfil profile ) throws CoreException
  {
    super.configure( profile );

    addWeir( profile );

    addWeirObject( profile );
  }

  private void addWeir( final IProfil profile ) throws CoreException
  {
    final IWProfPoint[] weirPoints = getPoints( m_weirPolygonID );

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IComponent component = provider.getPointProperty( POINT_PROPERTY_OBERKANTEWEHR );

    profile.addPointProperty( component );

    try
    {
      final int bridgeIndex = profile.indexOfProperty( POINT_PROPERTY_OBERKANTEWEHR );
      final int commentIndex = profile.indexOfProperty( POINT_PROPERTY_COMMENT );

      for( final IWProfPoint wprofPoint : weirPoints )
      {
        final BigDecimal distance = wprofPoint.getDistance();
        final double value = wprofPoint.getValue();
        final String comment = wprofPoint.getComment();

        final IRecord point = ProfilUtil.findOrInsertPointAt( profile, distance );

        point.setValue( bridgeIndex, value );

        if( comment != null && !comment.isEmpty() )
        {
          final String existingComment = (String) point.getValue( commentIndex );
          final String commentToSet = existingComment == null ? comment : existingComment + " - " + comment; //$NON-NLS-1$
          point.setValue( commentIndex, commentToSet );
        }
      }
    }
    catch( final Exception e )
    {
      final String message = String.format( "Unable to create profile at %.4f", profile.getStation() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private void addWeirObject( final IProfil profile )
  {
    final BuildingWehr weir = new BuildingWehr( profile );
// final IWProfPoint widthPoint = findBridgetWidth();
// if( widthPoint != null )
// setWidth( profile, bridge, widthPoint );
// setUWheight( profile, bridge, widthPoint );

    profile.addProfileObjects( new IProfileObject[] { weir } );
  }
}
