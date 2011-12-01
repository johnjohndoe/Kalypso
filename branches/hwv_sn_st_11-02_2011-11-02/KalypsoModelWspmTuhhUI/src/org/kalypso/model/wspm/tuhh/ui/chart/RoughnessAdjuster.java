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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * Helper class for displacementof trennflaechen. If active, the mover will also change the roughness values of the
 * corresponding channel.<br>
 * TODO: we should use this from the flow zone panel as well...
 * 
 * @author Gernot Belger
 */
public class RoughnessAdjuster
{
  private final IProfil m_profil;

  private final IProfilPointMarker m_devider;

  public RoughnessAdjuster( final IProfil profil, final IProfilPointMarker devider )
  {
    m_profil = profil;
    m_devider = devider;
    Assert.isTrue( m_devider.getId().getId().equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
  }

  public void moveDevider( final IRecord newPoint )
  {
    final IProfilPointMarker[] trennflaechen = m_profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] durchstroemte = m_profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( trennflaechen.length < 2 || durchstroemte.length < 2 )
      return;

    final IRecord startDB = durchstroemte[0].getPoint();
    final IRecord startTF = trennflaechen[0].getPoint();
    final IRecord endTF = trennflaechen[1].getPoint();
    final IRecord endDB = durchstroemte[1].getPoint();

    final int startDBindex = m_profil.indexOfPoint( startDB );
    final int startTFindex = m_profil.indexOfPoint( startTF );
    final int endTFindex = m_profil.indexOfPoint( endTF );
    final int endDBindex = m_profil.indexOfPoint( endDB );

    final int newPointIndex = m_profil.indexOfPoint( newPoint );
    final int oldPointIndex = m_profil.indexOfPoint( m_devider.getPoint() );

    final ProfileChannel leftChannel = new ProfileChannel( m_profil, startDBindex, startTFindex );
    final ProfileChannel mainChannel = new ProfileChannel( m_profil, startTFindex, endTFindex );
    final ProfileChannel rightChannel = new ProfileChannel( m_profil, endTFindex, endDBindex );

    final ProfileChannel newLeftChannel = moveChannel( leftChannel, oldPointIndex, newPointIndex );
    final ProfileChannel newMainChannel = moveChannel( mainChannel, oldPointIndex, newPointIndex );
    final ProfileChannel newRightChannel = moveChannel( rightChannel, oldPointIndex, newPointIndex );

    final IComponent[] roughnessComponents = findRoughnessComponents();
    for( final IComponent roughnessComponent : roughnessComponents )
    {
      final int roughnessIndex = m_profil.indexOfProperty( roughnessComponent );

      final Object valueLeft = leftChannel.getValue( roughnessIndex );
      final Object valueMain = mainChannel.getValue( roughnessIndex );
      final Object valueRight = rightChannel.getValue( roughnessIndex );

      if( valueLeft != null && valueMain != null && valueRight != null )
      {
        newLeftChannel.setValue( roughnessIndex, valueLeft );
        newMainChannel.setValue( roughnessIndex, valueMain );
        newRightChannel.setValue( roughnessIndex, valueRight );
      }
    }
  }

  private ProfileChannel moveChannel( final ProfileChannel channel, final int oldPointIndex, final int newPointIndex )
  {
    final int start = channel.getStartPoint();
    final int end = channel.getEndPoint();

    if( start == oldPointIndex && end == oldPointIndex )
      return new ProfileChannel( m_profil, newPointIndex, newPointIndex );

    if( start == oldPointIndex )
      return new ProfileChannel( m_profil, newPointIndex, end );

    if( end == oldPointIndex )
      return new ProfileChannel( m_profil, start, newPointIndex );

    return channel;
  }

  private IComponent[] findRoughnessComponents( )
  {
    final Collection<IComponent> roughnessComponents = new ArrayList<IComponent>( 2 );

    final IComponent cmpKS = m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    if( cmpKS != null )
      roughnessComponents.add( cmpKS );

    final IComponent cmpKST = m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( cmpKST != null )
      roughnessComponents.add( cmpKST );

    return roughnessComponents.toArray( new IComponent[roughnessComponents.size()] );
  }
}
