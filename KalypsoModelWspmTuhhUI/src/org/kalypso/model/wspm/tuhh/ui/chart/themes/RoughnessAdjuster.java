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
package org.kalypso.model.wspm.tuhh.ui.chart.themes;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;

/**
 * Helper class for displacementof trennflaechen. If active, the mover will also change the roughness values of the
 * corresponding channel.<br>
 * TODO: we should use this from the flow zone panel as well...
 *
 * @author Gernot Belger
 */
public class RoughnessAdjuster
{
  private final IProfile m_profil;

  private final IProfilePointMarker m_devider;

  public RoughnessAdjuster( final IProfile profil, final IProfilePointMarker devider )
  {
    m_profil = profil;
    m_devider = devider;
    Assert.isTrue( m_devider.getComponent().getId().equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
  }

  public void moveDevider( final IProfileRecord newPoint )
  {
    final IProfilePointMarker[] trennflaechen = m_profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilePointMarker[] durchstroemte = m_profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( trennflaechen.length < 2 || durchstroemte.length < 2 )
      return;

    final IProfileRecord startDB = durchstroemte[0].getPoint();
    final IProfileRecord startTF = trennflaechen[0].getPoint();
    final IProfileRecord endTF = trennflaechen[1].getPoint();
    final IProfileRecord endDB = durchstroemte[1].getPoint();

    final int startDBindex = startDB.getIndex();
    final int startTFindex = startTF.getIndex();
    final int endTFindex = endTF.getIndex();
    final int endDBindex = endDB.getIndex();

    final int newPointIndex = newPoint.getIndex();
    final int oldPointIndex = m_devider.getPoint().getIndex();

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

      if( Objects.allNotNull( valueLeft, valueMain, valueRight ) )
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
    final Collection<IComponent> roughnessComponents = new ArrayList<>();

    final IComponent cmpKS = m_profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
    if( cmpKS != null )
      roughnessComponents.add( cmpKS );

    final IComponent cmpKST = m_profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );
    if( cmpKST != null )
      roughnessComponents.add( cmpKST );

    final IComponent clazzes = m_profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS );
    if( Objects.isNotNull( clazzes ) )
      roughnessComponents.add( clazzes );

    final IComponent factor = m_profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR );
    if( Objects.isNotNull( factor ) )
      roughnessComponents.add( factor );

    return roughnessComponents.toArray( new IComponent[roughnessComponents.size()] );
  }
}
