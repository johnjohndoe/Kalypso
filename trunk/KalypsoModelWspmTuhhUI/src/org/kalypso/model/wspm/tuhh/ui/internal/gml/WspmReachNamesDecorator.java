/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.internal.gml;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

public class WspmReachNamesDecorator implements ILightweightLabelDecorator
{
  @Override
  public void dispose( )
  {
  }

  @Override
  public void addListener( final ILabelProviderListener listener )
  {
  }

  @Override
  public void removeListener( final ILabelProviderListener listener )
  {
  }

  @Override
  public boolean isLabelProperty( final Object element, final String property )
  {
    return true;
  }

  @Override
  public void decorate( final Object element, final IDecoration decoration )
  {
    if( element instanceof IProfileFeature )
      decorateProfile( (IProfileFeature)element, decoration );
  }

  private void decorateProfile( final IProfileFeature profile, final IDecoration decoration )
  {
    final WspmWaterBody water = profile.getWater();
    if( water == null )
      return;

    final String[] reachNames = collectReachNames( water, profile );
    if( ArrayUtils.isEmpty( reachNames ) )
      return;

    final String names = StringUtils.join( reachNames, ", " ); //$NON-NLS-1$
    final String suffix = String.format( Messages.getString("WspmReachNamesDecorator_0"), names ); //$NON-NLS-1$

    final String shortenedSuffix = StringUtils.abbreviate( suffix, 255 );

    decoration.addSuffix( shortenedSuffix );
  }

  private String[] collectReachNames( final WspmWaterBody water, final IProfileFeature profile )
  {
    final IFeatureBindingCollection<WspmReach> reaches = water.getReaches();

    final Collection<String> names = new ArrayList<>( reaches.size() );

    for( final WspmReach wspmReach : reaches )
    {
      if( containsProfile( wspmReach, profile ) )
        names.add( wspmReach.getName() );
    }

    return names.toArray( new String[names.size()] );
  }

  private boolean containsProfile( final WspmReach wspmReach, final IProfileFeature profile )
  {
    if( !(wspmReach instanceof TuhhReach) )
      return false;

    final TuhhReach reach = (TuhhReach)wspmReach;
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature member = segment.getProfileMember();
      if( member == profile )
        return true;
    }

    return false;
  }
}