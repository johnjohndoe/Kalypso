/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.gml.selection;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author belger
 */
public class FeatureSelectionHelper
{
  /** Return all features contained in the selection */
  public static Feature[] getFeatures( final IFeatureSelection selection )
  {
    return getFeatures( selection, null );
  }

  /**
   * Return all features contained in the selection which live in the given workspace.
   * 
   * @param filterWorkspace
   *          if null, all features are returned
   */
  public static Feature[] getFeatures( final IFeatureSelection selection, final GMLWorkspace filterWorkspace )
  {
    final List list = selection.toList();
    final ArrayList features = new ArrayList( list.size() );
    for( final Iterator iter = list.iterator(); iter.hasNext(); )
    {
      final Object element = iter.next();
      if( element instanceof Feature )
      {
        if( filterWorkspace == null
            || ( filterWorkspace != null && selection.getWorkspace( (Feature)element ) == filterWorkspace ) )
          features.add( element );
      }
    }

    return (Feature[])features.toArray( new Feature[features.size()] );
  }

  public static boolean compare( final Feature[] globalFeatures, final Feature[] features )
  {
    final HashSet globalSet = new HashSet( Arrays.asList( globalFeatures ) );
    final HashSet set = new HashSet( Arrays.asList( features ) );

    return globalSet.equals( set );
  }

  /** Return the amount of features contained in the given selection. */
  public static int getFeatureCount( final IFeatureSelection selection )
  {
    return getFeatures( selection ).length;
  }

  public static EasyFeatureWrapper[] createEasyWrappers( final IFeatureSelection selection )
  {
    final Feature[] features = getFeatures( selection );
    final EasyFeatureWrapper[] wrappers = new EasyFeatureWrapper[features.length];
    for( int i = 0; i < wrappers.length; i++ )
    {
      final Feature f = features[i];
      wrappers[i] = new EasyFeatureWrapper( selection.getWorkspace( f ), f, selection.getParentFeature( f ), selection
          .getParentFeatureProperty( f ) );
    }

    return wrappers;
  }

  public static Feature getFirstFeature( final IFeatureSelection selection )
  {
    final Feature[] features = getFeatures( selection );
    if( features.length == 0 )
      return null;

    return features[0];
  };
}
