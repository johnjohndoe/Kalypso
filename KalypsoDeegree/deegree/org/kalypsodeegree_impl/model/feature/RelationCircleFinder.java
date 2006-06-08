/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypsodeegree_impl.model.feature;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class RelationCircleFinder
{

  private final GMLWorkspace m_workspace;

  private final Feature m_testFeature;

  /**
   *  
   */
  public RelationCircleFinder( GMLWorkspace workspace, Feature testFeature )
  {
    m_workspace = workspace;
    m_testFeature = testFeature;
  }

  public List[] findCircle( )
  {
    return findCircle( m_testFeature, new ArrayList<Feature>() );
  }

  private List<Feature>[] findCircle( final Feature feature, final List<Feature> list )
  {
    final List<List<Feature>> result = new ArrayList<List<Feature>>();
    list.add( feature );
    final Feature[] linkedFeatures = getLinkedFeatures( feature );
    for( int i = 0; i < linkedFeatures.length; i++ )
    {
      final List<Feature> newList = new ArrayList<Feature>( list );
      final Feature linkFeature = linkedFeatures[i];
      if( linkFeature == m_testFeature )
        result.add( newList );
      else if( list.contains( linkFeature ) )
      {
        // an other circle
        System.out.println( "found an other circle:\n" + list.toString() + " : " + linkFeature.toString() );
      }
      else
      {
        final List<Feature>[] lists = findCircle( linkFeature, newList );
        result.addAll( java.util.Arrays.asList( lists ) ); // TODO modified from kalypso.contribs.java.util.Arrays to
                                                            // java.util.Arrays: check if this is ok
      }
    }
    return result.toArray( new List[result.size()] );
  }

  private Feature[] getLinkedFeatures( Feature feature )
  {
    final List result = new ArrayList();
    IFeatureType featureType = feature.getFeatureType();
    IPropertyType[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      IPropertyType property = properties[i];
      if( property instanceof IRelationType )
      {
        final IRelationType linkPT = (IRelationType) property;
        if( property.isList() )
        {
          // TODO: is this really intended? The list is added, not its content!
          final Feature[] links = m_workspace.resolveLinks( feature, linkPT );
          result.add( java.util.Arrays.asList( links ) ); // TODO modified from kalypso.contribs.java.util.Arrays to
                                                          // java.util.Arrays: check if this is ok
        }
        else
        {
          final Feature link = m_workspace.resolveLink( feature, linkPT );
          if( link != null )
            result.add( link );
        }
      }
    }
    return (Feature[]) result.toArray( new Feature[result.size()] );
  }
}
