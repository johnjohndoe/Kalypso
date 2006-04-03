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
package org.kalypsodeegree_impl.model.feature.gmlxpath;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.IXElement;

/**
 * @author doemming
 */
public class GMLXPathUtilities
{

  /**
   * query xPath for GMLWorkspace
   */
  public static Object query( final GMLXPath xPath, final GMLWorkspace workspace ) throws GMLXPathException
  {
    return getResultForSegment( xPath, workspace, 0, true );
  }

  /**
   * query xPath for Feature
   */
  public static Object query( final GMLXPath xPath, final Feature feature ) throws GMLXPathException
  {
    return getResultForSegment( xPath, feature, 0, false );
  }

  private static Object getResultForSegment( final GMLXPath xPath, final Object context, final int segmentIndex, boolean isFeatureTypeLevel ) throws GMLXPathException
  {
    if( segmentIndex >= xPath.getSegmentSize() )
      return context;
    final GMLXPathSegment segment = xPath.getSegment( segmentIndex );

    final Object newContext = getValueFromSegment( segment, context, isFeatureTypeLevel );

    if( segmentIndex + 1 >= xPath.getSegmentSize() )
      return newContext;
    // operate next level in xpath
    // recursion starts...
    if( newContext instanceof Feature )
      return getResultForSegment( xPath, newContext, segmentIndex + 1, !isFeatureTypeLevel );
    if( newContext instanceof List )
    {
      final List contextList = (FeatureList) newContext;
      final Iterator iterator = contextList.iterator();
      // final FeatureList results = FeatureFactory.createFeatureList( fList.getParentFeature(),
      // fList.getParentFeatureTypeProperty() );
      final List<Object> resultList = new ArrayList<Object>();
      while( iterator.hasNext() )
      {
        final Object object = iterator.next();
        if( object instanceof Feature )
        {
          final Object result = getResultForSegment( xPath, object, segmentIndex + 1, !isFeatureTypeLevel );
          if( result != null )
            resultList.add( result );
        }
      }
      if( resultList.size() == 1 )
        return resultList.get( 0 );
      return resultList;
    }
    // everything else is a error, as only feaures and featurelists can have subelements
    return null;
  }

  /**
   * @return result of xpath expression for one xpath segment
   */
  private static Object getValueFromSegment( final GMLXPathSegment xSegment, final Object context, boolean isFeatureTypeLevel ) throws GMLXPathException
  {
    final IXElement addressXElement = xSegment.getAddressXElement();

    final Object newContext = addressXElement.evaluate( context, isFeatureTypeLevel );

    final IXElement conditionXElement = xSegment.getConditionXElement();
    if( conditionXElement == null )
      return newContext;

    if( newContext instanceof Feature )
    {
      final Boolean b = (Boolean) conditionXElement.evaluate( newContext, isFeatureTypeLevel );
      if( b )
        return newContext;
      return null;
    }
    throw new GMLXPathException();
  }

}
