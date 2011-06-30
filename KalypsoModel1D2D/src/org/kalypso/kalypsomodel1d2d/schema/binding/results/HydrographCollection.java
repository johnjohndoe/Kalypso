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
package org.kalypso.kalypsomodel1d2d.schema.binding.results;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class HydrographCollection extends FeatureWrapperCollection<IHydrograph> implements IHydrographCollection
{

  public HydrographCollection( final Feature featureCol )
  {
    this( featureCol, IHydrograph.class, IHydrographCollection.QNAME_PROP_HYDROGRAPH_MEMBER );
  }

  public HydrographCollection( final Feature featureCol, final Class<IHydrograph> fwClass, final QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }

  @Override
  public IHydrograph findHydrograph( final GM_Position position, final double searchRectWidth )
  {
    final List<Feature> foundFeatures = findFeatures( position, searchRectWidth );
    if( foundFeatures.isEmpty() )
      return null;

    double min = Double.MAX_VALUE;
    IHydrograph nearest = null;
    for( final Feature feature : foundFeatures )
    {
      final IHydrograph curNode = (IHydrograph) feature.getAdapter( IHydrograph.class );

      GM_Object location = curNode.getLocation();
      if( curNode instanceof GM_Point )
      {
        GM_Point point = (GM_Point) location;
        final double curDist = position.getDistance( point.getPosition() );
        if( min > curDist )
        {
          nearest = curNode;
          min = curDist;
        }
      }
    }
    return nearest;
  }

  @SuppressWarnings("unchecked")
  private List<Feature> findFeatures( GM_Position position, double searchRectWidth )
  {
    final FeatureList nodeList = getWrappedList();
    final double posX = position.getX();
    final double posY = position.getY();
    final double searchWidthHalf = searchRectWidth / 2;

    final GM_Position minPos = GeometryFactory.createGM_Position( posX - searchWidthHalf, posY - searchWidthHalf );
    final GM_Position maxPos = GeometryFactory.createGM_Position( posX + searchWidthHalf, posY + searchWidthHalf );
    final GM_Envelope reqEnvelope = GeometryFactory.createGM_Envelope( minPos, maxPos, null );

    final List<Feature> foundFeatures = nodeList.query( reqEnvelope, null );
    return foundFeatures;
  }

  @Override
  public Map<IPath, Date> getResults( )
  {
    final Feature feature = getFeature();
    FeatureList resultFeatures = (FeatureList) feature.getProperty( QNAME_PROP_RESULT_MEMBER );

    if( resultFeatures == null )
      return null;

    final Map<IPath, Date> resultList = new HashMap<IPath, Date>();

    for( final Object object : resultFeatures )
    {
      Feature resultFeature = (Feature) object;
      if( resultFeature == null )
        return null;
      String pathString = (String) resultFeature.getProperty( QNAME_PROP_RESULT_MEMBER_PATH );
      IPath path = Path.fromPortableString( pathString );
      Date date = DateUtilities.toDate( (XMLGregorianCalendar) resultFeature.getProperty( QNAME_PROP_RESULT_MEMBER_DATE ) );
      resultList.put( path, date );
    }

    return resultList;
  }

  @Override
  public void setResults( final Map<IPath, Date> resultMap ) throws Exception
  {
    final GMLWorkspace workspace = getFeature().getWorkspace();

    final IRelationType relation = (IRelationType) getFeature().getFeatureType().getProperty( QNAME_PROP_RESULT_MEMBER );
    final IFeatureType featureType = relation.getTargetFeatureType();

    final Set<Entry<IPath, Date>> entrySet = resultMap.entrySet();

    for( Entry<IPath, Date> entry : entrySet )
    {
      final IPath path = entry.getKey();
      final String pathString = path.toPortableString();

      final Date date = entry.getValue();
      final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( date );

      /* Create the feature. */
      final Feature feature = workspace.createFeature( getFeature(), relation, featureType );

      /* set the values */
      feature.setProperty( QNAME_PROP_RESULT_MEMBER_PATH, pathString );
      feature.setProperty( QNAME_PROP_RESULT_MEMBER_DATE, gregorianCalendar );

      workspace.addFeatureAsComposition( getFeature(), relation, 0, feature );
    }
  }
}
