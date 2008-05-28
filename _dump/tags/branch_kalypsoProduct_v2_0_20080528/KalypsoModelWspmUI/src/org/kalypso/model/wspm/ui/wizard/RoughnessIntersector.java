/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.ui.wizard;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.assignment.AssignmentBinder;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Gernot Belger
 */
public class RoughnessIntersector
{
  private final Object[] m_profileFeatures;

  private final FeatureList m_polygoneFeatures;

  private final IPropertyType m_polygoneGeomType;

  private final IPropertyType m_polygoneValueType;

  private final AssignmentBinder m_assignment;

  private final IProfilePointFilter[] m_pointFilters;

  public RoughnessIntersector( final Object[] profileFeatures, final FeatureList polygoneFeatures, final IPropertyType polygoneGeomType, final IPropertyType polygoneValueType, final AssignmentBinder assignment, final IProfilePointFilter[] pointFilters )
  {
    m_profileFeatures = profileFeatures;
    m_polygoneFeatures = polygoneFeatures;
    m_polygoneGeomType = polygoneGeomType;
    m_polygoneValueType = polygoneValueType;
    m_assignment = assignment;
    m_pointFilters = pointFilters;
  }

  @SuppressWarnings("unchecked")//$NON-NLS-1$
  public FeatureChange[] intersect( final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( Messages.RoughnessIntersector_1, m_profileFeatures.length );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    /* apply polygone data to profile data */
    for( final Object object : m_profileFeatures )
    {
      final Feature profileFeature = (Feature) object;
      WspmProfile wspmProfile = new WspmProfile( profileFeature );
      String crs = wspmProfile.getSrsName();
      final IProfil profil = wspmProfile.getProfil();
      // TODO: check if the profile has all components already.
      // but how to do, we don't know here what components are necessary for the current profile...
      final String label = FeatureHelper.getAnnotationValue( profileFeature, IAnnotation.ANNO_LABEL );

      final IRecord[] points = profil.getPoints();

      int count = 1;
      final int length = points.length;

      for( final IRecord point : points )
      {
        monitor.subTask( label + " (" + count + "/" + length + ")" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        if( !acceptPoint( profil, point ) )
          continue;

        final GM_Point geoPoint = ProfileCacherFeaturePropertyFunction.convertPoint( profil, point, crs );
        if( geoPoint == null )
          continue;
        final Geometry jtsPoint = JTSAdapter.export( geoPoint );
        assignValueToPoint( profil, point, geoPoint, jtsPoint );

        count++;

      }

      final FeatureChange[] fcs = ProfileFeatureFactory.toFeatureAsChanges( profil, profileFeature );
      Collections.addAll( changes, fcs );

      ProgressUtilities.worked( monitor, 1 );
    }

    return changes.toArray( new FeatureChange[changes.size()] );
  }

  @SuppressWarnings("unchecked")//$NON-NLS-1$
  private List<Object> assignValueToPoint( final IProfil profil, final IRecord point, final GM_Point geoPoint, final Geometry jtsPoint ) throws GM_Exception
  {
    final TupleResult owner = point.getOwner();

    /* find polygon for location */
    final List<Object> foundPolygones = m_polygoneFeatures.query( geoPoint.getPosition(), null );
    for( final Object polyObject : foundPolygones )
    {
      final Feature polygoneFeature = (Feature) polyObject;

      // BUGFIX: use any gm_object here, because we do not know what it is (surface, multi surface, ...)
      final GM_Object gmObject = (GM_Object) polygoneFeature.getProperty( m_polygoneGeomType );

      final Geometry jtsGeom = JTSAdapter.export( gmObject );
      if( jtsGeom.contains( jtsPoint ) )
      {
        final Object polygoneValue = polygoneFeature.getProperty( m_polygoneValueType );
        if( polygoneValue != null )
        {
          // find assignment for polygon
          final Map<String, Double> assignments = m_assignment.getAssignmentsFor( polygoneValue.toString() );
          // apply assignment to point properties
          for( final Map.Entry<String, Double> entry : assignments.entrySet() )
          {
            final String componentId = entry.getKey();
            final Double newValue = entry.getValue();

            if( newValue != null )
              if( componentId != null )
              {
                final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );
// final IComponent component = ProfilObsHelper.getPropertyFromId( profil, componentId );

                final IComponent component = provider.getPointProperty( componentId );

                profil.addPointProperty( component );

                point.setValue( owner.indexOfComponent( component ), newValue );
              }
          }
        }
        // DONT break, because we may have several polygone covering the point, but only one has an assigned value
        // break;
      }
    }
    return foundPolygones;
  }

  private boolean acceptPoint( final IProfil profil, final IRecord point )
  {
    for( final IProfilePointFilter pointFilter : m_pointFilters )
      if( !pointFilter.accept( profil, point ) )
        return false;

    return true;
  }

}
