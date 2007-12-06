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
package org.kalypso.kalypso1d2d.pjt.map;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Thomas Jung
 * 
 */
public final class HydrographProcessResultsOperation2 implements ICoreRunnableWithProgress
{
  private final IHydrographCollection m_hydrographs;

  private final Map<IPath, Date> m_resultMap;

  private final IFolder m_scenarioFolder;

  public HydrographProcessResultsOperation2( final IHydrographCollection hydrographs, final Map<IPath, Date> resultMap, final IFolder scenarioFolder )
  {
    m_hydrographs = hydrographs;
    m_resultMap = resultMap;
    m_scenarioFolder = scenarioFolder;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      final Set<Entry<IPath, Date>> entrySet = m_resultMap.entrySet();
      for( Entry<IPath, Date> entry : entrySet )
      {
        /* get the observation */

        /* get the date for which this result is valid */
        final Date date = entry.getValue();

        /* get the node result gml */
        final IFolder folder = m_scenarioFolder.getFolder( entry.getKey() );

        final URL hydrographURL = ResourceUtilities.createURL( folder );
        final GMLWorkspace w = GmlSerializer.createGMLWorkspace( hydrographURL, null );

        final Feature feature = w.getRootFeature();
        final INodeResultCollection nodeResultCollection = (INodeResultCollection) feature.getAdapter( INodeResultCollection.class );

        final FeatureList list = nodeResultCollection.getWrappedList();

        /* get the hydrograph locations and observations */
        final List<GM_Point> pointList = new LinkedList<GM_Point>();
        for( IHydrograph hydrograph : m_hydrographs )
        {
          GM_Object location = hydrograph.getLocation();
          if( location instanceof GM_Point )
          {
            final GM_Point point = (GM_Point) location;
            pointList.add( point );
            Feature wrappedFeature = hydrograph.getWrappedFeature();
            IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( wrappedFeature );
            TupleResult tuples = obs.getResult();
            final IComponent[] components = tuples.getComponents();

            final IComponent dateComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
            final IComponent waterlevelComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
            final IComponent depthComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DEPTH );
            final IComponent velocityComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_VEOCITY );
            final IComponent dischargeComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

            /* find the nearest node */
            Feature nearestFeature = GeometryUtilities.findNearestFeature( point, 0.0, list, GMLNodeResult.QNAME_PROP_LOCATION );
            if( nearestFeature != null )
            {
              /* get the data of that node */
              final INodeResult nodeResult = (INodeResult) nearestFeature.getAdapter( INodeResult.class );
              final double depth = nodeResult.getDepth();
              final double waterlevel = nodeResult.getWaterlevel();
              final double discharge = nodeResult.getDischarge();
              final double absoluteVelocity = nodeResult.getAbsoluteVelocity();
              final List<Double> velocity = nodeResult.getVelocity();

              /* add the data to the observation */
              final IRecord newRecord = tuples.createRecord();

              newRecord.setValue( waterlevelComp, waterlevel );
              newRecord.setValue( depthComp, depth );
              newRecord.setValue( velocityComp, velocity );
              newRecord.setValue( dischargeComp, discharge );

              tuples.add( newRecord );
            }
          }
        }
      }
      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

}
