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
package org.kalypso.model.wspm.sobek.core.wizard.worker;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author kuch
 */
public class FinishWorkerEditBoundaryCondition implements ICoreRunnableWithProgress
{
  public static final String OBS_DATE = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#DATE";

  public static final String OBS_W = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#W";

  public static final String OBS_Q = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#Q";

  private final ILastfall m_lastfall;

  private final IBoundaryNode m_node;

  private final PageEditBoundaryConditionGeneral m_general;

  private final PageEditBoundaryConditionTimeSeries m_timeSeries;

  public FinishWorkerEditBoundaryCondition( final ILastfall lastfall, final IBoundaryNode node, final PageEditBoundaryConditionGeneral general, final PageEditBoundaryConditionTimeSeries timeSeries )
  {
    m_lastfall = lastfall;
    m_node = node;
    m_general = general;
    m_timeSeries = timeSeries;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    try
    {
      // TODO differ between constant and timeseries
      final IBoundaryNodeLastfallCondition condition = m_node.getLastfallCondition( m_lastfall );

      final Map<QName, Object> changes = new HashMap<QName, Object>();

      /* link to ts repository */
      final TimeseriesLinkType lnkTimeSeries = new TimeseriesLinkType();
      final ZmlObservationItem item = m_timeSeries.getZmlObservationItem();
      final String path = getRelativePath( item.getFile().getPath() );
      lnkTimeSeries.setHref( path );

      changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES, lnkTimeSeries );

      /* start date */
      final GregorianCalendar grStart = m_general.getStartDate();
      changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS, new XMLGregorianCalendarImpl( grStart ) );

      /* end date */
      final GregorianCalendar grEnd = m_general.getEndDate();
      changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS, new XMLGregorianCalendarImpl( grEnd ) );

      FeatureUtils.updateFeature( condition.getFeature(), changes );

      generateTimeSeriesObs( condition );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return null;
  }

  private void generateTimeSeriesObs( final IBoundaryNodeLastfallCondition condition )
  {
    try
    {
      final Feature feature = condition.getFeature();
      final CommandableWorkspace workspace = FeatureUtils.getWorkspace( feature );

      final IPropertyType property = feature.getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION );
      final IRelationType relation = (IRelationType) property;

      final Feature fObs = workspace.createFeature( feature, relation, relation.getTargetFeatureType() );
      workspace.setFeatureAsComposition( feature, relation, fObs, true );

      /* new resultset */
      final TupleResult result = new TupleResult();

      /* add components to resultset */
      result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, FinishWorkerEditBoundaryCondition.OBS_DATE ) );
      result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, FinishWorkerEditBoundaryCondition.OBS_W ) );
      result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, FinishWorkerEditBoundaryCondition.OBS_Q ) );

      /* fill TupleResult with data */
      // writeCombinationsToObservation( result, m_baseMap );
      /* add observation to workspace */
      final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result, new ArrayList<MetadataObject>() );

      // maybe set phenomenon?
      ObservationFeatureFactory.toFeature( obs, fObs );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /* nofdp relative link to time series repository */
  private String getRelativePath( final String path )
  {
    final String lowerCase = path.toLowerCase();
    final int index = lowerCase.indexOf( "timeseriesrepository" );

    return "project:/" + path.substring( index );
  }

}
