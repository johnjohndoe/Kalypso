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
package org.kalypso.convert.namodel.hydrotope;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.convert.namodel.FeatureListGeometryIntersector;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.schema.binding.Geology;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.SoilType;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * Creates and writes hydrotops into a 'hydrotop.gml' file from 'modell.gml' (catchments), 'pedologie.gml',
 * 'geologie.gml' and 'landuse.gml'
 *
 * @author Dejan Antanaskovic
 */
public class HydrotopeCreationOperation implements IRunnableWithProgress
{
  private final FeatureList m_landuseList;

  private final FeatureList m_pedologyList;

  private final FeatureList m_geologyList;

  private final FeatureList m_catchmentsList;

  private final FeatureList m_outputList;

  private final GMLWorkspace m_outputWorkspace;

  private final IFeatureType m_outputFeatureType;

  public HydrotopeCreationOperation( final FeatureList landuseList, final FeatureList pedologyList, final FeatureList geologyList, final FeatureList catchmentsList, final FeatureList outputList, final GMLWorkspace outputWorkspace, final IFeatureType outputFeatureType )
  {
    m_landuseList = landuseList;
    m_pedologyList = pedologyList;
    m_geologyList = geologyList;
    m_catchmentsList = catchmentsList;
    m_outputList = outputList;
    m_outputWorkspace = outputWorkspace;
    m_outputFeatureType = outputFeatureType;
  }

  /**
   * TODO implements/extends ICoreRunnalbeWithProgress or WorkspaceJob -> run method return status!!!!
   * 
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.0"), 100 ); //$NON-NLS-1$

    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector();
    geometryIntersector.addFeatureList( m_catchmentsList );
    geometryIntersector.addFeatureList( m_pedologyList );
    geometryIntersector.addFeatureList( m_geologyList );
    geometryIntersector.addFeatureList( m_landuseList );
    List<MultiPolygon> intersectionList;
    try
    {
      progress.setTaskName( Messages.getString("org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.1") ); //$NON-NLS-1$
      intersectionList = geometryIntersector.intersect( progress.newChild( 50 ) );

      progress.setTaskName( Messages.getString("org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.2") ); //$NON-NLS-1$
      progress.setWorkRemaining( intersectionList.size() );

      m_outputList.clear();
      int count = 0;
      for( final Geometry geometry : intersectionList )
      {
        if( count % 100 == 0 )
          progress.subTask( String.format( Messages.getString("org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.3"), count, intersectionList.size() ) ); //$NON-NLS-1$
        count++;

        // TODO: belongs to the end of this loop, but there are just too many else's
        // Better: put into sub-method and 'return' instead of 'continue'
        ProgressUtilities.worked( monitor, 1 );

        final Feature feature = m_outputWorkspace.createFeature( null, null, m_outputFeatureType );
        final GM_Envelope envelope = JTSAdapter.wrap( geometry.getInteriorPoint().getEnvelopeInternal() );
        final GM_Point point = (GM_Point) JTSAdapter.wrap( geometry.getInteriorPoint() );

        final List<Object> catchmentList = m_catchmentsList.query( envelope, null );
        if( catchmentList.size() == 0 )
          continue;
        else
        {
          boolean catchmentFound = false;
          for( final Object object : catchmentList )
            if( ((Feature) object).getDefaultGeometryPropertyValue().contains( point ) )
            {
              catchmentFound = true;
              break;
            }
          if( !catchmentFound )
            continue;
        }

        final List<Landuse> landuseList = m_landuseList.query( envelope, null );
        if( landuseList.size() > 0 )
        {
          Landuse landuse = null;
          for( final Landuse l : landuseList )
            if( l.getDefaultGeometryPropertyValue().contains( point ) )
            {
              landuse = l;
              break;
            }
          if( landuse == null )
            continue;
          final Object landuseClassLink = landuse.getLanduse();
          String value = ""; //$NON-NLS-1$
          if( landuseClassLink instanceof XLinkedFeature_Impl )
            value = ((XLinkedFeature_Impl) landuseClassLink).getFeatureId();
          else
            value = landuseClassLink.toString().substring( landuseClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
          feature.setProperty( NaModelConstants.HYDRO_PROP_LANDUSE_NAME, value );
          feature.setProperty( NaModelConstants.HYDRO_PROP_DAINAGETYPE, landuse.getDrainageType() );
          feature.setProperty( NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR, landuse.getCorrSealing() );
        }
        else
          continue;

        final List<SoilType> soilTypesList = m_pedologyList.query( envelope, null );
        if( soilTypesList.size() > 0 )
        {
          SoilType soilType = null;
          for( final SoilType s : soilTypesList )
            if( s.getDefaultGeometryPropertyValue().contains( point ) )
            {
              soilType = s;
              break;
            }
          if( soilType == null )
            continue;
          final Object soiltypeClassLink = soilType.getSoilType();
          String value = ""; //$NON-NLS-1$
          if( soiltypeClassLink instanceof XLinkedFeature_Impl )
            value = ((XLinkedFeature_Impl) soiltypeClassLink).getFeatureId();
          else
            value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
          feature.setProperty( NaModelConstants.HYDRO_PROP_SOILTYPE, value );
        }
        else
          continue;

        final List<Geology> geologyList = m_geologyList.query( envelope, null );
        if( geologyList.size() > 0 )
        {
          Geology geology = null;
          for( final Geology g : geologyList )
            if( g.getDefaultGeometryPropertyValue().contains( point ) )
            {
              geology = g;
              break;
            }
          if( geology == null )
            continue;
          feature.setProperty( NaModelConstants.HYDRO_PROP_MAXPERCOLATIONSRATE, geology.getMaxPerkulationsRate() );
          feature.setProperty( NaModelConstants.HYDRO_PROP_INFLOWRATEGW, geology.getGWFactor() );
        }
        else
          continue;

        feature.setProperty( NaModelConstants.HYDRO_PROP_GEOM, JTSAdapter.wrap( geometry ) );
        feature.setProperty( NaModelConstants.HYDRO_PROP_AREA, geometry.getArea() );

        m_outputList.add( feature );
      }
    }
    catch( final GM_Exception e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final CoreException e )
    {
      throw new InvocationTargetException( e );
    }

  }

}
