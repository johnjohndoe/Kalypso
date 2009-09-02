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
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.convert.namodel.FeatureListGeometryIntersector;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.schema.binding.Geology;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.SoilType;
import org.kalypso.convert.namodel.schema.binding.Hydrotop.HYDROTOP_TYPE;
import org.kalypso.convert.namodel.schema.binding.suds.Greenroof;
import org.kalypso.convert.namodel.schema.binding.suds.Swale;
import org.kalypso.convert.namodel.schema.binding.suds.SwaleInfiltrationDitch;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;

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

  private final GMLWorkspace m_workspace;

  private final IFeatureType m_featureType;

  private boolean m_dissolveFeatures = true;

  public HydrotopeCreationOperation( final FeatureList landuseList, final FeatureList pedologyList, final FeatureList geologyList, final FeatureList catchmentsList, final FeatureList outputList, final GMLWorkspace outputWorkspace )
  {
    m_landuseList = landuseList;
    m_pedologyList = pedologyList;
    m_geologyList = geologyList;
    m_catchmentsList = catchmentsList;
    m_outputList = outputList;
    m_workspace = outputWorkspace;
    m_featureType = m_workspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT );
  }

  public final void setDissolveMode( final boolean dissolveFeatures )
  {
    m_dissolveFeatures = dissolveFeatures;
  }

  /**
   * TODO implements/extends ICoreRunnalbeWithProgress or WorkspaceJob -> run method return status!!!!
   * 
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.0" ), 100 ); //$NON-NLS-1$

    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector();
    geometryIntersector.addFeatureList( m_catchmentsList );
    geometryIntersector.addFeatureList( m_pedologyList );
    geometryIntersector.addFeatureList( m_geologyList );
    geometryIntersector.addFeatureList( m_landuseList );
    final List<MultiPolygon> intersectionList;
    try
    {
      progress.setTaskName( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.1" ) ); //$NON-NLS-1$
      intersectionList = geometryIntersector.intersect( progress.newChild( 50 ) );

      progress.setTaskName( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.2" ) ); //$NON-NLS-1$
      progress.setWorkRemaining( intersectionList.size() );

      m_outputList.clear();
      int count = 0;

      final IRelationType sudsMemberRT = (IRelationType) m_workspace.getGMLSchema().getFeatureType( Hydrotop.QNAME ).getProperty( Hydrotop.QNAME_PROP_SUD_MEMBERS );
      final IRelationType catchmentMemberRT = (IRelationType) m_workspace.getGMLSchema().getFeatureType( Hydrotop.QNAME ).getProperty( Hydrotop.QNAME_PROP_CATCHMENT_MEMBER );

      for( final Geometry geometry : intersectionList )
      {
        if( count % 100 == 0 )
          progress.subTask( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.3" , count, intersectionList.size() ) ); //$NON-NLS-1$
        count++;

        // TODO: belongs to the end of this loop, but there are just too many else's
        // Better: put into sub-method and 'return' instead of 'continue'
        ProgressUtilities.worked( monitor, 1 );

        if( geometry.getArea() == 0.0 )
          continue;

        final Hydrotop hydrotop = (Hydrotop) m_workspace.createFeature( null, null, m_featureType );
        final Point interiorPoint = geometry.getInteriorPoint();
        final GM_Envelope envelope = JTSAdapter.wrap( interiorPoint.getEnvelopeInternal() );
        final GM_Point point = (GM_Point) JTSAdapter.wrap( interiorPoint );

        final List<Object> catchmentList = m_catchmentsList.query( envelope, null );
        if( catchmentList.size() == 0 )
          continue;
        else
        {
          boolean catchmentFound = false;
          for( final Object object : catchmentList )
          {
            final Feature catchment = (Feature) object;
            if( catchment.getDefaultGeometryPropertyValue().contains( point ) )
            {
              catchmentFound = true;
              final IFeatureType featureType = catchment.getFeatureType();
              final String href = String.format( "project:/modell.gml#%s", catchment.getId() ); //$NON-NLS-1$
              final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, catchmentMemberRT, featureType, href, null, null, null, null, null );
              hydrotop.setCatchmentMember( lnk );
              break;
            }
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
          final Feature featureLanduse = FeatureHelper.resolveLinkedFeature( m_workspace, landuseClassLink );
          if( featureLanduse == null )
            hydrotop.setLanduse( ((XLinkedFeature_Impl) landuseClassLink).getFeatureId() );
          else
            hydrotop.setLanduse( featureLanduse.getName() );
          hydrotop.setDrainageType( landuse.getDrainageType() );
          hydrotop.setCorrSealing( landuse.getCorrSealing() );

          final IFeatureBindingCollection<Feature> landuseSudsCollection = landuse.getSudCollection();
          final IFeatureBindingCollection<Feature> hydrotopeSudsCollection = hydrotop.getSudCollection();
          for( final Feature feature : landuseSudsCollection )
          {
            // TODO check why landuse have the collection of suds, when hydrotop may be connected to just one sud
            if( feature instanceof XLinkedFeature_Impl )
            {
              final IFeatureType ft = feature.getFeatureType();
              final String href = String.format( "project:/suds.gml#%s", ((XLinkedFeature_Impl) feature).getFeatureId() ); //$NON-NLS-1$

              final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, sudsMemberRT, ft, href, null, null, null, null, null );
              hydrotopeSudsCollection.add( lnk );

              final Feature sudFeature = ((XLinkedFeature_Impl) feature).getFeature();
              if( sudFeature instanceof Swale || sudFeature instanceof SwaleInfiltrationDitch )
                hydrotop.setHydrotopType( HYDROTOP_TYPE.MULDEN_RIGOLE );
              else if( sudFeature instanceof Greenroof )
                hydrotop.setHydrotopType( HYDROTOP_TYPE.DACHBEGRUENUNG );
              else
                hydrotop.setHydrotopType( HYDROTOP_TYPE.BODENSPEICHER );
            }
            else
              hydrotop.setHydrotopType( HYDROTOP_TYPE.BODENSPEICHER );
          }
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
          final String value; //$NON-NLS-1$
          if( soiltypeClassLink instanceof XLinkedFeature_Impl )
            value = ((XLinkedFeature_Impl) soiltypeClassLink).getFeatureId();
          else
            value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
          hydrotop.setSoilType( value );
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
          hydrotop.setMaxPerkolationRate( geology.getMaxPerkulationsRate() );
          hydrotop.setGWFactor( geology.getGWFactor() );
        }
        else
          continue;

        if( m_dissolveFeatures )
        {
          final GM_Envelope featureGeometryEnvelope = JTSAdapter.wrap( geometry.getEnvelopeInternal() );
          final List<Feature> list = m_outputList.query( featureGeometryEnvelope, null );
          final List<Feature> featuresToMergeWith = new ArrayList<Feature>();
          final List<Geometry> geometriesToMergeWith = new ArrayList<Geometry>();
          for( final Feature f : list )
          {
            final Geometry g = JTSAdapter.export( f.getDefaultGeometryPropertyValue() );
            if( geometry.intersects( g ) )
              if( hydrotop.isEqualByPropertiesWith( f ) )
              {
                featuresToMergeWith.add( f );
                geometriesToMergeWith.add( g );
              }
          }
          if( !featuresToMergeWith.isEmpty() )
          {
            m_outputList.removeAll( featuresToMergeWith );
            geometriesToMergeWith.add( geometry );

            Geometry union = null;
            try
            {
              union = new GeometryFactory().createGeometryCollection( GeometryFactory.toGeometryArray( geometriesToMergeWith ) ).buffer( 0.0 );
            }
            catch( final Exception e )
            {
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, e.getLocalizedMessage() );
              union = geometry;
              for( int i = 0; i < geometriesToMergeWith.size() - 1; i++ )
                union = union.union( geometriesToMergeWith.get( i ) );
            }

            final GM_Object newGeometry = JTSAdapter.wrap( union );
            if( newGeometry instanceof GM_MultiSurface )
              hydrotop.setGeometry( (GM_MultiSurface) newGeometry );
            else if( newGeometry instanceof GM_Surface )
            {
              final ArrayList<GM_Surface> arrayList = new ArrayList<GM_Surface>();
              arrayList.add( (GM_Surface) newGeometry );
              final GM_MultiSurface multiSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_MultiSurface( arrayList.toArray( new GM_Surface[0] ), newGeometry.getCoordinateSystem() );
              hydrotop.setGeometry( multiSurface );
            }
            hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, union.getArea() );
          }
          else
          {
            hydrotop.setGeometry( (GM_MultiSurface) JTSAdapter.wrap( geometry ) );
            hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, geometry.getArea() );
          }
        }
        else
        {
          hydrotop.setGeometry( (GM_MultiSurface) JTSAdapter.wrap( geometry ) );
          hydrotop.setProperty( NaModelConstants.HYDRO_PROP_AREA, geometry.getArea() );
        }
        hydrotop.setName( hydrotop.getId() );
        m_outputList.add( hydrotop );
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
