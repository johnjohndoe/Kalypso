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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.Hydrotop;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.suds.ISuds;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Creates and writes hydrotops into a 'hydrotop.gml' file from 'modell.gml' (catchments), 'pedologie.gml',
 * 'geologie.gml' and 'landuse.gml'<br/>
 * FIXME: break this code into smaller chunks. Else, no one will every understand how it works....
 * 
 * @author Dejan Antanaskovic
 */
public class HydrotopeCreationOperation implements IRunnableWithProgress
{
  private static final String COORDINATE_SYSTEM = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private final FeatureList m_landuseList;

  private final FeatureList m_pedologyList;

  private final FeatureList m_geologyList;

  private final FeatureList m_catchmentsList;

  private final IFeatureBindingCollection<IHydrotope> m_outputList;

  private final GMLWorkspace m_workspace;

  private boolean m_dissolveFeatures = true;

  private GM_MultiSurface m_workingArea = null;

  public HydrotopeCreationOperation( final FeatureList landuseList, final FeatureList pedologyList, final FeatureList geologyList, final FeatureList catchmentsList, final IFeatureBindingCollection<IHydrotope> outputList, final GMLWorkspace outputWorkspace, final GM_MultiSurface workingArea )
  {
    m_landuseList = landuseList;
    m_pedologyList = pedologyList;
    m_geologyList = geologyList;
    m_catchmentsList = catchmentsList;
    m_outputList = outputList;
    m_workspace = outputWorkspace;
    m_workingArea = workingArea;
  }

  public final void setDissolveMode( final boolean dissolveFeatures )
  {
    m_dissolveFeatures = dissolveFeatures;
  }

  private final FeatureListGeometryIntersector getIntersector( )
  {
    final FeatureListGeometryIntersector geometryIntersector = new FeatureListGeometryIntersector();

    if( m_workingArea == null )
    {
      geometryIntersector.addFeatureList( m_pedologyList );
      geometryIntersector.addFeatureList( m_geologyList );
      geometryIntersector.addFeatureList( m_catchmentsList );
      geometryIntersector.addFeatureList( m_landuseList );
      m_outputList.clear();
    }
    else
    {
      // FIXME: this doe not work; if the planning area is present, hydrotope creation fails.
      final GM_Envelope envelope = m_workingArea.getEnvelope();

      final List pedologyQuery = m_pedologyList.query( envelope, null );
      final List geologyQuery = m_geologyList.query( envelope, null );
      final List catchmentsQuery = m_catchmentsList.query( envelope, null );
      final List landuseQuery = m_landuseList.query( envelope, null );

      geometryIntersector.addFeatureList( pedologyQuery );
      geometryIntersector.addFeatureList( geologyQuery );
      geometryIntersector.addFeatureList( catchmentsQuery );
      geometryIntersector.addFeatureList( landuseQuery );
    }
    return geometryIntersector;
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

    final FeatureListGeometryIntersector geometryIntersector = getIntersector();
    try
    {
      progress.setTaskName( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.1" ) ); //$NON-NLS-1$

      final List<Polygon> intersectionList = geometryIntersector.intersect( progress.newChild( 80 ) );

      progress.setTaskName( Messages.getString( "org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation.2" ) ); //$NON-NLS-1$
      progress.setWorkRemaining( intersectionList.size() );

      if( m_outputList.size() > 0 )
      {
        final Geometry intersectionArea = JTSAdapter.jtsFactory.createGeometryCollection( intersectionList.toArray( new Polygon[intersectionList.size()] ) ).buffer( 0.0 );
        // FIXME: check if the coordinate system is really required, it was always null in previous implementation
        // FIXME: yes, it is requiered: FIXME: putting the global one is WRONG -> put the crs of the wrapped geoemtry
        final GM_Envelope gmEnvelope = JTSAdapter.wrap( intersectionArea.getEnvelopeInternal(), COORDINATE_SYSTEM );
        final List<IHydrotope> list = m_outputList.query( gmEnvelope );
        for( final IHydrotope hydrotop : list )
        {
          final Geometry g = JTSAdapter.export( hydrotop.getGeometry() );
          if( g.disjoint( intersectionArea ) || g.touches( intersectionArea ) )
            continue;

          if( g.coveredBy( intersectionArea ) )
            m_outputList.remove( hydrotop );
          else
          {
            final Geometry geometry = g.difference( intersectionArea );
            if( geometry.isEmpty() )
            {
              continue;
            }
            if( geometry instanceof MultiPolygon )
            {
              // FIXME: Complicated case! fix it!
              continue;
            }
            else
            {
              final GM_MultiSurface hydrotopGeometry = toMultiSurface( geometry, COORDINATE_SYSTEM );
              if( hydrotopGeometry != null )
                hydrotop.setGeometry( hydrotopGeometry );
            }
          }
        }
      }

      int count = 0;

      final IGMLSchema gmlSchema = m_workspace.getGMLSchema();
      final IFeatureType hydrotopeFT = gmlSchema.getFeatureType( IHydrotope.QNAME );
      final IRelationType sudsMemberRT = (IRelationType) hydrotopeFT.getProperty( Hydrotop.QNAME_PROP_SUD_MEMBERS );
      final IRelationType catchmentMemberRT = (IRelationType) hydrotopeFT.getProperty( Hydrotop.QNAME_PROP_CATCHMENT_MEMBER );

      for( final Geometry geometry : intersectionList )
      {
        if( count % 100 == 0 )
        {
          final String msg = Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation.3", count, intersectionList.size() ); //$NON-NLS-1$
          progress.subTask( msg );
          // TODO: belongs to the end of this loop, but there are just too many else's
          // Better: put into sub-method and 'return' instead of 'continue'
          ProgressUtilities.worked( monitor, 100 );

          // FIXME
          System.out.println( msg );
        }
        count++;

        final FeatureIntersection userData = (FeatureIntersection) geometry.getUserData();
        final Feature[] features = userData.getFeatures();
        // we always expect an intersection of subcatchments, landuse, geology and pedology
        if( features.length != 4 )
          continue;

        boolean b_catchment = false;
        boolean b_landuse = false;
        boolean b_soiltype = false;
        boolean b_geology = false;

        for( final Feature feature : features )
        {
          if( feature instanceof org.kalypso.model.hydrology.binding.model.Catchment )
          {
            b_catchment = true;
          }
          else if( feature instanceof Landuse )
          {
            b_landuse = true;
          }
          else if( feature instanceof SoilType )
          {
            b_soiltype = true;
          }
          else if( feature instanceof Geology )
          {
            b_geology = true;
          }

        }

        if( !b_geology || !b_landuse || !b_catchment || !b_soiltype )
        {
          continue;
        }

        final IHydrotope hydrotop = m_outputList.addNew( IHydrotope.QNAME );

        // sealing correction factor is now the product of
        // factors from catchment and landuse
        double corrSealing = 1.0;
        for( final Feature feature : features )
        {
          if( feature instanceof org.kalypso.model.hydrology.binding.model.Catchment )
          {
            final IFeatureType featureType = feature.getFeatureType();
            final String href = String.format( "modell.gml#%s", feature.getId() ); //$NON-NLS-1$
            final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, catchmentMemberRT, featureType, href, null, null, null, null, null );
            hydrotop.setCatchmentMember( lnk );
            corrSealing = corrSealing * ((Catchment) feature).getCorrSealing();
          }
          else if( feature instanceof Landuse )
          {
            final Landuse landuse = (Landuse) feature;
            final Object landuseClassLink = landuse.getLanduse();
            final Feature featureLanduse = FeatureHelper.resolveLinkedFeature( m_workspace, landuseClassLink );
            if( featureLanduse == null )
              hydrotop.setLanduse( ((XLinkedFeature_Impl) landuseClassLink).getFeatureId() );
            else
              hydrotop.setLanduse( featureLanduse.getName() );

            corrSealing = corrSealing * landuse.getCorrSealing();

            final IFeatureBindingCollection<Feature> landuseSudsCollection = landuse.getSudCollection();
            final IFeatureBindingCollection<ISuds> hydrotopeSudsCollection = hydrotop.getSudCollection();
            final FeatureList hydrotopeFeatureList = hydrotopeSudsCollection.getFeatureList();
            for( final Feature sudsFeature : landuseSudsCollection )
            {
              // TODO check why landuse have the collection of suds, when hydrotop may be connected to just one sud
              if( sudsFeature instanceof XLinkedFeature_Impl )
              {
                final IFeatureType ft = sudsFeature.getFeatureType();
                final String href = String.format( "suds.gml#%s", ((XLinkedFeature_Impl) sudsFeature).getFeatureId() ); //$NON-NLS-1$

                final XLinkedFeature_Impl lnk = new XLinkedFeature_Impl( hydrotop, sudsMemberRT, ft, href, null, null, null, null, null );
                hydrotopeFeatureList.add( lnk );
              }
            }
          }
          else if( feature instanceof SoilType )
          {
            final Object soiltypeClassLink = ((SoilType) feature).getSoilType();
            final String value;
            if( soiltypeClassLink instanceof XLinkedFeature_Impl )
              value = ((XLinkedFeature_Impl) soiltypeClassLink).getFeatureId();
            else
              value = soiltypeClassLink.toString().substring( soiltypeClassLink.toString().indexOf( "#" ) + 1 ); //$NON-NLS-1$
            hydrotop.setSoilType( value );
          }
          else if( feature instanceof Geology )
          {
            final Geology geology = (Geology) feature;
            hydrotop.setMaxPerkolationRate( geology.getMaxPerkulationsRate() );
            hydrotop.setGWFactor( geology.getGWFactor() );
          }
        }
        hydrotop.setCorrSealing( corrSealing );

        if( m_dissolveFeatures )
        {
          final GM_Envelope featureGeometryEnvelope = JTSAdapter.wrap( geometry.getEnvelopeInternal(), COORDINATE_SYSTEM );
          // FIXME: this is most probably a heavy performance bug: removing/adding feature to m_output list invalidates
          // the geo-index
          // The #query however rebuilds it completly -> so in every loop the geo-index is rebuilt
          // TODO: the dissolve should happen in another extra operation, mixing it with the hydrotope-creation is bad

          final List<IHydrotope> list = m_outputList.query( featureGeometryEnvelope );
          final List<Feature> featuresToMergeWith = new ArrayList<Feature>();
          final List<Geometry> geometriesToMergeWith = new ArrayList<Geometry>();
          for( final Feature f : list )
          {
            if( f == hydrotop )
              continue;

            final Geometry g = JTSAdapter.export( f.getDefaultGeometryPropertyValue() );
            if( g == null )
            {
              System.out.println( "Broken Feature:" + f + " MORE:" + f.getDefaultGeometryPropertyValue() );
              m_outputList.remove( f );
              continue;
            }
            else if( geometry.intersects( g ) && hydrotop.isEqualByPropertiesWith( f ) )
            {
              featuresToMergeWith.add( f );
              geometriesToMergeWith.add( g );
            }
          }
          if( !featuresToMergeWith.isEmpty() )
          {
            geometriesToMergeWith.add( geometry );

            Geometry union = null;

            final GeometryCollection geometryCollection = new GeometryFactory().createGeometryCollection( GeometryFactory.toGeometryArray( geometriesToMergeWith ) );
            try
            {
              union = geometryCollection.union();
              if( union.getNumGeometries() > 1 )
                union = union.buffer( 0.0001 ); // fixes problem with precision
              m_outputList.removeAll( featuresToMergeWith );
              m_outputList.removeAll( featuresToMergeWith ); // fixes problem, what is going on here?
            }
            catch( final Exception e )
            {
              // FIXME: why all this strange exception handling? At least: comment this!
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, e.getLocalizedMessage() );
              try
              {
                union = geometryCollection.buffer( 0.0 );
                m_outputList.removeAll( featuresToMergeWith );
              }
              catch( final Exception e2 )
              {
                try
                {
                  union = geometry;
                  for( int i = 0; i < geometriesToMergeWith.size() - 1; i++ )
                    union = union.union( geometriesToMergeWith.get( i ) );
                  m_outputList.removeAll( featuresToMergeWith );
                }
                catch( final Exception e3 )
                {
                  e.printStackTrace();
                  continue;
                }
              }
            }
            final GM_MultiSurface hydrotopGeometry = toMultiSurface( union, COORDINATE_SYSTEM );
            hydrotop.setGeometry( hydrotopGeometry );
          }
          else
          {
            final GM_MultiSurface lGeometry = toMultiSurface( geometry, COORDINATE_SYSTEM );
            hydrotop.setGeometry( lGeometry );
          }
        }
        else
        {
          final GM_MultiSurface lGeometry = toMultiSurface( geometry, COORDINATE_SYSTEM );
          hydrotop.setGeometry( lGeometry );
        }
        hydrotop.setName( hydrotop.getId() );
      }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }

  private GM_MultiSurface toMultiSurface( final Geometry geometry, final String crs )
  {
    try
    {
      final GM_Object newGeometry = JTSAdapter.wrap( geometry, crs );
      if( newGeometry instanceof GM_Surface )
      {
        final ArrayList<GM_Surface< ? >> arrayList = new ArrayList<GM_Surface< ? >>();
        arrayList.add( (GM_Surface< ? >) newGeometry );
        final GM_MultiSurface multiSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_MultiSurface( arrayList.toArray( new GM_Surface[0] ), newGeometry.getCoordinateSystem() );
        return multiSurface;
      }
      else
      {
        throw new IllegalStateException( "cannot create hydrotope with multisurface" );
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
